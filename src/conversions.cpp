#include "conversions.h"

#include <R_ext/Error.h>
#include <iostream>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP stack2SEXP(MagickStack& stack, bool rgb) {
    int nimages = stack.size();
    if (nimages < 1) {
        if (verbose)
            warning("image stack empty: returning NULL");
        return R_NilValue;
    }
    int nProt = 0;
    SEXP rimage = R_NilValue;
    try {
        /* get parameters of the first image to set 'data' size */
        MagickImage image = *stack.begin();
        unsigned int dx = image.columns();
        unsigned int dy = image.rows();
        if (dx * dy <= 0) {
            if (verbose)
                warning("first image in the stack is of size 0: returning NULL");
            return R_NilValue;
        }
        if (rgb)
            PROTECT(rimage = allocVector(INTSXP, dx * dy * nimages));
        else
            PROTECT(rimage = allocVector(REALSXP, dx * dy * nimages));
        nProt++;
        int i = 0;
        void * dest;
        for (MagickStack::iterator it = stack.begin(); it != stack.end(); it++) {
            image = *it;
            int idx = (image.columns() < dx)?image.columns():dx;
            int idy = (image.rows() < dy)?image.rows():dy;
            if (rgb)
                dest = &(INTEGER(rimage)[i * dx * dy]);
            else
                dest = &(REAL(rimage)[i * dx * dy]);
            switch (rgb) {
                case true: {
                    image.type(TrueColorType);
                    /* IMPORTANT: the next line sets the highest byte in RGBO to 0 thus permitting
                    int operations instead of unsigned int (R doesn't support unsigned int)
                    It's a must here for correct color conversions in R later */
                    image.opacity(OpaqueOpacity);
                    image.write(0, 0, idx, idy, "RGBO", CharPixel, dest);
                }; break;
                default: {
                    image.type(GrayscaleType);
                    image.opacity(OpaqueOpacity);
                    image.write(0, 0, idx, idy, "I", DoublePixel, dest);
                }
            }
            i++;
        }
        SEXP dim;
        if (nimages > 1)
            PROTECT(dim = allocVector(INTSXP, 3));
        else
            PROTECT(dim = allocVector(INTSXP, 2));
        nProt++;
        INTEGER(dim)[0] = dx;
        INTEGER(dim)[1] = dy;
        if (nimages > 1)
            INTEGER(dim)[2] = nimages;
        SET_DIM(rimage, dim);
        if (nimages > 1)
            SET_CLASS(rimage, mkString("Image3D"));
        else
            SET_CLASS(rimage, mkString("Image2D"));
        SEXP isrgb;
        PROTECT(isrgb = allocVector(LGLSXP, 1));
        nProt++;
        LOGICAL(isrgb)[0] = rgb;
        SET_SLOT(rimage, mkString("rgb"), isrgb);
// TODO MUST CREATE ALL SLOTS HERE OTHERWISE THEY WILL BE MISSING
        if (nProt > 0) {
            int nUnprotect = nProt;
            nProt = 0;
            UNPROTECT(nUnprotect);
        }
    }
    catch(...) {
        rimage = R_NilValue;
        if (nProt > 0)
            UNPROTECT(nProt);
        if (verbose)
            warning("problems loading stack (crash): returning NULL");
    }
    return rimage;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
MagickStack SEXP2Stack(SEXP rimage) {
    MagickStack stack;
    try {
        if (strcmp(CHAR(asChar(GET_CLASS(rimage))), "Image3D") != 0)
            error("'image' argument is expected to be of class 'Image3D'");
        SEXP dim = GET_DIM(rimage);
        int ndim = LENGTH(dim);
        if (ndim != 3)
            error("only 3D images can be converted to stacks. Select a subset and try again.");
        int nimages = 1;
        if (ndim > 2)
            nimages = INTEGER(dim)[2];
        int dx = INTEGER(dim)[0];
        int dy = INTEGER(dim)[1];
        bool rgb = LOGICAL(GET_SLOT(rimage, mkString("rgb")))[0];
        Geometry geom(dx, dy);
        MagickImage image(geom, "black");
        void * src;
        for (int i = 0; i < nimages; i++) {
            if (rgb)
                src = &(INTEGER(rimage)[i * dx * dy]);
            else
                src = &(REAL(rimage)[i * dx * dy]);
            switch(rgb) {
                case true: {
                    image.type(TrueColorType);
                    image.read(dx, dy, "RGBO", CharPixel, src);
                    image.opacity(OpaqueOpacity);
                }; break;
                default: {
                    image.type(GrayscaleType);
                    image.read(dx, dy, "I", DoublePixel, src);
                    image.opacity(OpaqueOpacity);
                }
            }
            stack.push_back(image);
        }
    }
    catch(...) {
        error("unidentified problems during image converion in 'SEXP2Stack' c++ routine");
    }
    return stack;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
MagickImage  SEXP2Image(SEXP rimage) {
    try {
        if (strcmp(CHAR(asChar(GET_CLASS(rimage))), "Image2D") != 0)
            error("'image' argument is expected to be of class 'Image2D'");
        SEXP dim = GET_DIM(rimage);
        int ndim = LENGTH(dim);
        if (ndim != 2)
            error("only 2D R-images can be converted to ImageMagick images");
        int dx = INTEGER(dim)[0];
        int dy = INTEGER(dim)[1];
        bool rgb = LOGICAL(GET_SLOT(rimage, mkString("rgb")))[0];
        Geometry geom(dx, dy);
        MagickImage image(geom, "black");
        void * src;
        if (rgb)
            src = &(INTEGER(rimage)[0]);
        else
            src = &(REAL(rimage)[0]);
        switch(rgb) {
            case true: {
                image.type(TrueColorType);
                image.read(dx, dy, "RGBO", CharPixel, src);
                image.opacity(OpaqueOpacity);
            }; break;
            default: {
                image.type(GrayscaleType);
                image.read(dx, dy, "I", DoublePixel, src);
                image.opacity(OpaqueOpacity);
            }
        }
        return image;
    }
    catch(...) {
        error("unidentified problems during image converion in 'SEXP2Image' c++ routine");
    }
    /* this should never happen, but it prevents warning: the function exits either on
       previous return or on error - it should not happen that the control comes here
    */
    return MagickImage(Geometry(10, 10), "black");
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
MagickImage pullImageData(SEXP rimage, int index) {
    try {
        SEXP dim = GET_DIM(rimage);
        int ndim = LENGTH(dim);
        int nimage = 1;
        int dx = INTEGER(dim)[0];
        int dy = INTEGER(dim)[1];
        if (ndim > 2)
            nimage = INTEGER(dim)[2];
        bool rgb = LOGICAL(GET_SLOT(rimage, mkString("rgb")))[0];
        Geometry geom(dx, dy);
        MagickImage image(geom, "black");
        void * src;
        int i = index;
        if (i >= nimage)
            i = nimage - 1;
        if (rgb)
            src = &(INTEGER(rimage)[i * dx * dy]);
        else
            src = &(REAL(rimage)[i * dx * dy]);
        switch(rgb) {
            case true: {
                image.type(TrueColorType);
                image.read(dx, dy, "RGBO", CharPixel, src);
                image.opacity(OpaqueOpacity);
            }; break;
            default: {
                image.type(GrayscaleType);
                image.read(dx, dy, "I", DoublePixel, src);
                image.opacity(OpaqueOpacity);
            }
        }
        return image;
    }
    catch(...) {
        error("unidentified problems during image converion in 'pullImageData' c++ routine");
    }
    /* this should never happen, but it prevents warning: the function exits either on
       previous return or on error - it should not happen that the control comes here
    */
    return MagickImage(Geometry(10, 10), "black");
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
void pushImageData(MagickImage& image, SEXP rimage, int index) {
    try {
        SEXP dim = GET_DIM(rimage);
        int ndim = LENGTH(dim);
        int nimage = 1;
        int dx = INTEGER(dim)[0];
        int dy = INTEGER(dim)[1];
        if (ndim > 2)
            nimage = INTEGER(dim)[2];
        bool rgb = LOGICAL(GET_SLOT(rimage, mkString("rgb")))[0];
        void * dest;
        int i = index;
        if (i >= nimage)
            i = nimage - 1;
        if (rgb)
            dest = &(INTEGER(rimage)[i * dx * dy]);
        else
            dest = &(REAL(rimage)[i * dx * dy]);
        switch(rgb) {
            case true: {
                image.type(TrueColorType);
                image.opacity(OpaqueOpacity);
                image.write(0, 0, dx, dy, "RGBO", CharPixel, dest);
            }; break;
            default: {
                image.type(GrayscaleType);
                image.opacity(OpaqueOpacity);
                image.write(0, 0, dx, dy, "I", DoublePixel, dest);
            }
        }
    }
    catch(...) {
        error("unidentified problems during image converion in 'pullImageData' c++ routine");
    }
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP toGray(SEXP rgb) {
    int nval = LENGTH(rgb);
    try {
        Geometry geom(nval, 1);
        MagickImage image(geom, "black");
        void * src = &(INTEGER(rgb)[0]);
        image.read(nval, 1, "RGBp", CharPixel, src);
        SEXP res;
        PROTECT(res = allocVector(REALSXP, nval));
        void * dest = &(REAL(res)[0]);
        image.opacity(OpaqueOpacity);
        image.type(GrayscaleType);
        image.write(0, 0, nval, 1, "I", DoublePixel, dest);
        UNPROTECT(1);
        return res;
    }
    catch(...) {
        error("memory allocation problems in 'toGray' c++ routine");
    }
    return R_NilValue;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP toRGB(SEXP gray) {
    int nval = LENGTH(gray);
    try {
        Geometry geom(nval, 1);
        MagickImage image(geom, "black");
        void * src = &(REAL(gray)[0]);
        image.read(nval, 1, "I", DoublePixel, src);
        SEXP res;
        PROTECT(res = allocVector(INTSXP, nval));
        void * dest = &(INTEGER(res)[0]);
        image.opacity(OpaqueOpacity);
        image.type(TrueColorType);
        image.write(0, 0, nval, 1, "RGBO", CharPixel, dest);
        UNPROTECT(1);
        return res;
    }
    catch(...) {
        error("memory allocation problems in 'toRGB' c++ routine");
    }
    return R_NilValue;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP getRed(SEXP rgb) {
    int nval = LENGTH(rgb);
    try {
        Geometry geom(nval, 1);
        MagickImage image(geom, "black");
        void * src = &(INTEGER(rgb)[0]);
        image.read(nval, 1, "RGBp", CharPixel, src);
        SEXP res;
        PROTECT(res = allocVector(INTSXP, nval));
        void * dest = &(REAL(res)[0]);
        image.opacity(OpaqueOpacity);
        //image.type(GrayscaleType);
        image.write(0, 0, nval, 1, "R", DoublePixel, dest);
        UNPROTECT(1);
        return res;
    }
    catch(...) {
        error("memory allocation problems in 'getRed' c++ routine");
    }
    return R_NilValue;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP getGreen(SEXP rgb) {
    int nval = LENGTH(rgb);
    try {
        Geometry geom(nval, 1);
        MagickImage image(geom, "black");
        void * src = &(INTEGER(rgb)[0]);
        image.read(nval, 1, "RGBp", CharPixel, src);
        SEXP res;
        PROTECT(res = allocVector(INTSXP, nval));
        void * dest = &(REAL(res)[0]);
        image.opacity(OpaqueOpacity);
        //image.type(GrayscaleType);
        image.write(0, 0, nval, 1, "G", DoublePixel, dest);
        UNPROTECT(1);
        return res;
    }
    catch(...) {
        error("memory allocation problems in 'getGreen' c++ routine");
    }
    return R_NilValue;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP getBlue(SEXP rgb) {
    int nval = LENGTH(rgb);
    try {
        Geometry geom(nval, 1);
        MagickImage image(geom, "black");
        void * src = &(INTEGER(rgb)[0]);
        image.read(nval, 1, "RGBp", CharPixel, src);
        SEXP res;
        PROTECT(res = allocVector(INTSXP, nval));
        void * dest = &(REAL(res)[0]);
        image.opacity(OpaqueOpacity);
        //image.type(GrayscaleType);
        image.write(0, 0, nval, 1, "B", DoublePixel, dest);
        UNPROTECT(1);
        return res;
    }
    catch(...) {
        error("memory allocation problems in 'getBlue' c++ routine");
    }
    return R_NilValue;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP asRed(SEXP gray) {
    int nval = LENGTH(gray);
    try {
        Geometry geom(nval, 1);
        MagickImage image(geom, "black");
        void * src = &(REAL(gray)[0]);
        image.read(nval, 1, "I", DoublePixel, src);
        SEXP res;
        PROTECT(res = allocVector(INTSXP, nval));
        void * dest = &(INTEGER(res)[0]);
        image.opacity(OpaqueOpacity);
        image.type(TrueColorType);
        image.write(0, 0, nval, 1, "ROOO", CharPixel, dest);
        UNPROTECT(1);
        return res;
    }
    catch(...) {
        error("memory allocation problems in 'asRed' c++ routine");
    }
    return R_NilValue;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP asGreen(SEXP gray) {
    int nval = LENGTH(gray);
    try {
        Geometry geom(nval, 1);
        MagickImage image(geom, "black");
        void * src = &(REAL(gray)[0]);
        image.read(nval, 1, "I", DoublePixel, src);
        SEXP res;
        PROTECT(res = allocVector(INTSXP, nval));
        void * dest = &(INTEGER(res)[0]);
        image.opacity(OpaqueOpacity);
        image.type(TrueColorType);
        image.write(0, 0, nval, 1, "OGOO", CharPixel, dest);
        UNPROTECT(1);
        return res;
    }
    catch(...) {
        error("memory allocation problems in 'asGreen' c++ routine");
    }
    return R_NilValue;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP asBlue(SEXP gray) {
    int nval = LENGTH(gray);
    try {
        Geometry geom(nval, 1);
        MagickImage image(geom, "black");
        void * src = &(REAL(gray)[0]);
        image.read(nval, 1, "I", DoublePixel, src);
        SEXP res;
        PROTECT(res = allocVector(INTSXP, nval));
        void * dest = &(INTEGER(res)[0]);
        image.opacity(OpaqueOpacity);
        image.type(TrueColorType);
        image.write(0, 0, nval, 1, "OOBO", CharPixel, dest);
        UNPROTECT(1);
        return res;
    }
    catch(...) {
        error("memory allocation problems in 'asBlue' c++ routine");
    }
    return R_NilValue;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
