#include "conversions.h"

#include <R_ext/Error.h>
#include <iostream>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP stack2SEXP(MagickStack& stack, bool rgb) {
    int nimages = stack.size();
    if (nimages < 1) {
        if (verbose)
            warning("Stack empty, returning NULL");
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
                warning("First image in stack is of size 0, returning NULL");
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
        PROTECT(dim = allocVector(INTSXP, 3));
        nProt++;
        INTEGER(dim)[0] = dx;
        INTEGER(dim)[1] = dy;
        INTEGER(dim)[2] = nimages;
        SET_DIM(rimage, dim);
        SET_CLASS(rimage, mkString("Image"));
        SEXP isrgb;
        PROTECT(isrgb = allocVector(LGLSXP, 1));
        nProt++;
        LOGICAL(isrgb)[0] = rgb;
        SET_SLOT(rimage, mkString("rgb"), isrgb);
/* TODO MUST CREATE ALL SLOTS HERE OTHERWISE THEY WILL BE MISSING: no slots defined at the moment */
        if (nProt > 0) {
            int nUnprotect = nProt;
            nProt = 0;
            UNPROTECT(nUnprotect);
        }
    }
    catch(exception &error_) {
        rimage = R_NilValue;
        if (nProt > 0)
            UNPROTECT(nProt);
        if (verbose) {
            warning(error_.what());
            warning("Returning NULL");
        }
    }
    return rimage;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
MagickStack SEXP2Stack(SEXP rimage) {
    MagickStack stack;
    try {
        if (!assertImage(rimage))
            error("Wrong argument class, class Image expected");
        SEXP dim = GET_DIM(rimage);
        int dx = INTEGER(dim)[0];
        int dy = INTEGER(dim)[1];
        int nimages = INTEGER(dim)[2];
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
    catch(exception &error_) {
        error(error_.what());
    }
    return stack;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
MagickImage  SEXP2Image(SEXP rimage) {
    try {
        if (!assertImage2D(rimage))
            error("Wrong argument type, class Image with dim(Image)[[3]] = 1 expected");
        SEXP dim = GET_DIM(rimage);
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
    catch(exception &error_) {
        error(error_.what());
    }
    /* this should never happen, but it prevents warning: the function exits either on
       previous return or on error - it should not happen that the control comes here
    */
    return MagickImage(Geometry(10, 10), "black");
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
MagickImage pullImageData(SEXP rimage, int index) {
    try {
        if (!assertImage(rimage))
            error("Wrong argument type, class Image expected");
        SEXP dim = GET_DIM(rimage);
        int dx = INTEGER(dim)[0];
        int dy = INTEGER(dim)[1];
        int nimage = INTEGER(dim)[2];
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
    catch(exception &error_) {
        error(error_.what());
    }
    /* this should never happen, but it prevents warning: the function exits either on
       previous return or on error - it should not happen that the control comes here
    */
    return MagickImage(Geometry(10, 10), "black");
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
void pushImageData(MagickImage& image, SEXP rimage, int index) {
    try {
        if (!assertImage(rimage))
            error("Wrong argument type, class Image expected");
        SEXP dim = GET_DIM(rimage);
        int dx = INTEGER(dim)[0];
        int dy = INTEGER(dim)[1];
        int nimage = INTEGER(dim)[2];
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
    catch(exception &error_) {
        error(error_.what());
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
    catch(exception &error_) {
        error(error_.what());
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
    catch(exception &error_) {
        error(error_.what());
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
        PROTECT(res = allocVector(REALSXP, nval));
        void * dest = &(REAL(res)[0]);
        image.opacity(OpaqueOpacity);
        /* image.type(GrayscaleType); */
        image.write(0, 0, nval, 1, "R", DoublePixel, dest);
        UNPROTECT(1);
        return res;
    }
    catch(exception &error_) {
        error(error_.what());
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
        PROTECT(res = allocVector(REALSXP, nval));
        void * dest = &(REAL(res)[0]);
        image.opacity(OpaqueOpacity);
        /* image.type(GrayscaleType); */
        image.write(0, 0, nval, 1, "G", DoublePixel, dest);
        UNPROTECT(1);
        return res;
    }
    catch(exception &error_) {
        error(error_.what());
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
        PROTECT(res = allocVector(REALSXP, nval));
        void * dest = &(REAL(res)[0]);
        image.opacity(OpaqueOpacity);
        /* image.type(GrayscaleType); */
        image.write(0, 0, nval, 1, "B", DoublePixel, dest);
        UNPROTECT(1);
        return res;
    }
    catch(exception &error_) {
        error(error_.what());
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
    catch(exception &error_) {
        error(error_.what());
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
    catch(exception &error_) {
        error(error_.what());
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
    catch(exception &error_) {
        error(error_.what());
    }
    return R_NilValue;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   colors
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP toColorString(SEXP rgb) {
    SEXP res = R_NilValue;
    bool isProtected = false;
    try {
        double * values = REAL(rgb);
        Color col;
        switch(LENGTH(rgb)) {
            case 1: col = ColorGray(values[0]); break;
            case 3: col = ColorRGB(values[0], values[1], values[2]); break;
            default:
                error("Please supply either 1 value for gray or 3 for RGB");
        }
        PROTECT(res = allocVector(STRSXP,1));
        isProtected = true;
        SET_STRING_ELT(res, 0, mkChar(string(col).c_str()));
        UNPROTECT(1);
    }
    catch(exception &error_) {
        if (isProtected)
            UNPROTECT(1);
        error(error_.what());
    }
    return res;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP fromColorString(SEXP str) { /* RGB vector */
    SEXP res = R_NilValue;
    bool isProtected = false;
    try {
        ColorRGB col = Color(CHAR(asChar(str)));
        PROTECT(res = allocVector(REALSXP, 3));
        isProtected = true;
        REAL(res)[0] = col.red();
        REAL(res)[1] = col.green();
        REAL(res)[2] = col.blue();
        UNPROTECT(1);
    }
    catch(exception &error_) {
        if (isProtected)
            UNPROTECT(1);
        error(error_.what());
    }
    return res;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
