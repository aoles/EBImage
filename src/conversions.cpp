/* -------------------------------------------------------------------------
Image conversion routines
Copyright (c) 2006 Oleg Sklyar
See: conversions.h for license
------------------------------------------------------------------------- */
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
