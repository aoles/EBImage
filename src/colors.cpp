#include "colors.h"

#include <R_ext/Error.h>
#include <iostream>

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
MagickImage colorToImage(SEXP x) {
    try {
        int nvalues = LENGTH(x);
        Geometry geom(nvalues, 1);
        MagickImage image(geom, "black");
        if (IS_INTEGER(x)) {
            image.read(nvalues, 1, "RGBp", CharPixel, &(INTEGER(x)[0]));
            return image;
        }
        if (IS_NUMERIC(x)) {
            image.read(nvalues, 1, "I", DoublePixel, &(REAL(x)[0]));
            return image;
        }
        if (IS_CHARACTER(x)) {
            char * str;
            for (int i = 0; i < nvalues; i++) {
                str = CHAR(STRING_ELT(x, i));
                if (strcmp(str, "NA") != 0)
                    image.pixelColor(i, 0, Color(str));
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
    return MagickImage(Geometry(1, 1), "black");
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP intToColorString(SEXP x) {
    SEXP res = R_NilValue;
    int nprotect = 0;
    try {
        if (!IS_INTEGER(x) && !IS_NUMERIC(x))
            error("argument must be a numeric or integer vector (NA's allowed)");
        int nvalues = LENGTH(x);
        PROTECT(res = allocVector(STRSXP, nvalues));
        nprotect++;
        MagickImage image = colorToImage(x);
        if (IS_INTEGER(x)) {
            int * values = &(INTEGER(x)[0]);
            for (int i = 0; i < nvalues; i++) {
                if (values[i] == NA_INTEGER)
                    SET_STRING_ELT(res, i, NA_STRING);
                else
                    SET_STRING_ELT(res, i, mkChar(string(image.pixelColor(i, 0)).c_str()));
            }
        }
        else {
            double * values = &(REAL(x)[0]);
            for (int i = 0; i < nvalues; i++) {
                if (isnan(values[i]) || isinf(values[i]))
                    SET_STRING_ELT(res, i, NA_STRING);
                else
                    SET_STRING_ELT(res, i, mkChar(string(image.pixelColor(i, 0)).c_str()));
            }
        }
    }
    catch(exception &error_) {
        error(error_.what());
    }
    UNPROTECT(nprotect);
    return res;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP colorStringToInt(SEXP x) {
    SEXP res = R_NilValue;
    int nprotect = 0;
    try {
        if (!IS_CHARACTER(x))
            error("argument must be a character vector (NA's allowed)");
        int nvalues = LENGTH(x);
        PROTECT(res = allocVector(INTSXP, nvalues));
        nprotect++;
        MagickImage image = colorToImage(x);
        int * values = &(INTEGER(res)[0]);
        image.opacity(OpaqueOpacity);
        image.type(TrueColorType);
        image.write(0, 0, nvalues, 1, "RGBp", CharPixel, values);
        for (int i = 0; i < nvalues; i++)
            if (strcmp(CHAR(STRING_ELT(x, i)), "NA") == 0)
                values[i] = NA_INTEGER;
    }
    catch(exception &error_) {
        error(error_.what());
    }
    UNPROTECT(nprotect);
    return res;
}

