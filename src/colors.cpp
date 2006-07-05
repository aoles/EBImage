#include "colors.h"

#include <R_ext/Error.h>
#include <vector>
#include <iostream>

const int ALL = 0;
const int R   = 1;
const int G   = 2;
const int B   = 3;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
SEXP          image2INTEGER   (MagickImage & image, vector<int> & nas, int ch);
SEXP          image2REAL      (MagickImage &, vector<int> &, int ch);
SEXP          image2CHARACTER (MagickImage &, vector<int> &, int ch);
vector<int>   getNAs          (SEXP);
MagickImage   vector2image    (SEXP, vector<int> &);
void          add2image       (MagickImage &, MagickImage &);
void          sub2image       (MagickImage &, MagickImage &);
void          scale2image     (MagickImage &, double);

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
SEXP any2rgb(SEXP x) {
    vector<int> nas = getNAs(x);
    MagickImage image = vector2image(x, nas);
    return image2INTEGER(image, nas, ALL);
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
SEXP any2gray(SEXP x) {
    vector<int> nas = getNAs(x);
    MagickImage image = vector2image(x, nas);
    return image2REAL(image, nas, ALL);
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
SEXP any2X11char(SEXP x) {
    vector<int> nas = getNAs(x);
    MagickImage image = vector2image(x, nas);
    return image2CHARACTER(image, nas, ALL);
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
SEXP add2rgb(SEXP x, SEXP y) {
    vector<int> nas = getNAs(x);
    vector<int> nas1 = getNAs(y);
    MagickImage image = vector2image(x, nas);
    MagickImage addition = vector2image(y, nas1);
    add2image(image, addition);
    /* TODO combine to nas */
    return image2INTEGER(image, nas, ALL);
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
SEXP sub2rgb(SEXP x, SEXP y) {
    vector<int> nas = getNAs(x);
    vector<int> nas1 = getNAs(y);
    MagickImage image = vector2image(x, nas);
    MagickImage subtraction = vector2image(y, nas1);
    sub2image(image, subtraction);
    /* TODO combine to nas */
    return image2INTEGER(image, nas, ALL);
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
SEXP scale2rgb(SEXP x, SEXP factor) {
    vector<int> nas = getNAs(x);
    MagickImage image = vector2image(x, nas);
    try {
        double value = REAL(factor)[0];
        scale2image(image, value);
    }
    catch(exception &error_) {
        error(error_.what());
    }
    return image2INTEGER(image, nas, ALL);
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
SEXP getred(SEXP x) {
    vector<int> nas = getNAs(x);
    MagickImage image = vector2image(x, nas);
    return image2REAL(image, nas, R);
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
SEXP asred(SEXP x) {
    vector<int> nas = getNAs(x);
    MagickImage image = vector2image(x, nas);
    return image2INTEGER(image, nas, R);
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
SEXP getgreen(SEXP x) {
    vector<int> nas = getNAs(x);
    MagickImage image = vector2image(x, nas);
    return image2REAL(image, nas, G);
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
SEXP asgreen(SEXP x) {
    vector<int> nas = getNAs(x);
    MagickImage image = vector2image(x, nas);
    return image2INTEGER(image, nas, G);
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
SEXP getblue(SEXP x) {
    vector<int> nas = getNAs(x);
    MagickImage image = vector2image(x, nas);
    return image2REAL(image, nas, B);
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
SEXP asblue(SEXP x) {
    vector<int> nas = getNAs(x);
    MagickImage image = vector2image(x, nas);
    return image2INTEGER(image, nas, B);
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
// internal function that returns an R-integer (RGB) from the supplied image
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
SEXP image2INTEGER(MagickImage & image, vector<int> & nas, int ch) {
    SEXP res = R_NilValue;
    int nprotect = 0;
    try {
        int nvalues = image.columns() * image.rows();
        PROTECT(res = allocVector(INTSXP, nvalues));
        nprotect++;
        int * values = &(INTEGER(res)[0]);
        image.opacity(OpaqueOpacity);
        image.type(TrueColorType);
        switch (ch) {
            case R: image.write(0, 0, image.columns(), image.rows(), "Rppp", CharPixel, values); break;
            case G: image.write(0, 0, image.columns(), image.rows(), "pGpp", CharPixel, values); break;
            case B: image.write(0, 0, image.columns(), image.rows(), "ppBp", CharPixel, values); break;
            default: image.write(0, 0, image.columns(), image.rows(), "RGBp", CharPixel, values); 
        }
        if (nas.size() > 0) {
            vector<int>::iterator it = nas.begin();
            for (int i = 0; i < nvalues; i++) {
                if (*it != i) continue;
                values[i] = NA_INTEGER;
                if (it != nas.end()) it++;
            }
        }
    }
    catch(exception &error_) {
        error(error_.what());
    }
    UNPROTECT(nprotect);
    return res;
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
// internal function that returns an R-numeric (grayscale) from the supplied image
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
SEXP image2REAL(MagickImage & image, vector<int> & nas, int ch) {
    SEXP res = R_NilValue;
    int nprotect = 0;
    try {
        int nvalues = image.columns() * image.rows();
        PROTECT(res = allocVector(REALSXP, nvalues));
        nprotect++;
        double * values = &(REAL(res)[0]);
        image.opacity(OpaqueOpacity);
        if (ch == ALL)
            image.type(GrayscaleType);
        else
        image.type(TrueColorType);
        switch (ch) {
            case R: image.write(0, 0, image.columns(), image.rows(), "R", DoublePixel, values); break;
            case G: image.write(0, 0, image.columns(), image.rows(), "G", DoublePixel, values); break;
            case B: image.write(0, 0, image.columns(), image.rows(), "B", DoublePixel, values); break;
            default: image.write(0, 0, image.columns(), image.rows(), "I", DoublePixel, values);
        }
        if (nas.size() > 0) {
            vector<int>::iterator it = nas.begin();
            for (int i = 0; i < nvalues; i++) {
                if (*it != i) continue;
                values[i] = NA_REAL;
                if (it != nas.end()) it++;
            }
        }
    }
    catch(exception &error_) {
        error(error_.what());
    }
    UNPROTECT(nprotect);
    return res;
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
// internal function that returns an R-character (X11-format) from the supplied image
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
SEXP image2CHARACTER(MagickImage & image, vector<int> & nas, int ch) {
    SEXP res = R_NilValue;
    int nprotect = 0;
    try {
        int nx = image.columns();
        int ny = image.rows();
        PROTECT(res = allocVector(STRSXP, nx * ny));
        nprotect++;
        image.opacity(OpaqueOpacity);
        image.type(TrueColorType);
        ColorRGB col;
        if (nas.size() <= 0) {
            for (int j = 0; j < ny; j++)
                for (int i = 0; i < nx; i++) {
                    col = image.pixelColor(i, j);
                    switch (ch) {
                        case R: {
                            col.green(0);
                            col.blue(0);
                        }; break;
                        case G: {
                            col.red(0);
                            col.blue(0);
                        }; break;
                        case B: {
                            col.red(0);
                            col.green(0);
                        }; break;
                        default: {};                    
                    }
                    SET_STRING_ELT(res, i + j * nx, mkChar(string(col).c_str()));
                }
        }
        else {
            int index;
            vector<int>::iterator it = nas.begin();
            for (int j = 0; j < ny; j++)
                for (int i = 0; i < nx; i++) {
                    index = i + j * nx;
                    if (*it == index) {
                        SET_STRING_ELT(res, index, NA_STRING);
                        if (it != nas.end()) it++;
                    }
                    else {
                        col = image.pixelColor(i, j);
                        switch (ch) {
                            case R: {
                                col.green(0);
                                col.blue(0);
                            }; break;
                            case G: {
                                col.red(0);
                                col.blue(0);
                            }; break;
                            case B: {
                                col.red(0);
                                col.green(0);
                            }; break;
                            default: {};                    
                        }
                        SET_STRING_ELT(res, index, mkChar(string(col).c_str()));
                    }
                }
        }
    }
    catch(exception &error_) {
        error(error_.what());
    }
    UNPROTECT(nprotect);
    return res;
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
// internal function that returns a vector with indexes of all NAs in an 
// integer, character or numeric R-vector 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
vector<int> getNAs(SEXP x) {
    vector<int> res;
    try {
        int nvalues = LENGTH(x);
        if (IS_INTEGER(x)) {
            int * values = &(INTEGER(x)[0]);
            for (int i = 0; i < nvalues; i++)
                if (values[i] == NA_INTEGER)
                    res.push_back(i);
            return res;
        }
        if (IS_NUMERIC(x)) {
            double * values = &(REAL(x)[0]);
            for (int i = 0; i < nvalues; i++)
                if (isnan(values[i]))
                    res.push_back(i);
            return res;
        }
        if (IS_CHARACTER(x)) {
            for (int i = 0; i < nvalues; i++)
                if (strcmp(CHAR(STRING_ELT(x, i)), "NA") == 0)
                    res.push_back(i);
            return res;
        }
        error("supplied argument could not be coersed to numeric, integer or character");
    }
    catch(exception &error_) {
        error(error_.what());
    }
    /* unreachable */
    return res;    
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
// internal functions that converts an integer, character or numeric R-vector
// into a MagickImage of size nvalues x 1, supply vector of NA indexes 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
MagickImage vector2image(SEXP x, vector<int> & nas) {
    try {
        int nvalues = LENGTH(x);
        if (IS_INTEGER(x))
            return MagickImage(nvalues, 1, "RGBp", CharPixel, &(INTEGER(x)[0]));
        if (IS_NUMERIC(x))
            return MagickImage(nvalues, 1, "I", DoublePixel, &(REAL(x)[0]));
        if (IS_CHARACTER(x)) {
            MagickImage image(Geometry(nvalues, 1), "black");
            if (nas.size() <= 0) {
                for (int i = 0; i < nvalues; i++)
                    image.pixelColor(i, 0, Color(CHAR(STRING_ELT(x, i))));
            }
            else {
                vector<int>::iterator it = nas.begin();
                for (int i = 0; i < nvalues; i++) {
                    if (*it == i) {
                        if (it != nas.end())
                            it++;
                        continue;
                    }
                    image.pixelColor(i, 0, Color(CHAR(STRING_ELT(x, i))));
                }
            }
            return image;
        }
        error("supplied argument could not be coersed to numeric, integer or character");
    }
    catch(exception &error_) {
        error(error_.what());
    }
    /* unreachable */
    return MagickImage();
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
// internal functions that adds to equal-size images
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
void add2image(MagickImage & image, MagickImage & addition) {
    try {
        if (image.size() != addition.size())
            error("supplied arrays have different sizes");
        ColorRGB col, coladd;
        int nx = image.columns();
        int ny = image.rows();
        double val;
        for (int j = 0; j < ny; j++)
            for (int i = 0; i < nx; i++) {
                col = image.pixelColor(i, j);
                coladd = addition.pixelColor(i, j);
                val = col.red() + coladd.red();
                if (val > 1.0) val = 1.0;
                col.red(val);
                val = col.green() + coladd.green();
                if (val > 1.0) val = 1.0;
                col.green(val);
                val = col.blue() + coladd.blue();
                if (val > 1.0) val = 1.0;
                col.blue();
                image.pixelColor(i, j, col);
            }
    }
    catch(exception &error_) {
        error(error_.what());
    }
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
// internal functions that subtracts an equal-size images
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
void sub2image(MagickImage & image, MagickImage & subtraction) {
    try {
        if (image.size() != subtraction.size())
            error("supplied arrays have different sizes");
        ColorRGB col, colsub;
        int nx = image.columns();
        int ny = image.rows();
        double val;
        for (int j = 0; j < ny; j++)
            for (int i = 0; i < nx; i++) {
                col = image.pixelColor(i, j);
                colsub = subtraction.pixelColor(i, j);
                val = col.red() - colsub.red();
                if (val < 0.0) val = 0.0;
                col.red(val);
                val = col.green() - colsub.green();
                if (val < 0.0) val = 0.0;
                col.green(val);
                val = col.blue() - colsub.blue();
                if (val < 0.0) val = 0.0;
                col.blue();
                image.pixelColor(i, j, col);
            }
    }
    catch(exception &error_) {
        error(error_.what());
    }
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
// internal function that scales image color
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
void scale2image(MagickImage & image, double factor) {
    try {
        ColorRGB col;
        int nx = image.columns();
        int ny = image.rows();
        double val;
        for (int j = 0; j < ny; j++)
            for (int i = 0; i < nx; i++) {
                col = image.pixelColor(i, j);
                val = col.red() * factor;
                if (val < 0.0) val = 0.0;
                if (val > 1.0) val = 1.0;
                col.red(val);
                val = col.green() * factor;
                if (val < 0.0) val = 0.0;
                if (val > 1.0) val = 1.0;
                col.green(val);
                val = col.blue() * factor;
                if (val < 0.0) val = 0.0;
                if (val > 1.0) val = 1.0;
                col.blue();
                image.pixelColor(i, j, col);
            }
    }
    catch(exception &error_) {
        error(error_.what());
    }
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/*SEXP toGray(SEXP rgb) {
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
        // image.type(GrayscaleType);
        image.write(0, 0, nval, 1, "R", DoublePixel, dest);
        UNPROTECT(1);
        return res;
    }
    catch(exception &error_) {
        error(error_.what());
    }
    return R_NilValue;
}
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
        // image.type(GrayscaleType);
        image.write(0, 0, nval, 1, "G", DoublePixel, dest);
        UNPROTECT(1);
        return res;
    }
    catch(exception &error_) {
        error(error_.what());
    }
    return R_NilValue;
}
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
        // image.type(GrayscaleType);
        image.write(0, 0, nval, 1, "B", DoublePixel, dest);
        UNPROTECT(1);
        return res;
    }
    catch(exception &error_) {
        error(error_.what());
    }
    return R_NilValue;
}
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
*/
