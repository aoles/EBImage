#include "colors.h"

/* -------------------------------------------------------------------------
Image conversions between MagickCore and R
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include <Rgraphics.h>
#include <R_ext/Error.h>

/*----------------------------------------------------------------------- */
#define RGBA  0
#define GRAY  1
#define R     2
#define G     3
#define B     4
#define AsR   5
#define AsG   6
#define AsB   7
#define X11C  8
/*----------------------------------------------------------------------- */
/* forward declarations */
Image * vector2image1D (SEXP);
SEXP image1D2REAL (Image *, int);
SEXP image1D2INTEGER (Image *, int);
SEXP image1D2CHAR (Image *);

/*----------------------------------------------------------------------- */
SEXP
lib_channel (SEXP x, SEXP mode) {
    SEXP res;
    Image * image;

    image = vector2image1D (x);

    res = R_NilValue;
    switch ( INTEGER (mode)[0] ) {
        case RGBA:
            res = image1D2INTEGER (image, RGBA);
            break;
        case GRAY:
            res = image1D2REAL (image, RGBA);
            break;
        case R:
            res = image1D2REAL (image, R);
            break;
        case G:
            res = image1D2REAL (image, G);
            break;
        case B:
            res = image1D2REAL (image, B);
            break;
        case AsR:
            res = image1D2INTEGER (image, R);
            break;
        case AsG:
            res = image1D2INTEGER (image, G);
            break;
        case AsB:
            res = image1D2INTEGER (image, B);
            break;
        case X11C:
            res = image1D2CHAR (image);
            break;
        default:
            image = DestroyImage (image);
            error ( "incorrect mode" );
    }
    image = DestroyImage (image);
    return res;
}

/*----------------------------------------------------------------------- */
Image *
vector2image1D (SEXP x) {
    Image * res;
    int nvals;
    ExceptionInfo exception;
    int i;
    PixelPacket * pixelPtr;
    char pixelStr[255];
    ImageInfo info;

    nvals = LENGTH (x);
    res = NULL;
    GetExceptionInfo (&exception);
    if ( IS_INTEGER(x) )
        res = ConstituteImage (nvals, 1, "RGBp", CharPixel, &( INTEGER(x)[0] ), &exception );
    else if ( IS_NUMERIC(x) )
        res = ConstituteImage (nvals, 1, "I", DoublePixel, &( REAL(x)[0] ), &exception );
    else if ( IS_CHARACTER(x) ) {
        GetImageInfo (&info);
        res = AllocateImage (&info);
        if ( SetImageExtent(res, nvals, 1) == MagickFalse )
            error ( "cannot allocate memory" );
        QueryColorDatabase ("black", &res->background_color, &exception);
        if (exception.severity != UndefinedException)
            CatchException (&exception);
        SetImageBackgroundColor (res);
        for ( i = 0; i < nvals; i++ ) {
            strcpy (pixelStr, CHAR( STRING_ELT(x, i) ) );
            if ( strcmp(pixelStr, "NA") == 0 ) continue;
            pixelPtr = SetImagePixels (res, i, 0, 1, 1);
            QueryColorDatabase (pixelStr, pixelPtr, &exception);
            CatchException (&exception);
        }
        exception.severity = UndefinedException;
    }

    CatchException (&exception);
    if ( res != NULL )
        SetImageOpacity (res, 0);
    return res;
}

/*----------------------------------------------------------------------- */
Image *
int2image1D (int * x, int nvals) {
    Image * res;
    ExceptionInfo exception;

    res = NULL;
    GetExceptionInfo (&exception);
    res = ConstituteImage (nvals, 1, "RGBp", CharPixel, x, &exception );
    CatchException (&exception);
    if ( res != NULL )
        SetImageOpacity (res, 0);
    return res;
}

/*----------------------------------------------------------------------- */
Image *
double2image1D (double * x, int nvals) {
    Image * res;
    ExceptionInfo exception;

    res = NULL;
    GetExceptionInfo (&exception);
    res = ConstituteImage (nvals, 1, "I", DoublePixel, x, &exception );
    CatchException (&exception);
    if ( res != NULL )
        SetImageOpacity (res, 0);
    return res;
}


/*----------------------------------------------------------------------- */
SEXP
image1D2REAL (Image * image, int what) {
    SEXP res;
    int nprotect, nvals;
    double * data;
    ExceptionInfo exception;

    if ( image == NULL ) return R_NilValue;
    res = R_NilValue;
    nprotect = 0;
    nvals = image->columns;
    PROTECT (res = allocVector(REALSXP, nvals) );
    nprotect++;
    data = REAL (res);

    GetExceptionInfo (&exception);
    switch ( what ) {
        case R:
            DispatchImage (image, 0, 0, nvals, 1, "R", DoublePixel, data, &exception );
            break;
        case G:
            DispatchImage (image, 0, 0, nvals, 1, "G", DoublePixel, data, &exception );
            break;
        case B:
            DispatchImage (image, 0, 0, nvals, 1, "B", DoublePixel, data, &exception );
            break;
        default:
            DispatchImage (image, 0, 0, nvals, 1, "I", DoublePixel, data, &exception );
    }

    CatchException (&exception);

    UNPROTECT (nprotect);
    return res;
}

/*----------------------------------------------------------------------- */
void
image1D2double (Image * image, double * tgt, int nvals) {
    ExceptionInfo exception;

    if ( image == NULL ) return;

    GetExceptionInfo (&exception);
    DispatchImage (image, 0, 0, nvals, 1, "I", DoublePixel, tgt, &exception );
    CatchException (&exception);
}

/*----------------------------------------------------------------------- */
SEXP
image1D2INTEGER (Image * image, int what) {
    SEXP res;
    int nprotect, nvals;
    int * data;
    ExceptionInfo exception;

    if ( image == NULL ) return R_NilValue;
    res = R_NilValue;
    nprotect = 0;
    nvals = image->columns;
    PROTECT (res = allocVector(INTSXP, nvals) );
    nprotect++;
    data = INTEGER (res);

    GetExceptionInfo (&exception);
    switch ( what ) {
        case R:
            DispatchImage (image, 0, 0, nvals, 1, "Rppp", CharPixel, data, &exception );
            break;
        case G:
            DispatchImage (image, 0, 0, nvals, 1, "pGpp", CharPixel, data, &exception );
            break;
        case B:
            DispatchImage (image, 0, 0, nvals, 1, "ppBp", CharPixel, data, &exception );
            break;
        default:
            DispatchImage (image, 0, 0, nvals, 1, "RGBp", CharPixel, data, &exception );
    }

    CatchException (&exception);

    UNPROTECT (nprotect);
    return res;
}

/*----------------------------------------------------------------------- */
void
image1D2int (Image * image, int * tgt, int nvals) {
    ExceptionInfo exception;

    if ( image == NULL ) return;

    GetExceptionInfo (&exception);
    DispatchImage (image, 0, 0, nvals, 1, "RGBp", CharPixel, tgt, &exception );
    CatchException (&exception);
}

/*----------------------------------------------------------------------- */
SEXP
image1D2CHAR (Image * image) {
    SEXP res;
    int nprotect, i, nvals;
    char pixelStr[128], component[16];
    PixelPacket pp;

    if ( image == NULL ) return R_NilValue;
    nprotect = 0;
    nvals = image->columns;
    PROTECT (res = allocVector(STRSXP, nvals) );
    nprotect++;

    for ( i = 0; i < (int)image->columns; i++ ) {
        pp = GetOnePixel (image, i, 0);
        strcpy (pixelStr, "#");
        sprintf (component, "%02X", ScaleQuantumToChar(pp.red) );
        strcat (pixelStr, component);
        sprintf (component, "%02X", ScaleQuantumToChar(pp.green) );
        strcat (pixelStr, component);
        sprintf (component, "%02X", ScaleQuantumToChar(pp.blue) );
        strcat (pixelStr, component);
        SET_STRING_ELT ( res, i, mkChar( pixelStr ) );
    }

    UNPROTECT (nprotect);
    return res;
}
