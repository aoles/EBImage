#include "filters_magick.h"

/* -------------------------------------------------------------------------
Image filters from Magick
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tools.h"
#include "conversions.h"

#include <R_ext/Error.h>
#include <magick/ImageMagick.h>

/*----------------------------------------------------------------------- */
#define FLTR_LENGTH 15

const FilterTypes FLTR_VALS [] = {
    PointFilter, BoxFilter, TriangleFilter, HermiteFilter, HanningFilter,
    HammingFilter, BlackmanFilter, GaussianFilter, QuadraticFilter, CubicFilter,
    CatromFilter, MitchellFilter, LanczosFilter, BesselFilter, SincFilter };

/*----------------------------------------------------------------------- */
#define FLT_BLUR        0
#define FLT_GAUSSBLUR   1
#define FLT_EQUALIZE    7
#define FLT_RESIZE      11
#define FLT_ROTATE      12

/*----------------------------------------------------------------------- */
SEXP
lib_filterMagick (SEXP x, SEXP filter, SEXP parameters) {
    Image * images, * newimages, * image;
    ExceptionInfo exception;
    int npar, mode, i, nz, nappended, flt;
    double * par;
    SEXP res;

    flt = INTEGER(filter)[0];
    mode = getColorMode(x);
    par = &( REAL (parameters)[0] );
    npar = LENGTH (parameters);

    images = sexp2Magick (x);
    if ( flt == FLT_RESIZE ) images->filter = FLTR_VALS[(int)(par[3])];
    GetExceptionInfo(&exception);
    nz = GetImageListLength (images);
    res = R_NilValue;
    newimages = NewImageList ();
    nappended = 0;

    for ( i = 0; i < nz; i++ ) {
        image = GetFirstImageInList (images);
        switch ( flt ) {
            case FLT_BLUR:
                image = BlurImage (image, par[0], par[1], &exception);
                break;
            case FLT_GAUSSBLUR:
                image = GaussianBlurImage (image, par[0], par[1], &exception);
                break;
            case FLT_EQUALIZE:
                EqualizeImage(image);
                image = CloneImage (image, 0, 0, 1, &exception);
                break;
            case FLT_RESIZE:
                image = ResizeImage (image, (unsigned long)par[0], (unsigned long)par[1], images->filter, par[2], &exception);
                break;
            case FLT_ROTATE:
                /* FIXME: implement BG specification in rotate()
                 * this doesn't work, it is not purely correct either, but this
                 * is the way to go later
                 * col = (int)par[1];
                 * image->background_color.red = colb[0] / 255.0;
                 * image->background_color.green = colb[1] / 255.0;
                 * image->background_color.blue = colb[2] / 255.0;
                 * image->background_color.opacity = 1.0; */
                /* or we can supply the color as string, which is much more
                 * straight forward, but then we need to change a lot of R
                 * code to add an extra NULL to all functions calling
                 * this one from R:
                 *  str = CHAR ( asChar(colStrSXP) ); */
                QueryColorDatabase ("black", &(image->background_color), &exception);
                image = RotateImage (image, par[0], &exception);
                break;
            default:
                images = DestroyImage (images);
                DestroyExceptionInfo (&exception);
                error ( "unsupported filter specified" );
        }
        if (exception.severity != UndefinedException) {
            CatchException (&exception);
            continue;
        }
        AppendImageToList (&newimages, image);
        if ( nappended == 0 ) {
            /* copy attributes once and only if image contains more than 1 element */
            nappended = 1;
            newimages->compression = images->compression;
            strcpy (newimages->filename, image->filename);
            newimages->x_resolution = images->x_resolution;
            newimages->y_resolution = images->y_resolution;
        }
        image = GetFirstImageInList (images);
        RemoveFirstImageFromList (&images);
        image = DestroyImage( image );
    }

    images = DestroyImageList (images);

    PROTECT ( res = magick2SEXP(newimages, mode) );
    newimages = DestroyImageList (newimages);

    DestroyExceptionInfo(&exception);

    UNPROTECT (1);
    return res;
}

