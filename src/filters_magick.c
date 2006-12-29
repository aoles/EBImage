/* -------------------------------------------------------------------------
Image filters from Magick
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "common.h"

/*----------------------------------------------------------------------- */
#define FLT_BLUR        0
#define FLT_GAUSSBLUR   1
#define FLT_CONTRAST    2
#define FLT_DENOISE     3
#define FLT_DESPECKLE   4
#define FLT_EDGE        5
#define FLT_ENHANCE     6
#define FLT_EQUALIZE    7
#define FLT_GAMMA       8
#define FLT_MEDIAN      9
#define FLT_NOISE       10
#define FLT_RESIZE      11
#define FLT_ROTATE      12
#define FLT_SAMPLE      13
#define FLT_SEGMENT     14
#define FLT_SHARPEN     15
#define FLT_UNSHARP     16
#define FLT_ATHRESH     17
#define FLT_CTHRESH     18
#define FLT_AFFINET     19
#define FLT_MODULATE    20
#define FLT_NEGATE      21
#define FLT_NORM        22


/*----------------------------------------------------------------------- */
SEXP
lib_filterMagick (SEXP x, SEXP filter, SEXP parameters) {
    Image * images, * newimages, * image;
    AffineMatrix amatrix;
    ExceptionInfo exception;
    int npar, mode, i, nz, nappended;
    double * par;
    SEXP res;
    char aStr[255];

    images = sexp2Magick (x);
    mode = INTEGER ( GET_SLOT(x, mkString("colormode") ) )[0];
    GetExceptionInfo(&exception);
    nz = GetImageListLength (images);
    par = &( REAL (parameters)[0] );
    npar = LENGTH (parameters);
    res = R_NilValue;
    newimages = NewImageList ();
    nappended = 0;

    for ( i = 0; i < nz; i++ ) {
        image = GetFirstImageInList (images);
        switch ( INTEGER(filter)[0] ) {
            case FLT_BLUR:
                image = BlurImage (image, par[0], par[1], &exception);
                break;
            case FLT_GAUSSBLUR:
                image = GaussianBlurImage (image, par[0], par[1], &exception);
                break;
            case FLT_CONTRAST:
                ContrastImage (image, (MagickBooleanType) par[0]);
                image = CloneImage (image, 0, 0, 1, &exception);
                break;
            case FLT_DENOISE:
                image = ReduceNoiseImage (image, par[0], &exception);
                break;
            case FLT_DESPECKLE:
                image = DespeckleImage (image, &exception);
                break;
            case FLT_EDGE:
                image = EdgeImage (image, par[0], &exception);
                break;        
            case FLT_ENHANCE:
                image = EnhanceImage (image, &exception);
                break;
            case FLT_EQUALIZE:
                EqualizeImage(image);
                image = CloneImage (image, 0, 0, 1, &exception);
                break;
           case FLT_GAMMA:
                sprintf (aStr, "%3.6f", par[0]);
                GammaImage(images, aStr);
                image = CloneImage (image, 0, 0, 1, &exception);
                break;
            case FLT_MEDIAN:
                image = MedianFilterImage (image, par[0], &exception);
                break;
            case FLT_NOISE:
                switch ( (int)par[0] ) {
                    case 1: image = AddNoiseImage (image, UniformNoise, &exception); break;
                    case 2: image = AddNoiseImage (image, GaussianNoise, &exception); break;
                    case 3: image = AddNoiseImage (image, MultiplicativeGaussianNoise, &exception); break;
                    case 4: image = AddNoiseImage (image, ImpulseNoise, &exception); break;
                    case 5: image = AddNoiseImage (image, LaplacianNoise, &exception); break;
                    case 6: image = AddNoiseImage (image, PoissonNoise, &exception); break;
                    default: image = AddNoiseImage (image, GaussianNoise, &exception); 
                }
                break;
            case FLT_RESIZE:
                image = ResizeImage (image, (unsigned long)par[0], (unsigned long)par[1], images->filter, par[2], &exception);
                break;
            case FLT_ROTATE:
                image = RotateImage (image, par[0], &exception);
                break;
            case FLT_SAMPLE:
                image = SampleImage (image, (unsigned long)par[0], (unsigned long)par[1], &exception);
                break;
            case FLT_SEGMENT:
                if ( mode == MODE_GRAY)
                    SegmentImage(image, GRAYColorspace, 0, par[0], par[1]);
                else
                    SegmentImage(image, RGBColorspace, 0, par[0], par[1]);
                image = CloneImage (image, 0, 0, 1, &exception);
                break;
            case FLT_SHARPEN:
                image = SharpenImage (image, par[0], par[1], &exception);
                break;
            case FLT_UNSHARP:
                image = UnsharpMaskImage (image, par[0], par[1], par[2], par[3], &exception);
                break;
            case FLT_ATHRESH:
                image = AdaptiveThresholdImage (image, (unsigned long)par[0], (unsigned long)par[1], (unsigned long)par[2], &exception);
                break;
            case FLT_CTHRESH:
                sprintf (aStr, "%3.6f", par[0]);
                ChannelThresholdImage (image, aStr);
                image = CloneImage (image, 0, 0, 1, &exception);
                break;
            case FLT_AFFINET:
                amatrix.sx = par[0];
                amatrix.rx = par[1];
                amatrix.ry = par[2];
                amatrix.sy = par[3];
                amatrix.tx = par[4];
                amatrix.ty = par[5];
                image = AffineTransformImage (image, &amatrix, &exception);
                break;
            case FLT_MODULATE:
                sprintf (aStr, "%3.6f", par[0]);
                ModulateImage (image, aStr);
                image = CloneImage (image, 0, 0, 1, &exception);
                break;
            case FLT_NEGATE:
                NegateImage (image, 0);
                image = CloneImage (image, 0, 0, 1, &exception);
                break;
            case FLT_NORM:
                NormalizeImage (image);
                image = CloneImage (image, 0, 0, 1, &exception);
                break;
            default:
                DestroyImage (images);
                DestroyExceptionInfo (&exception);
                error ( _("unsupported filter specified") );
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
            newimages->filter = images->filter;
            strcpy (newimages->filename, image->filename);
            newimages->x_resolution = images->x_resolution;
            newimages->y_resolution = images->y_resolution;
        }
        RemoveFirstImageFromList (&images);
    }

    DestroyImageList (images);

    PROTECT ( res = magick2SEXP(newimages, mode) );
    SET_SLOT (res, mkString("features"), Rf_duplicate( GET_SLOT(x, mkString("features") ) ) );
    DestroyImageList (newimages);
    UNPROTECT (1);
    
    return res;    
}

SEXP
lib_filterFill (SEXP x, SEXP colStrSXP, SEXP coords, SEXP methodSXP, SEXP fuzzSXP) {
    Image * images, * newimages, * image;
    ExceptionInfo exception;
    DrawInfo dinfo;
    ImageInfo iinfo;
    int mode, i, nz, nappended;
    SEXP res;
    PaintMethod pm;
    PixelPacket pp, tgt;
    int * xy, fuzz;
    char * str;

    images = sexp2Magick (x);
    mode = INTEGER ( GET_SLOT(x, mkString("colormode") ) )[0];
    GetExceptionInfo(&exception);
    nz = GetImageListLength (images);
    res = R_NilValue;
    nappended = 0;
    fuzz = INTEGER (fuzzSXP)[0];
    xy = INTEGER (coords);
    str = CHAR ( asChar(colStrSXP) );
    QueryColorDatabase (str, &pp, &exception);
    if ( exception.severity != UndefinedException ) {
        CatchException (&exception);
        DestroyImageList (images);
        error ( _("cannot identify color") ); 
    }
    str = CHAR ( asChar(methodSXP) );
    if ( strcmp (str, "floodfill") == 0 )
        pm =  FloodfillMethod;
    else
        pm = ReplaceMethod;
    newimages = NewImageList ();
    GetImageInfo (&iinfo);
    GetDrawInfo (&iinfo, &dinfo);
    dinfo.fill = pp;
    
warning ("FIXME: the fill function does not seem to fill anything, no idea why\n");

    for ( i = 0; i < nz; i++ ) {
        image = GetFirstImageInList (images);
        image->fuzz = fuzz;
        tgt = GetOnePixel (image, xy[0], xy[1]);
        ColorFloodfillImage (image, &dinfo, tgt, xy[0], xy[1], pm);
        image = CloneImage (image, 0, 0, 1, &exception);
        if (exception.severity != UndefinedException) {
            CatchException (&exception);
            continue;
        }
        AppendImageToList (&newimages, image);
        if ( nappended == 0 ) {
            /* copy attributes once and only if image contains more than 1 element */
            nappended = 1;
            newimages->compression = images->compression;
            newimages->filter = images->filter;
            strcpy (newimages->filename, image->filename);
            newimages->x_resolution = images->x_resolution;
            newimages->y_resolution = images->y_resolution;
        }
        RemoveFirstImageFromList (&images);
    }

    DestroyImageList (images);

    PROTECT ( res = magick2SEXP(newimages, mode) );
    DestroyImageList (newimages);
    UNPROTECT (1);
    
    return res;    
}
