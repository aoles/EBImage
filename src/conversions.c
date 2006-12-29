/* -------------------------------------------------------------------------
Image conversions between MagickCore and R
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "common.h"

/*----------------------------------------------------------------------- */
#define COMP_LENGTH 6

const char * COMP_IDS [] = { 
    "NONE", "LZW", "ZIP", "JPEG", "BZIP", "GROUP4" };

const CompressionType COMP_VALS [] = { 
    NoCompression, LZWCompression, ZipCompression, 
    JPEGCompression, BZipCompression, Group4Compression };

/*----------------------------------------------------------------------- */
#define FLTR_LENGTH 15

const char * FLTR_IDS [] = {
    "point", "box", "triangle", "hermite", "hanning", 
    "hamming", "blackman", "gaussian", "quadratic", "cubic", 
    "catrom", "mitchell", "lanczos", "bessel", "sinc" };

const FilterTypes FLTR_VALS [] = {
    PointFilter, BoxFilter, TriangleFilter, HermiteFilter, HanningFilter, 
    HammingFilter, BlackmanFilter, GaussianFilter, QuadraticFilter, CubicFilter,
    CatromFilter, MitchellFilter, LanczosFilter, BesselFilter, SincFilter };

/*----------------------------------------------------------------------- */
Image *
sexp2Magick (SEXP x) {
    int nx, ny, nz, colormode, i, j, * dim;
    Image * image, * res;
    ExceptionInfo exception;
    void * data;
    char * compressStr, * filterStr;
    
    /* basic checks */
    if ( !isImage(x) )
        error ( _("argument must be of class 'Image'") );
    dim = INTEGER ( GET_DIM(x) );
    nx = dim[0];
    ny = dim[1];
    nz = dim[2];
    colormode = INTEGER ( GET_SLOT(x, mkString("colormode") ) )[0];
    /* conversion */
    res = NewImageList ();
    GetExceptionInfo (&exception);
    for ( i = 0; i < nz; i++ ) {
        switch (colormode) {
            case MODE_RGB:
                data = &( INTEGER(x)[i * nx * ny] );
                image = ConstituteImage (nx, ny, "RGBO", CharPixel, data, &exception);
            break;
            default:/* grayscale */
                data = &( REAL(x)[i * nx * ny] );
                image = ConstituteImage (nx, ny, "I", DoublePixel, data, &exception);
        }
        if (exception.severity != UndefinedException) {
            CatchException (&exception);
            continue;
        }
        if ( image == (Image *)NULL ) {
            warning ( _("cannot convert the image") );
            continue;
        }
        if ( colormode == MODE_RGB ) 
            SetImageType (image, TrueColorType);
        else
            SetImageType (image, GrayscaleType);
        SetImageOpacity (image, 0);
        /* to enable display in all sessions, a used to be ssh and MacOS error */
        strcpy (image->filename, "\0");
        /* do not destroy image here */
        AppendImageToList (&res, image);
    }

    /* copy attributes: filename */
    strcpy ( res->filename, CHAR( asChar( GET_SLOT(x, mkString("filename") ) ) ) );
    /* propagate to all images */
    for ( i = 0; i < GetImageListLength(res); i++ ) {
        image = GetImageFromList (res, i);
        strcpy ( image->filename, CHAR( asChar( GET_SLOT(x, mkString("filename") ) ) ) );
    }
    /* copy attributes: compression */
    compressStr = CHAR( asChar( GET_SLOT(x, mkString("compression") ) ) );
    for ( i = 0; i < COMP_LENGTH; i++ )
        if ( strcmp(compressStr, COMP_IDS[i]) == 0 ) {
            res->compression = COMP_VALS[i];
            /* propagate to all images */
            for ( j = 0; j < GetImageListLength(res); j++ ) {
                image = GetImageFromList (res, j);
                image->compression = COMP_VALS[i];
            }
            break;
        }
    /* copy attributes: filter */
    filterStr = CHAR( asChar( GET_SLOT(x, mkString("filter") ) ) );
    for ( i = 0; i < FLTR_LENGTH; i++ )
        if ( strcmp(filterStr, FLTR_IDS[i]) == 0 ) {
            res->filter = FLTR_VALS[i];
            /* propagate to all images */
            for ( j = 0; j < GetImageListLength(res); j++ ) {
                image = GetImageFromList (res, j);
                image->filter = FLTR_VALS[i];
            }
            break;
        }
    /* copy attributes: resolution */
    res->x_resolution = REAL ( GET_SLOT(x, mkString("resolution") ) )[0];
    res->y_resolution = REAL ( GET_SLOT(x, mkString("resolution") ) )[1];
    /* propagate to all images */
    for ( i = 0; i < GetImageListLength(res); i++ ) {
        image = GetImageFromList (res, i);
        image->x_resolution = REAL ( GET_SLOT(x, mkString("resolution") ) )[0];
        image->y_resolution = REAL ( GET_SLOT(x, mkString("resolution") ) )[1];
    }
    
    return res;
}

/*----------------------------------------------------------------------- */
SEXP
magick2SEXP (Image * images, int colormode) {
    unsigned int nx, ny, nz, i, nprotect, dx, dy;
    Image * image;
    SEXP res, dim;
    void * data;
    ExceptionInfo exception;
    SEXP modeSlot, filenameSlot, compSlot, filterSlot, resSlot, features;
    
    if ( images == (Image *)NULL )
        return R_NilValue;

    nz = GetImageListLength (images);
    if ( nz < 1 )
        return R_NilValue;

    if ( colormode < 0 || colormode > MAX_MODE )
        error ( _("requested colormode is not supported") );

    res = R_NilValue;
    nprotect = 0;
    /* determine size of the first image */
    image = GetFirstImageInList (images);
    nx = image->columns;
    ny = image->rows;
    if ( nx * ny * nz == 0 ) {
        warning ( _("image size is zero") );
        return R_NilValue;
    }
    GetExceptionInfo(&exception);
    /* allocate memory and copy data */
    switch ( colormode ) {
        case MODE_RGB:
            PROTECT ( res = allocVector(INTSXP, nx * ny * nz) );
            nprotect++;
        break;
        default: /* grayscale */
            PROTECT ( res = allocVector(REALSXP, nx * ny * nz) );
            nprotect++;
    }

    /* copy data from image to SEXP */
    for ( i = 0; i < nz; i++ ) {
        image = GetImageFromList (images, i);
        if ( image->columns != nx || image->rows != ny )
            warning ( _("image size differs from that of the first one in the stack") );
        dx = image->columns < nx ? image->columns : nx;
        dy = image->rows < ny ? image->rows : ny;
        SetImageOpacity (image, 0);
        switch ( colormode ) {
            case MODE_RGB:
                data = &( INTEGER(res)[i * nx * ny] );
                SetImageType (image, TrueColorType);
                DispatchImage (image, 0, 0, dx, dy, "RGBO", CharPixel, data, &exception);
            break;
            default: /* grayscale */
                data = &( REAL(res)[i * nx * ny] );
                SetImageType (image, GrayscaleType);
                DispatchImage (image, 0, 0, dx, dy, "I", DoublePixel, data, &exception);
        }
        CatchException (&exception);
    }
    /* set image properties */
    /* class */
    SET_CLASS (res, mkString("Image") );
    /* dim */
    PROTECT ( dim = allocVector(INTSXP, 3) );
    nprotect++;
    INTEGER (dim)[0] = nx;
    INTEGER (dim)[1] = ny;
    INTEGER (dim)[2] = nz;
    SET_DIM (res, dim);

    /* copy attributes: colormode */
    PROTECT ( modeSlot = allocVector(INTSXP, 1) );
    nprotect++;
    INTEGER (modeSlot)[0] = colormode;
    SET_SLOT (res, mkString("colormode"), modeSlot);
    
    /* copy attributes: filename */
    PROTECT ( filenameSlot = allocVector(STRSXP, 1) );
    nprotect++;
    SET_STRING_ELT (filenameSlot, 0, mkChar(images->filename) );
    SET_SLOT (res, mkString("filename"), filenameSlot);
    /* copy attributes: compression */
    PROTECT ( compSlot = allocVector(STRSXP, 1) );
    nprotect++;
    for ( i = 0; i < COMP_LENGTH; i++ )
        if ( images->compression == COMP_VALS[i] ) {
            SET_STRING_ELT (compSlot, 0, mkChar(COMP_IDS[i]) );
            break;
        }
    SET_SLOT (res, mkString("compression"), compSlot);
    /* copy attributes: filter */
    PROTECT ( filterSlot = allocVector(STRSXP, 1) );
    nprotect++;
    for ( i = 0; i < FLTR_LENGTH; i++ )
        if ( images->filter == FLTR_VALS[i] ) {
            SET_STRING_ELT (filterSlot, 0, mkChar(FLTR_IDS[i]) );
            break;
        }
    SET_SLOT (res, mkString("filter"), filterSlot);
    /* copy attributes: resolution */
    PROTECT ( resSlot = allocVector(REALSXP, 2) );
    nprotect++;
    REAL (resSlot)[0] = images->x_resolution;
    REAL (resSlot)[1] = images->y_resolution;
    SET_SLOT (res, mkString("resolution"), resSlot);
    
    PROTECT ( features = allocVector(VECSXP, 0) );
    nprotect++;
    SET_CLASS (features, mkString("list") );
    SET_SLOT (res, mkString("features"), features);

    if ( nprotect > 0 )
        UNPROTECT (nprotect);
    return res;
}

#ifdef USE_GTK
/*----------------------------------------------------------------------- */
GdkPixbuf * newPixbufFromImages (Image * images, int index) {
    GdkPixbuf * res;
    Image * image;
    int nx, ny;
    ExceptionInfo exception;
    
    if ( images == NULL )
        return NULL;
    res = NULL;
    image = GetImageFromList (images, index);
    nx = image->columns;
    ny = image->rows;
    GetExceptionInfo(&exception);
    res = gdk_pixbuf_new (GDK_COLORSPACE_RGB, TRUE, 8, nx, ny);
    if ( GetImageType(images, &exception) == GrayscaleType )
        DispatchImage (image, 0, 0, nx, ny, "IIIA", CharPixel, gdk_pixbuf_get_pixels(res), &exception);
    else
        DispatchImage (image, 0, 0, nx, ny, "RGBA", CharPixel, gdk_pixbuf_get_pixels(res), &exception);
    if (exception.severity != UndefinedException) {
        CatchException (&exception);
        g_object_unref (res);
        return NULL;
    }
    return res;
}
#endif
