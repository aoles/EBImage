#include "conversions.h"

/* -------------------------------------------------------------------------
Image conversions between MagickCore and R
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tools.h"

#include <R_ext/Error.h>

/*----------------------------------------------------------------------- */
Image *
sexp2Magick (SEXP x) {
    int nx, ny, nc, nz, colormode, i, * dim;
    Image * image, * res;
    ExceptionInfo exception;
    void * data;
    double *data2, *dp;
    int j,k,redstride,greenstride,bluestride;

    validImage(x,0);

    dim = INTEGER ( GET_DIM(x) );
    nx = dim[0];
    ny = dim[1];
    nz = getNumberOfFrames(x,1);
    colormode = getColorMode(x);

    /* conversion */
    res = NewImageList ();
    GetExceptionInfo (&exception);
    for ( i = 0; i < nz; i++ ) {
        switch (colormode) {
            case MODE_TRUECOLOR:
                data = &( INTEGER(x)[i * nx * ny] );
                image = ConstituteImage (nx, ny, "RGBO", CharPixel, data, &exception);
            break;
	case MODE_COLOR:
	  // GP: Using a temporary double to constitute an ImageMagick Image
	  // GP: I didn't find a better way to do it in ImageMagick...
	  nc=getNumberOfChannels(x);
	  dp=REAL(x);
	  data2=(double *)R_Calloc(nx*ny*3,double);
	  getColorStrides(x,i,&redstride,&greenstride,&bluestride);
	  
	  for (j=0;j<ny;j++) for (k=0;k<nx;k++) {
	      if (redstride!=-1) data2[3*k+j*3*nx]=dp[k+j*nx+redstride];
	      else data2[3*k+j*3*nx]=0.0;
	      if (greenstride!=-1) data2[3*k+j*3*nx+1]=dp[k+j*nx+greenstride];
	      else data2[3*k+j*3*nx+1]=0.0;
	      if (bluestride!=-1) data2[3*k+j*3*nx+2]=dp[k+j*nx+bluestride];
	      else data2[3*k+j*3*nx+2]=0.0;
	    }
	 
	  image = ConstituteImage (nx, ny, "RGB", DoublePixel, data2, &exception);

	  R_Free(data2);
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
            warning ( "cannot convert the image" );
            continue;
        }
	/*
        if ( colormode == MODE_GRAYSCALE )
	  SetImageType (image, GrayscaleType);
        else
	  SetImageType (image, TrueColorType );*/
	
        SetImageOpacity (image, 0);
        /* to enable display in all sessions, a used to be ssh and MacOS error */
        strcpy (image->filename, "\0");
        /* do not destroy image here */
        AppendImageToList (&res, image);
    }

    DestroyExceptionInfo(&exception);
    return res;
}

/*----------------------------------------------------------------------- */
SEXP
magick2SEXP (Image * images, int colormode) {
    unsigned int nx, ny, nz, i, nprotect, dx, dy;
    Image * image;
    SEXP res, resd, dim;
    void * data;
    ExceptionInfo exception;
    SEXP modeSlot;

    if ( images == (Image *)NULL )
        return R_NilValue;

    nz = GetImageListLength (images);
    if ( nz < 1 )
        return R_NilValue;

    if ( colormode < 0 || colormode > MODE_MAX)
        error ( "requested colormode is not supported" );

    res = R_NilValue;
    nprotect = 0;
    /* determine size of the first image */
    image = GetFirstImageInList (images);
    nx = image->columns;
    ny = image->rows;
    if ( nx * ny * nz == 0 ) {
        warning ( "image size is zero" );
        return R_NilValue;
    }
    GetExceptionInfo(&exception);
    /* allocate memory and copy data */
    switch ( colormode ) {
        case MODE_TRUECOLOR:
            PROTECT ( resd = allocVector(INTSXP, nx * ny * nz) );
            nprotect++;
        break;
        case MODE_COLOR:
            PROTECT ( resd = allocVector(REALSXP, nx * ny * 3 * nz) );
            nprotect++;
        break;
        default: /* grayscale */
            PROTECT ( resd = allocVector(REALSXP, nx * ny * nz) );
            nprotect++;
    }

    /* copy data from image to SEXP */
    for ( i = 0; i < nz; i++ ) {
        image = GetImageFromList (images, i);
        if ( image->columns != nx || image->rows != ny )
            warning ( "image size differs from that of the first one in the stack" );
        dx = image->columns < nx ? image->columns : nx;
        dy = image->rows < ny ? image->rows : ny;
        SetImageOpacity (image, 0);
        switch ( colormode ) {
            case MODE_TRUECOLOR:
                data = &( INTEGER(resd)[i * nx * ny] );
                SetImageType (image, TrueColorType);
                DispatchImage (image, 0, 0, dx, dy, "RGBO", CharPixel, data, &exception);
            break;
	    case MODE_COLOR:
                data = &( REAL(resd)[i * 3 * nx * ny] );
                SetImageType (image, TrueColorType);
                DispatchImage (image, 0, 0, dx, dy, "R", DoublePixel, data, &exception);
		data = &( REAL(resd)[i * 3 * nx * ny + 1 * nx * ny] );
                DispatchImage (image, 0, 0, dx, dy, "G", DoublePixel, data, &exception);
		data = &( REAL(resd)[i * 3 * nx * ny + 2 * nx * ny] );
                DispatchImage (image, 0, 0, dx, dy, "B", DoublePixel, data, &exception);
            break;
            default: /* grayscale */
                data = &( REAL(resd)[i * nx * ny] );
                SetImageType (image, GrayscaleType);
                DispatchImage (image, 0, 0, dx, dy, "I", DoublePixel, data, &exception);
        }
        CatchException (&exception);
    }
    /* set image properties */
    /* dim */
    if (colormode!=MODE_COLOR) {
      if (nz==1) PROTECT(dim=allocVector(INTSXP, 2));
      else PROTECT(dim=allocVector(INTSXP, 3));
      nprotect++;
      INTEGER (dim)[0] = nx;
      INTEGER (dim)[1] = ny;
      if (nz!=1) INTEGER (dim)[2] = nz;
    } else {
      if (nz==1) PROTECT(dim=allocVector(INTSXP, 3));
      else PROTECT(dim=allocVector(INTSXP, 4));
      nprotect++;
      INTEGER (dim)[0] = nx;
      INTEGER (dim)[1] = ny;
      INTEGER (dim)[2] = 3;
      if (nz!=1) INTEGER (dim)[3] = nz;
    }

    SET_DIM (resd, dim);

    /* class */
    PROTECT(res = NEW_OBJECT(MAKE_CLASS("Image")) );
    nprotect++;
    /* WARNING: we must reassign here, otherwise .Data slot is not assigned
     * apparently this problem exists only for .Data slot */
    res = SET_SLOT(res, install(".Data"), resd);

    /* copy attributes: colormode */
    PROTECT ( modeSlot = allocVector(INTSXP, 1) );
    nprotect++;
    INTEGER (modeSlot)[0] = colormode;
    SET_SLOT (res, install("colormode"), modeSlot);

    DestroyExceptionInfo(&exception);

    if ( nprotect > 0 )
        UNPROTECT (nprotect);
    return res;
}
