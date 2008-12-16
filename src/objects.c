#include "objects.h"
#include "features_hull.h"

/* -------------------------------------------------------------------------
Counting objects determined in segmentations like watershed
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tools.h"
#include "colors.h"
#include <R_ext/Error.h>
#include <magick/ImageMagick.h>
#include <stdio.h>

/*----------------------------------------------------------------------- */
#define BG 0.0

/*----------------------------------------------------------------------- */
/* will paint features on the target image with given colors and opacs    */
SEXP
paintObjects (SEXP x, SEXP tgt, SEXP _opac, SEXP _col) {
    SEXP res;
    Image * image, * colors;
    PixelPacket * pixelPtr, * colorPtr;
    int nprotect, nx, ny, nz, im, i, j, tgtmode, index;
    double * data, * imdata, * opac;

    double *dx,*dres,dp;
    int redstride,greenstride,bluestride;

    validImage(x,0);
    validImage(tgt,0);

    nx = INTEGER ( GET_DIM(x) )[0];
    ny = INTEGER ( GET_DIM(x) )[1];
    nz = getNumberOfFrames(x,0);
    nprotect = 0;

    PROTECT ( res = Rf_duplicate(tgt) );
    nprotect++;

    tgtmode = getColorMode(tgt);
    opac = REAL (_opac);

    /* will keep colors in a small image -- easier to access - 3 values */
    colors = vector2image1D (_col);
    colorPtr = SetImagePixels (colors, 0, 0, 3, 1);
    for ( i = 0; i < 3; i++ ) {
        colorPtr[i].red *= opac[i];
        colorPtr[i].green *= opac[i];
        colorPtr[i].blue *= opac[i];
    }

    if (tgtmode==MODE_GRAYSCALE || tgtmode==MODE_COLOR) {
       for ( im = 0; im < nz; im++ ) {
	 dx   = &( REAL(x)[ im * nx * ny ] );
	 dres = REAL(res);
	 getColorStrides(tgt,im,&redstride,&greenstride,&bluestride);

	 for ( j = 0; j < ny; j++ ) {
	   for ( i = 0; i < nx; i++ ) {	    

	     /* pixel is contact */
	     index = 1;
	     if ( dx[j*nx + i]<=0 ) continue;
	     if ( dx[j*nx + i] < 1.0 || i < 1 || i > nx - 2 || j < 1 || j > ny - 2 ) index = 2;
	     else {
	       /* check if pixel is border, edge is same as contact */
	       if ( dx[j*nx + i-1] != dx[j*nx + i] ||  dx[j*nx + i+1] != dx[j*nx + i] ||
		    dx[(j-1)*nx + i] != dx[j*nx + i] || dx[(j+1)*nx + i] != dx[j*nx + i]) index = 0;
	     }	  

	     if (redstride!=-1) {
	       dp=dres[redstride+j*nx + i]+((double)colorPtr[index].red)/QuantumRange;		
	       if (dp<0.0) dp=0.0;
	       if (dp>1.0) dp=1.0;
	       dres[redstride+j*nx + i]=dp;
	     }
	     if (greenstride!=-1) {
	       dp=dres[greenstride+j*nx + i]+((double)colorPtr[index].green)/QuantumRange;		
	       if (dp<0.0) dp=0.0;
	       if (dp>1.0) dp=1.0;
	       dres[greenstride+j*nx + i]=dp;
	     }
	     if (bluestride!=-1) {
	       dp=dres[bluestride+j*nx + i]+((double)colorPtr[index].blue)/QuantumRange;	
	       if (dp<0.0) dp=0.0;
	       if (dp>1.0) dp=1.0;
	       dres[bluestride+j*nx + i]=dp;
	     }
	   }
	 }
       }
    } else {
      for ( im = 0; im < nz; im++ ) {
        imdata = &( REAL(x)[ im * nx * ny ] );
        for ( j = 0; j < ny; j++ ) {
	  data = &( REAL(x)[ im * nx * ny + j * nx ] );
	  image = NULL;
	  image = int2image1D ( &(INTEGER(res)[ im * nx * ny + j * nx ]), nx );
	  if ( image == NULL ) continue;
	  for ( i = 0; i < nx; i++ ) {
	    if ( data[i] <= 0 ) continue;
	    pixelPtr = SetImagePixels (image, i, 0, 1, 1);
	    index = 1;
	    if ( data[i] < 1.0 || i < 1 || i > nx - 2 || j < 1 || j > ny - 2 )
	      /* pixel is contact */
	      index = 2;
	    else
	      /* check if pixel is border, edge is same as contact */
	      if ( imdata[ i - 1 + j * nx ] != data[i] || imdata[ i + 1 + j * nx ] != data[i] ||
		   imdata[ i + (j - 1) * nx ] != data[i] || imdata[ i + (j + 1) * nx ] != data[i] )
		index = 0;
	    if ( pixelPtr->red + colorPtr[index].red < QuantumRange )
	      pixelPtr->red += colorPtr[index].red;
	    else
	      pixelPtr->red = QuantumRange;
	    if ( pixelPtr->green + colorPtr[index].green < QuantumRange )
	      pixelPtr->green += colorPtr[index].green;
	    else
	      pixelPtr->green = QuantumRange;
	    if ( pixelPtr->blue + colorPtr[index].blue < QuantumRange )
	      pixelPtr->blue += colorPtr[index].blue;
	    else
	      pixelPtr->blue = QuantumRange;
	  }
	  image1D2int (image, &(INTEGER(res)[ im * nx * ny + j * nx ]), nx );
	  image = DestroyImage (image);
        }
      }
    }

    colors = DestroyImage (colors);

    UNPROTECT (nprotect);
    return res;
}

/*----------------------------------------------------------------------- */
SEXP
matchObjects (SEXP x, SEXP ref) {
    SEXP res, xf, * indexes, ft;
    int nprotect, nx, ny, nz, i, ix, jy, im, nobj;
    double * data, * ftrs;

    validImage(x,0);
    validImage(ref,0);

    nx = INTEGER ( GET_DIM(x) )[0];
    ny = INTEGER ( GET_DIM(x) )[1];
    nz = getNumberOfFrames(x,0);
    nprotect = 0;

    indexes = (SEXP *) R_alloc (nz, sizeof(SEXP) );

    /* we need this to know centres of objects in x */
    PROTECT (xf = lib_basic_hull(x) );
    nprotect++;

    for ( im = 0; im < nz; im++ ) {
        /* get image data */
        data = &( REAL(x)[ im * nx * ny ] );
        /* get number of objects -- max index */
        nobj = 0;
        for ( i = 0; i < nx * ny; i++ )
            if ( data[i] > nobj ) nobj = data[i];
        /* create results vector */
        PROTECT ( indexes[im] = allocVector(INTSXP, nobj) );
        nprotect++;
        if ( nobj < 1 ) continue;
        if ( nz == 1 ) ft = xf;
        else ft = VECTOR_ELT(xf, im);
        if ( ft == R_NilValue ) continue;
        /* check if features correspond to objects */
        ftrs = REAL( ft );
        /* reset data to ref */
        data = &( REAL(ref)[ im * nx * ny ] );

        /* scan through objects, collect indexes */
        for ( i = 0; i < nobj; i++ ) {
            ix = ftrs [i];
            jy = ftrs [i + nobj];
            INTEGER (indexes[im])[i] = NA_INTEGER;
            if ( ix >= 0 && jy >= 0 && ix < nx && jy < ny )
                if ( data[ix + jy * nx] > 0.9 )
                    INTEGER (indexes[im])[i] = (int)data[ix + jy * nx];
        }
    }

    if ( nz > 1 ) {
      PROTECT (res = allocVector(VECSXP, nz) );
      nprotect++;
      for ( im = 0; im < nz; im++ )
        SET_VECTOR_ELT (res, im, indexes[im] );
    }
    else
      res = indexes[0];
    UNPROTECT (nprotect);
    return res;
}

/*----------------------------------------------------------------------- */
SEXP
rmObjects (SEXP x, SEXP _index) {
    SEXP res, index;
    int nprotect, nx, ny, nz, i, j, im, nobj, * indexes, found;
    double * data;

    validImage(x,0);

    nx = INTEGER ( GET_DIM(x) )[0];
    ny = INTEGER ( GET_DIM(x) )[1];
    nz = getNumberOfFrames(x,0);
    nprotect = 0;


    PROTECT ( res = Rf_duplicate(x) );
    nprotect++;
   
    for ( im = 0; im < nz; im++ ) {
        /* get image data */
        data = &( REAL(res)[ im * nx * ny ] );
        index = VECTOR_ELT (_index, im);
        /* get number of objects -- max index */
        nobj = 0;
        for ( i = 0; i < nx * ny; i++ )
            if ( data[i] > nobj ) nobj = data[i];
        indexes = (int *) Calloc (nobj, int );
        for ( i = 0; i < nobj; i++ ) {
            found = 0;
            for ( j = 0; j < LENGTH(index) && !found; j++ )
                if ( i + 1 == INTEGER(index)[j] )
                    found = 1;
            if ( found )
                indexes[i] = 0;
            else
                indexes[i] = i + 1;
        }
        /* shring indexes */
        j = 1;
        for ( i = 0; i < nobj; i++ ) {
            if ( indexes[i] > 0 ) {
                indexes[i] = j;
                j++;
            }
        }
        /* reset image */
        for ( i = 0; i < nx * ny; i++ ) {
            if ( data[i] < 0.9 ) continue;
            data [i] = indexes[ (int)data[i] - 1 ];
        }
        Free (indexes);

    }

    UNPROTECT (nprotect);
    return res;
}

/*----------------------------------------------------------------------- */
SEXP
stackObjects ( SEXP obj, SEXP ref, SEXP hdr, SEXP xy_list, SEXP extension, SEXP rotate ) {
  SEXP res, st, dm, xys;
  int nx, ny, nz, nprotect, im, x, y, i, pxi, nobj, index, dox, doy, ibg = 0, error=0;
  double * data, * xy, xx, yy, xxc, yyc, theta, dbg = 0.0;
  double * dst, * dref; int * ist, * iref; // double or integer reference and stack
  int ext = floor( REAL(extension)[0] );
  int snx = 2 * ext + 1;
  int sny = 2 * ext + 1;
  int rot = INTEGER(rotate)[0];
  int mode = INTEGER ( GET_SLOT(ref, mkString("colormode") ) )[0];
  nx = INTEGER ( GET_DIM(obj) )[0];
  ny = INTEGER ( GET_DIM(obj) )[1];
  nz = getNumberOfFrames(obj,0);
  nprotect = 0;

  if ( nz == 1 ) {
    PROTECT( res = Rf_duplicate(hdr) );
    nprotect++;
  }
  else {
    PROTECT( res = allocVector(VECSXP, nz) );
    nprotect++;
    for ( im = 0; im < nz; im++ ) SET_VECTOR_ELT(res, im, Rf_duplicate(hdr) );
  }

  if ( mode == MODE_TRUECOLOR ) ibg = INTEGER(hdr)[0];
  else dbg = REAL(hdr)[0];

  for ( im = 0; im < nz; im++ ) {
    /* get image data */
    data = &( REAL(obj)[ im * nx * ny ] );
    /* get number of objects -- max index */
    nobj = 0;
    for ( index = 0; index < nx * ny; index++ )
      if ( data[index] > nobj ) nobj = data[index];
    if ( nobj < 1 ) {
      error = 1;
      nobj = 1;
    } else error = 0;
    /* create stack */
    if ( mode == MODE_TRUECOLOR ) {
      PROTECT( st = allocVector(INTSXP, nobj * snx * sny) );
      nprotect++;
      ist = INTEGER( st );
      dst = NULL;
      for ( i = 0; i < nobj * snx * sny; i++ ) ist[i] = ibg;
      iref = &( INTEGER(ref)[im * nx * ny] );
      dref = NULL;
    }
    else {
      PROTECT( st = allocVector(REALSXP, nobj * snx * sny) );
      nprotect++;
      dst = REAL( st );
      ist = NULL;
      for ( i = 0; i < nobj * snx * sny; i++ ) dst[i] = dbg;
      dref = &( REAL(ref)[im * nx * ny] );
      iref = NULL;
    }
    /* set dims on array */
    PROTECT ( dm = allocVector( INTSXP, 3 ) );
    nprotect++;
    INTEGER (dm)[0] = snx;
    INTEGER (dm)[1] = sny;
    INTEGER (dm)[2] = nobj;
    SET_DIM ( st, dm );
    UNPROTECT( 1 ); nprotect--; // dm
    if ( nz == 1 ) res = SET_SLOT(res, install(".Data"), st);
    else SET_VECTOR_ELT(res, im, SET_SLOT(VECTOR_ELT(res, im), install(".Data"), st) );
    UNPROTECT( 1 ); nprotect--; // st
    if ( error == 1 ) continue;
    /* get xy */
    if ( nz == 1 ) xys = xy_list;
    else xys = VECTOR_ELT(xy_list, im);
    if ( xys == R_NilValue || INTEGER(GET_DIM(xys))[0] != nobj || INTEGER(GET_DIM(xys))[1] < 3 ) continue;
    xy = REAL(xys);

    if ( mode == MODE_TRUECOLOR ) {
      if ( nz == 1 )
        ist = INTEGER( res );
      else
        ist = INTEGER( VECTOR_ELT(res, im) );
    }
    else {
      if ( nz == 1 )
        dst = REAL( res );
      else
        dst = REAL( VECTOR_ELT(res, im) );
    }
    /* copy reference data into stack */
    /* unset all non-perimeter points */
    for ( x = 0; x < nx; x++ )
      for ( y = 0; y < ny; y++ ) {
        index = data[x + y * nx]; /* index of the object, R-style = 1-based */
        if ( index < 1 ) continue;
        /* all indexes were 1, 2, 3, but C has 0-based indexes!!! */
        index--;
        /* target frame x, y coordinates */
        xx = x - floor(xy[index]);
        yy = y - floor(xy[index + nobj]);
        dox = 0; doy = 0;
        if ( rot ) {
          theta = -xy[index + 2 * nobj];
          xxc = xx * cos(theta) - yy * sin(theta);
          yyc = xx * sin(theta) + yy * cos(theta);
          xx = floor(xxc);
          yy = floor(yyc);
          // check if we need to mark the adjucent pixels as well
          if ( xxc > xx + 0.5 ) dox = 1; // + as we did floor
          if ( yyc > yy + 0.5 ) doy = 1;
        }
        xx += ext + 1;
        yy += ext + 1;
        if ( xx < 0 || xx >= snx || yy < 0 || yy >= sny ) continue;
        /* put a pixel */
        pxi = xx + yy * snx + index * sny * snx;
        if ( mode == MODE_TRUECOLOR ) ist[ pxi ] = iref[x + y * nx];
        else dst[ pxi ] = dref[x + y * nx];
        if ( !rot ) continue;
        if ( dox && xx + 1 < nx ) {
          pxi = xx + 1 + yy * snx + index * sny * snx;
          if ( mode == MODE_TRUECOLOR ) ist[ pxi ] = iref[x + y * nx];
          else dst[ pxi ] = dref[x + y * nx];
        }
        if ( doy && yy + 1 < ny ) {
          pxi = xx + (yy + 1) * snx + index * sny * snx;
          if ( mode == MODE_TRUECOLOR ) ist[ pxi ] = iref[x + y * nx];
          else dst[ pxi ] = dref[x + y * nx];
        }
      }
  }
  UNPROTECT( nprotect );
  return res;
}
