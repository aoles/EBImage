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

    if ( !isImage(x) || !isImage(tgt) ) return tgt;

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
	  if ( tgtmode == MODE_TRUECOLOR )
	    image = int2image1D ( &(INTEGER(res)[ im * nx * ny + j * nx ]), nx );
	  if ( tgtmode == MODE_GRAYSCALE )
	    image = double2image1D ( &(REAL(res)[ im * nx * ny + j * nx ]), nx );
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
	  if ( tgtmode == MODE_TRUECOLOR )
	    image1D2int (image, &(INTEGER(res)[ im * nx * ny + j * nx ]), nx );
	  if ( tgtmode == MODE_GRAYSCALE )
	    image1D2double (image, &(REAL(res)[ im * nx * ny + j * nx ]), nx );
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

    if ( !isImage(x) || !isImage(ref) ) return x;

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

    if ( !isImage(x) ) return x;

    nx = INTEGER ( GET_DIM(x) )[0];
    ny = INTEGER ( GET_DIM(x) )[1];
    nz = getNumberOfFrames(x,0);
    nprotect = 0;


    PROTECT ( res = Rf_duplicate(x) );
    nprotect++;
    SET_CLASS (res, mkString("IndexedImage") );

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

/*----------------------------------------------------------------------- */
SEXP
tile (SEXP obj, SEXP hdr, SEXP params) {
  SEXP res, dm, ims;
  int mode =  getColorMode(obj);
  int ndy, ndx  = INTEGER(params)[0];
  int lwd = INTEGER(params)[1];
  int nc= getNumberOfChannels(obj);
  int nprotect, nx, ny, nz, ifg, ibg, nxr, nyr, * iim, i, j, index, x, y;
  double dfg, dbg, * dim, onetondx;
  int rredstride,rgreenstride,rbluestride;
  int oredstride,ogreenstride,obluestride;

  nx = INTEGER ( GET_DIM(obj) )[0];
  ny = INTEGER ( GET_DIM(obj) )[1];
  nz = getNumberOfFrames(obj,1);
  nprotect = 0;

  if ( nz < 1 ) error("no images in stack to tile");
  /* get FG and BG colors from supplied header */
  if ( mode == MODE_TRUECOLOR ) {
    ifg = INTEGER(hdr)[0]; dfg = 0.0;
    ibg = INTEGER(hdr)[1]; dbg = 0.0;
  }
  else {
    dfg = REAL(hdr)[0]; ifg = 0.0;
    dbg = REAL(hdr)[1]; ibg = 0.0;
  }

  /* calculate size of the resulting image */
  onetondx = 1.0 / (double)ndx;
  ndy = ceil( nz * onetondx ); // number of tiles in y-dir
  nxr = lwd + (nx + lwd) * ndx;
  nyr = lwd + (ny + lwd) * ndy;

  /* allocate memory for the image, reset to BG */
  if ( mode == MODE_TRUECOLOR ) {
    PROTECT( ims = allocVector(INTSXP, nxr * nyr) );
    nprotect++;
    iim = INTEGER(ims); dim = NULL;
    for ( i = 0; i < nxr * nyr; i++ ) iim[i] = ibg;
  }
  else {
    PROTECT( ims = allocVector(REALSXP, nc*nxr * nyr) );
    nprotect++;
    dim = REAL(ims); iim = NULL;
    for ( i = 0; i < nc*nxr * nyr; i++ ) dim[i] = dbg;
  } 
  
  // make res final object
  if (mode!=MODE_COLOR) {
    PROTECT ( dm = allocVector( INTSXP, 2) );
    nprotect++;
    INTEGER (dm)[0] = nxr;
    INTEGER (dm)[1] = nyr;
  } else {
    PROTECT ( dm = allocVector( INTSXP, 3) );
    nprotect++;
    INTEGER (dm)[0] = nxr;
    INTEGER (dm)[1] = nyr;
    INTEGER (dm)[2] = nc;
  }
  SET_DIM ( ims, dm ) ;

  /* create resulting image from header */
  PROTECT( res = Rf_duplicate(hdr) );
  nprotect++;
  res = SET_SLOT( res, install(".Data"), ims );

  /* loop through stack image and copy them to ims */
  for ( index = 0; index < nz; index++ ) {
    if (mode!=MODE_TRUECOLOR) {
      getColorStrides(obj,index,&oredstride,&ogreenstride,&obluestride);
      getColorStrides(res,0,&rredstride,&rgreenstride,&rbluestride);
    }
  
    /* loop through lines and copy by line */
    for ( j = 0; j < ny; j++ ) {
      y = lwd + floor(index * onetondx) * (ny + lwd) + j;
      x = lwd + (index - floor(index * onetondx) * ndx) * (nx + lwd);
      i = x + y * nxr;
      if ( i + nx >= nxr * nyr ) {
        warning("BAD THING HAPPEND -- WRONG INDEX CALCULATION");
        continue;
      }
      if ( mode == MODE_TRUECOLOR ) memcpy( &(iim[i]), &(INTEGER(obj)[(j + index * ny) * nx]), nx * sizeof(int));
      else {
	if (oredstride!=-1)   memcpy( &(dim[i+rredstride]), &(REAL(obj)[j* nx+oredstride]), nx * sizeof(double));
	if (ogreenstride!=-1) memcpy( &(dim[i+rgreenstride]), &(REAL(obj)[j* nx+ogreenstride]), nx * sizeof(double));
	if (obluestride!=-1)  memcpy( &(dim[i+rbluestride]), &(REAL(obj)[j* nx+obluestride]), nx * sizeof(double));
      }
    }
  }
  /* draw grid if required */
  if ( lwd > 0 && (dfg != dbg || ifg != dbg) ) {
    /* vertical stripes */
    for (i = 0; i <= ndx; i++ ) {
      for ( x = i * (nx + lwd); x < lwd + i * (nx + lwd); x++ ) {
        if ( mode == MODE_TRUECOLOR )
          for ( y = 0; y < nyr; y++ ) iim[x + y * nxr] = ifg;
        else
          for ( y = 0; y < nyr; y++ ) dim[x + y * nxr] = dfg;
      }
    }
    /* horizontal stripes */
    for (j = 0; j <= ndy; j++ ) {
      for ( y = j * (ny + lwd); y < lwd + j * (ny + lwd); y++ ) {
        if ( mode == MODE_TRUECOLOR )
          for ( x = 0; x < nxr; x++ ) iim[x + y * nxr] = ifg;
        else
          for ( x = 0; x < nxr; x++ ) dim[x + y * nxr] = dfg;
      }
    }
  }

  res = SET_SLOT( res, install(".Data"), ims );
 
  UNPROTECT( nprotect );
  return res;
}

/*----------------------------------------------------------------------- */
SEXP
untile(SEXP img, SEXP hdr, SEXP nim, SEXP linewd) {
  int mode = getColorMode(img);
  int nimx = INTEGER(nim)[0];
  int nimy = INTEGER(nim)[1];

  int lwd  = INTEGER(linewd)[0];
  int *sdim = INTEGER(GET_DIM(img));
  int nx = (sdim[0]-(nimx+1)*lwd) / nimx;
  int ny = (sdim[1]-(nimy+1)*lwd) / nimy;
  int nz = getNumberOfFrames(img,1) * nimx * nimy;
  int nc = getNumberOfChannels(img);
  int nprotect=0, i, j, im, y, iim;
  SEXP res, dim, dat;
  void *src, *tgt; double *dd; int *id;

  int rredstride,rgreenstride,rbluestride;
  int oredstride,ogreenstride,obluestride;

  if (nx<1 || ny<1 || nz <1 || ((nx*ny*nz*nc)>(1024*1024*1024))) {
    if (nc==1) Rprintf("size of the resulting image will be (nx=%d,ny=%d,nz=%d)\n",nx,ny,nz);
    else Rprintf("size of the resulting image will be (nx=%d,ny=%d,nc=%d,nz=%d)\n",nx,ny,nc,nz);
    error("invalid nx, ny or nz values: negative or too large");
  }

  if (mode==MODE_TRUECOLOR) {
    PROTECT(dat = allocVector(INTSXP, nx*ny*nz)); 
    nprotect++;
    id = INTEGER(dat);
    for (i=0; i<nx*ny*nz; i++) id[i] = 0.0;
  } else  {
    PROTECT(dat = allocVector(REALSXP, nc*nx*ny*nz)); 
    nprotect++;
    dd = REAL(dat);
    for (i=0; i<nc*nx*ny*nz; i++) dd[i] = 0.0;
  }

  if (mode!=MODE_COLOR) {
    PROTECT(dim = allocVector(INTSXP, 3)); nprotect++;
    INTEGER(dim)[0] = nx;
    INTEGER(dim)[1] = ny;
    INTEGER(dim)[2] = nz;
  } else {
    PROTECT(dim = allocVector(INTSXP, 4)); nprotect++;
    INTEGER(dim)[0] = nx;
    INTEGER(dim)[1] = ny;
    INTEGER(dim)[2] = nc;
    INTEGER(dim)[3] = nz;
  }
  SET_DIM(dat, dim);
  res = SET_SLOT(Rf_duplicate(hdr), install(".Data"), dat);

  for (im=0; im<nz; im++) {
    iim = im / (nimx*nimy);

    if (mode!=MODE_TRUECOLOR) {
      getColorStrides(img,iim,&oredstride,&ogreenstride,&obluestride);
      getColorStrides(res,im,&rredstride,&rgreenstride,&rbluestride);
    }
   
    i = im % nimx;
    j = (im-iim*nimx*nimy) / nimx;

    if (mode==MODE_TRUECOLOR) {
      for (y=0; y<ny; y++) {
        src = &(INTEGER(img)[iim*sdim[0]*sdim[1] + (j*ny+lwd*(j+1) + y)*sdim[0] + (i*nx+lwd*(i+1))]);
        tgt = &(INTEGER(dat)[im*nx*ny + y*nx]);
        memcpy(tgt, src, nx*sizeof(int));
      }
    } else {
      for (y=0; y<ny; y++) {
	if (oredstride!=-1) {
	  src = &(REAL(img)[oredstride + (j*ny+lwd*(j+1) + y)*sdim[0] + (i*nx+lwd*(i+1))]);
	  tgt=&(REAL(dat)[rredstride + y*nx]);
	  memcpy(tgt, src, nx*sizeof(double));
	}
	if (ogreenstride!=-1) {
	  src = &(REAL(img)[ogreenstride + (j*ny+lwd*(j+1) + y)*sdim[0] + (i*nx+lwd*(i+1))]);
	  tgt=&(REAL(dat)[rgreenstride + y*nx]);
	  memcpy(tgt, src, nx*sizeof(double));
	}
	if (obluestride!=-1) {
	  src = &(REAL(img)[obluestride+ (j*ny+lwd*(j+1) + y)*sdim[0] + (i*nx+lwd*(i+1))]);
	  tgt=&(REAL(dat)[rbluestride + y*nx]);
	  memcpy(tgt, src, nx*sizeof(double));
	}
      }
    }
  }
  res = SET_SLOT(Rf_duplicate(hdr), install(".Data"), dat);
  UNPROTECT(nprotect);
  return res;
}






