#include "tile.h"

/* -------------------------------------------------------------------------
Counting objects determined in segmentations like watershed
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tools.h"
#include <R_ext/Error.h>
#include <magick/ImageMagick.h>
#include <stdio.h>

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
  PROTECT( res = Rf_duplicate(obj) );
  nprotect++;
  if (strcmp( CHAR( asChar( GET_CLASS(obj) ) ), "Image") == 0) {
    res = SET_SLOT( res, install(".Data"), ims );
  } else res=ims;
  

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

  if (strcmp( CHAR( asChar( GET_CLASS(obj) ) ), "Image") == 0) {
    res = SET_SLOT( res, install(".Data"), ims );
  }
 
  UNPROTECT( nprotect );
  return res;
}

/*----------------------------------------------------------------------- */
SEXP
untile(SEXP img, SEXP nim, SEXP linewd) {
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
    error("invalid nx, ny or nz values: negative or too large values");
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
  if (strcmp( CHAR( asChar( GET_CLASS(img) ) ), "Image") == 0) {
    res = SET_SLOT(Rf_duplicate(img), install(".Data"), dat);
  } else res=dat;

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
  if (strcmp( CHAR( asChar( GET_CLASS(img) ) ), "Image") == 0) {
    res = SET_SLOT(res, install(".Data"), dat);
  }
  UNPROTECT(nprotect);
  return res;
}






