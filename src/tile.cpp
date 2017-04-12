/* -------------------------------------------------------------------------
Counting objects determined in segmentations like watershed
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tile.h"
#include "tools.h"
#include <R_ext/Error.h>
#include <stdio.h>

using namespace std;

typedef struct {
  int dx, dy;
  int nx, ny;
  int xoff, yoff;
} Dims;

template <class T> void _untile(T *, T *, ColorStrides, ColorStrides, Dims);

/*----------------------------------------------------------------------- */
SEXP tile (SEXP obj, SEXP _hdr, SEXP params) {
  SEXP res, dm, ims;
  int mode =  COLOR_MODE(obj);
  int ndy, ndx = INTEGER(params)[0];
  int lwd = INTEGER(params)[1];
  int nc = getNumberOfChannels(obj, mode);
  int nprotect, nx, ny, nz, nxr, nyr, i, j, index, x, y;
  double *hdr, *tgt;
  double *dsrc = NULL;
  int *isrc = NULL;
  
  ColorStrides src_strides, tgt_strides;
  
  nx = INTEGER ( GET_DIM(obj) )[0];
  ny = INTEGER ( GET_DIM(obj) )[1];
  nz = getNumberOfFrames(obj, 1);
  nprotect = 0;
  
  if ( nz < 1 ) error("no images in stack to tile");
  /* get FG and BG colors from supplied header */
  hdr = REAL(_hdr);
  
  /* calculate size of the resulting image */
  ndy = ceil( nz / (double) ndx ); // number of tiles in y-dir
  nxr = lwd + (nx + lwd) * ndx;
  nyr = lwd + (ny + lwd) * ndy;
  
  /* allocate memory for the image */
  PROTECT( res = allocVector(REALSXP, nc * nxr * nyr) );
  nprotect++;
  DUPLICATE_ATTRIB(res, obj);
  
  // set dimensions
  if (mode!=MODE_COLOR) {
    PROTECT ( dm = allocVector( INTSXP, 2) );
    nprotect++;
  } else {
    PROTECT ( dm = allocVector( INTSXP, 3) );
    nprotect++;
    INTEGER (dm)[2] = nc;
  }
  INTEGER (dm)[0] = nxr;
  INTEGER (dm)[1] = nyr;
  
  SET_DIM(res, dm);
  SET_DIMNAMES (res, R_NilValue);
  
  tgt = REAL(res);
  
  getColorStrides(res, 0, &tgt_strides);
  
  /* loop through image stack and copy them to res */  
  for ( index = 0; index < ndx * ndy; index++ ) {
    y = lwd + (index / ndx) * (ny + lwd);
    x = lwd + (index % ndx) * (nx + lwd);
    i = x + y * nxr;
    
    /*  copy line by line */
    if (index < nz) {
      getColorStrides(obj, index, &src_strides);
      
      switch (TYPEOF(obj)) {
      case LGLSXP:
      case INTSXP:
        isrc = INTEGER(obj);
        for (j = 0; j < nx*ny; j+=nx, i+=nxr ) {
          for (int k = 0; k < nx; k++) {
            if (tgt_strides.r!=-1) tgt[tgt_strides.r + i + k] = (double) isrc[j+src_strides.r + k];
            if (tgt_strides.g!=-1) tgt[tgt_strides.g + i + k] = (double) isrc[j+src_strides.g + k];
            if (tgt_strides.b!=-1) tgt[tgt_strides.b + i + k] = (double) isrc[j+src_strides.b + k];
          }
        } 
        break;
      case REALSXP:
        dsrc = REAL(obj);
        for (j = 0; j < nx*ny; j+=nx, i+=nxr ) {
          if (src_strides.r!=-1) memcpy( &(tgt[i+tgt_strides.r]), &(dsrc[j+src_strides.r]), nx * sizeof(double));
          if (src_strides.g!=-1) memcpy( &(tgt[i+tgt_strides.g]), &(dsrc[j+src_strides.g]), nx * sizeof(double));
          if (src_strides.b!=-1) memcpy( &(tgt[i+tgt_strides.b]), &(dsrc[j+src_strides.b]), nx * sizeof(double));
        }
        break;
      }
      
    }
    /* reset remaining empty tiles to BG */
    else {
      for ( j = 0; j < ny; j++, i+=nxr ) {
        for (int k = 0; k < nx; k++){
          if (tgt_strides.r!=-1) tgt[tgt_strides.r + i + k] = hdr[1];
          if (tgt_strides.g!=-1) tgt[tgt_strides.g + i + k] = hdr[3];
          if (tgt_strides.b!=-1) tgt[tgt_strides.b + i + k] = hdr[5];
        }
      }
    }
  }
  
  /* draw grid if required */  
  if ( lwd > 0 ) {
    /* vertical stripes */
    for (i = 0; i <= ndx; i++ ) {
      for ( x = i * (nx + lwd); x < lwd + i * (nx + lwd); x++ ) {
        for ( y = 0; y < nyr; y++ ) {
          if (tgt_strides.r!=-1) tgt[tgt_strides.r + x + y * nxr] = hdr[0];
          if (tgt_strides.g!=-1) tgt[tgt_strides.g + x + y * nxr] = hdr[2];
          if (tgt_strides.b!=-1) tgt[tgt_strides.b + x + y * nxr] = hdr[4];
        }        
      }
    }
    /* horizontal stripes */
    for (j = 0; j <= ndy; j++ ) {
      for ( y = j * (ny + lwd); y < lwd + j * (ny + lwd); y++ ) {
        for ( x = 0; x < nxr; x++ ) {
          if (tgt_strides.r!=-1) tgt[tgt_strides.r + x + y * nxr] = hdr[0];
          if (tgt_strides.g!=-1) tgt[tgt_strides.g + x + y * nxr] = hdr[2];
          if (tgt_strides.b!=-1) tgt[tgt_strides.b + x + y * nxr] = hdr[4];
        }
      }
    }
  }
  
  UNPROTECT( nprotect );
  return res;
}

/*----------------------------------------------------------------------- */

SEXP
untile(SEXP img, SEXP nim, SEXP linewd) {
  int mode = COLOR_MODE(img);
  int nimx = INTEGER(nim)[0];
  int nimy = INTEGER(nim)[1];

  int lwd  = INTEGER(linewd)[0];
  int *sdim = INTEGER(GET_DIM(img));
  int nx = (sdim[0]-(nimx+1)*lwd) / nimx;
  int ny = (sdim[1]-(nimy+1)*lwd) / nimy;
  int nz = getNumberOfFrames(img,1) * nimx * nimy;
  int nc = getNumberOfChannels(img, mode);
  int nprotect=0, i, j, im, y, iim;
  SEXP res, dim, dat;
  double *src, *tgt;

  ColorStrides src_strides, res_strides;
  
  Dims d = (Dims) {dx: sdim[0], dy: sdim[1], nx: nx, ny: ny, xoff: 0, yoff: 0};

  if (nx<1 || ny<1 || nz <1 || ((nx*ny*nz*nc)>(1024*1024*1024))) {
    if (nc==1) Rprintf("size of the resulting image will be (nx=%d,ny=%d,nz=%d)\n",nx,ny,nz);
    else Rprintf("size of the resulting image will be (nx=%d,ny=%d,nc=%d,nz=%d)\n",nx,ny,nc,nz);
    error("invalid nx, ny or nz values: negative or too large values");
  }

  PROTECT(res = allocVector(TYPEOF(img), nc*nx*ny*nz)); 
  nprotect++;
  DUPLICATE_ATTRIB(res, img);
  
  // set dimensions
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
  SET_DIM(res, dim);
  SET_DIMNAMES (res, R_NilValue);
  
  for (im=0; im<nz; im++) {
    iim = im / (nimx*nimy);

    getColorStrides(img, iim, &src_strides);
    getColorStrides(res, im,  &res_strides);
   
    i = im % nimx;
    j = (im-iim*nimx*nimy) / nimx;

    d.xoff = i*nx+lwd*(i+1);                                                                                      
    d.yoff = j*ny+lwd*(j+1);                                                                                      
 
    switch (TYPEOF(img)) {
    case LGLSXP:
    case INTSXP:
      _untile<int>(INTEGER(img), INTEGER(res), src_strides, res_strides, d);
      break;
    case REALSXP:
      _untile<double>(REAL(img), REAL(res), src_strides, res_strides, d);
      break;
    }
    
  }
  
  UNPROTECT(nprotect);
  return res;
}

template <class T> void _untile(T *src, T *tgt, ColorStrides src_strides, ColorStrides tgt_strides, Dims d) { 
  for (int y=0; y<d.ny; y++) {
    if (src_strides.r!=-1) memcpy(&(tgt[tgt_strides.r + y*d.nx]), &(src[src_strides.r + (d.yoff + y)*d.dx + d.xoff]), d.nx*sizeof(T));
    if (src_strides.g!=-1) memcpy(&(tgt[tgt_strides.g + y*d.nx]), &(src[src_strides.g + (d.yoff + y)*d.dx + d.xoff]), d.nx*sizeof(T));
    if (src_strides.b!=-1) memcpy(&(tgt[tgt_strides.b + y*d.nx]), &(src[src_strides.b + (d.yoff + y)*d.dx + d.xoff]), d.nx*sizeof(T));
  }
}
