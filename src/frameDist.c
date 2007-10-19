#include "frameDist.h"

/* -------------------------------------------------------------------------
Pairwise distance between frames
Copyright (c) 2007 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include <R_ext/Error.h>

SEXP distGray(SEXP, SEXP, SEXP);
SEXP distRGB(SEXP, SEXP, SEXP, SEXP);

/* -------------------------------------------------------------------------*/
SEXP
lib_frameDist(SEXP im1, SEXP im2, SEXP weights, SEXP verbose) {
  if (im2==R_NilValue) im2 = im1;
  if (IS_INTEGER(im1) && IS_INTEGER(im2))
    return distRGB(im1, im2, weights, verbose);
  if (IS_NUMERIC(im1) && IS_NUMERIC(im2))
    return distGray(im1, im2, verbose);
  error("non-conform images");
  return R_NilValue;
}

/* -------------------------------------------------------------------------*/
SEXP
distGray(SEXP im1, SEXP im2, SEXP verbose) {
  int i, j, x, nx, ny, nz1, nz2, nprotect=0, verb;
  double *dat1, *dat2, *c, sum, scale;
  SEXP res, dm;
  
  nx =  INTEGER(GET_DIM(im1))[0];
  ny =  INTEGER(GET_DIM(im1))[1];
  nz1 = INTEGER(GET_DIM(im1))[2];
  nz2 = INTEGER(GET_DIM(im2))[2];
  verb = INTEGER(verbose)[0];
  
  if (nx!=INTEGER(GET_DIM(im2))[0] || ny!=INTEGER(GET_DIM(im2))[1])
    error("image sizes differ");

  PROTECT(res=allocVector(REALSXP, nz1*nz2));
  nprotect++;
  c = REAL(res);
  for (x=0; x<nz1*nz2; x++) c[x] = R_PosInf;

  if (verb) Rprintf("%d frames in 'x': ", nz1);
  for (i=0; i<nz1; i++) {
    if (verb) Rprintf("*");
    for (j=0; j<nz2; j++) {
      // if we compare same images, do not take same frame and matrix symmatric
      if (im1==im2)
        if (i==j || c[i+j*nz1]<R_PosInf) continue;
      dat1 = &(REAL(im1)[i*nx*ny]);
      dat2 = &(REAL(im2)[j*nx*ny]);
      sum = 0.0; scale = 0.0;
      for (x=0; x<nx*ny; x++)
        if (dat1[x]!=0.0 || dat2[x]!=0.0) {
          sum += fabs(dat1[x]-dat2[x]);
          scale += 1.0;
        }
      if (scale!=0.0) sum /= scale;
      c[i+j*nz1] = sum;
      if (im1==im2) c[j+i*nz1] = sum;
      R_CheckUserInterrupt();
    }
  }
  if (verb) Rprintf("\n");
  
  PROTECT(dm=allocVector(INTSXP, 2));
  nprotect++;
  INTEGER(dm)[0] = nz1;
  INTEGER(dm)[1] = nz2;
  SET_DIM(res, dm);
  
  UNPROTECT(nprotect);
  return res;
}

/* -------------------------------------------------------------------------*/
inline double distInt(char *x, char *y, double *wgts) {
  double sum = 0.0; 
  int i;
  for (i=0; i<4; i++) sum += wgts[i]*(x[i]-y[i])*(x[i]-y[i]);
  return sqrt(sum)/255.0;
}

/* -------------------------------------------------------------------------*/
SEXP
distRGB(SEXP im1, SEXP im2, SEXP weights, SEXP verbose) {
  int i, j, x, nx, ny, nz1, nz2, nprotect=0, verb;
  int *dat1, *dat2;
  double *c, sum, scale, *wgts;
  SEXP res, dm;
  
  nx =  INTEGER(GET_DIM(im1))[0];
  ny =  INTEGER(GET_DIM(im1))[1];
  nz1 = INTEGER(GET_DIM(im1))[2];
  nz2 = INTEGER(GET_DIM(im2))[2];
  verb = INTEGER(verbose)[0];
  wgts = REAL(weights);

  if (nx!=INTEGER(GET_DIM(im2))[0] || ny!=INTEGER(GET_DIM(im2))[1])
    error("image sizes differ");

  PROTECT(res=allocVector(REALSXP, nz1*nz2));
  nprotect++;
  c = REAL(res);
  for (x=0; x<nz1*nz2; x++) c[x] = R_PosInf;

  if (verb) Rprintf("%d frames in 'x': ", nz1);
  for (i=0; i<nz1; i++) {
    if (verb) Rprintf("*");
    for (j=0; j<nz2; j++) {
      // if we compare same images, do not take same frame and matrix symmatric
      if (im1==im2)
        if (i==j || c[i+j*nz1]<R_PosInf) continue;
      dat1 = &(INTEGER(im1)[i*nx*ny]);
      dat2 = &(INTEGER(im2)[j*nx*ny]);
      sum = 0.0; scale = 0.0;
      for (x=0; x<nx*ny; x++)
        if (dat1[x]!=0 || dat2[x]!=0) {
          sum += distInt((char*)&dat1[x],(char*)&dat2[x],wgts);
          scale += 1.0;
        }
      if (scale!=0.0) sum /= scale;
      c[i+j*nz1] = sum;
      if (im1==im2) c[j+i*nz1] = sum;
      R_CheckUserInterrupt();
    }
  }
  if (verb) Rprintf("\n");
  
  PROTECT(dm=allocVector(INTSXP, 2));
  nprotect++;
  INTEGER(dm)[0] = nz1;
  INTEGER(dm)[1] = nz2;
  SET_DIM(res, dm);
  
  UNPROTECT(nprotect);
  return res;
}
