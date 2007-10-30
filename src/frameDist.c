#include "frameDist.h"

/* -------------------------------------------------------------------------
Pairwise distance between frames
Copyright (c) 2007 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include <R_ext/Error.h>

SEXP distGray(SEXP, SEXP, SEXP, SEXP);
SEXP distRGB(SEXP, SEXP, SEXP, SEXP, SEXP);

/* -------------------------------------------------------------------------*/
SEXP
lib_frameDist(SEXP im1, SEXP im2, SEXP weights, SEXP method, SEXP verbose) {
  if (im2==R_NilValue) im2 = im1;
  if (IS_INTEGER(im1) && IS_INTEGER(im2))
    return distRGB(im1, im2, weights, method, verbose);
  if (IS_NUMERIC(im1) && IS_NUMERIC(im2))
    return distGray(im1, im2, method, verbose);
  error("non-conform images");
  return R_NilValue;
}

/* -------------------------------------------------------------------------*/
SEXP
distGray(SEXP im1, SEXP im2, SEXP method, SEXP verbose) {
  int i, j, x, nx, ny, nz1, nz2, nprotect=0, mthd, verb;
  double *dat1, *dat2, *c, sum;
  SEXP res, dm;
  
  nx =  INTEGER(GET_DIM(im1))[0];
  ny =  INTEGER(GET_DIM(im1))[1];
  nz1 = INTEGER(GET_DIM(im1))[2];
  nz2 = INTEGER(GET_DIM(im2))[2];
  mthd = INTEGER(method)[0];
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
      sum = 0.0; 
      switch (mthd) {
        // dot product
        case 1:
          for (x=0; x<nx*ny; x++) sum += dat1[x]*dat2[x];
          sum /= (double)(nx*ny);
        break;
        // distance
        default:
          for (x=0; x<nx*ny; x++) sum += fabs(dat1[x]-dat2[x]);
          sum /= (double)(nx*ny);
      }
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
SEXP
distRGB(SEXP im1, SEXP im2, SEXP weights, SEXP method, SEXP verbose) {
  int i, j, k, x, nx, ny, nz1, nz2, nprotect=0, mthd, verb;
  unsigned char *dat1, *xx, *dat2, *yy;
  double *c, sum, sum0, *wgts;
  SEXP res, dm;
  
  nx =  INTEGER(GET_DIM(im1))[0];
  ny =  INTEGER(GET_DIM(im1))[1];
  nz1 = INTEGER(GET_DIM(im1))[2];
  nz2 = INTEGER(GET_DIM(im2))[2];
  mthd = INTEGER(method)[0];
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
      dat1 = (unsigned char *)&(INTEGER(im1)[i*nx*ny]);
      dat2 = (unsigned char *)&(INTEGER(im2)[j*nx*ny]);
      sum = 0.0; 
      switch (mthd) {
        // dot product
        case 1:
          for (x=0; x<nx*ny; x++) {
            if (dat1[x]!=0 || dat2[x]!=0) {
              sum0 = 0.0;
              xx = &dat1[4*x];
              yy = &dat2[4*x];
              for (k=0; k<4; k++) sum += wgts[k]*xx[k]*yy[k];
            }
          }
          sum /= (double)(nx*ny*65025.0);
        break;
        // distance
        default:
          for (x=0; x<nx*ny; x++) {
            if (dat1[x]!=0 || dat2[x]!=0) {
              sum0 = 0.0;
              xx = &dat1[4*x];
              yy = &dat2[4*x];
              for (k=0; k<4; k++) sum0 += wgts[k]*(xx[k]-yy[k])*(xx[i]-yy[i]);
              sum += sqrt(sum0);
            }
          }
          sum /= (double)(nx*ny*255.0);
      }
      c[i+j*nz1] = sum;
      if (im1==im2) c[j+i*nz1] = sum;
    }
    R_CheckUserInterrupt();
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
