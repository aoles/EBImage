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
inline double d_cor(double *x, double *y, double nxny) {
  double mx, my, sdx, sdy, dx, dy, sum=0.0;
  int k;
  mx = 0.0;
  my = 0.0;
  for (k=0; k<nxny; k++) {
    mx += x[k];
    my += y[k];
  }
  mx /= nxny;
  my /= nxny;
  sdx = 0.0;
  sdy = 0.0;
  for (k=0; k<nxny; k++) {
    dx = x[k]-mx;
    dy = y[k]-my;
    sum += dx*dy;
    sdx += dx*dx;
    sdy += dy*dy;
  }          
  sdx = sqrt(sdx/(nxny-1.0));
  sdy = sqrt(sdy/(nxny-1.0));
  sum /= sdx*sdy*(nxny-1.0);
  return sum;
}

/* -------------------------------------------------------------------------*/
SEXP
distGray(SEXP im1, SEXP im2, SEXP method, SEXP verbose) {
  int i, j, k, x, nx, ny, nz1, nz2, nprotect=0, mthd, verb;
  double *dat1, *dat2, *c, sum, nxny;
  SEXP res, dm;
  
  nx =  INTEGER(GET_DIM(im1))[0];
  ny =  INTEGER(GET_DIM(im1))[1];
  nxny = nx*ny;
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
      if (im1==im2 && c[i+j*nz1]<R_PosInf) continue;
      dat1 = &(REAL(im1)[i*nx*ny]);
      dat2 = &(REAL(im2)[j*nx*ny]);
      switch (mthd) {
        // dot product
        case 1:
          sum = 0.0; 
          for (k=0; k<nxny; k++) sum += dat1[k]*dat2[k];
          sum /= nxny;
          break;
        // Pearson correlation
        case 2:
          sum = d_cor(dat1, dat2, nxny);
          break;
        // distance
        default:
          sum = 0.0; 
          for (k=0; k<nxny; k++) sum += fabs(dat1[k]-dat2[k]);
          sum /= nxny;
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

/* -------------------------------------------------------------------------*/
// compute dot product between two integer images
inline double i_dot(unsigned char *x, unsigned char *y, double nxny, double *wgts) {
  int k, l;
  double sum=0.0;
  for (k=0; k<nxny; k++)
    for (l=0; l<4; l++) sum += wgts[l]*x[4*k+l]*y[4*k+l];
  sum /= nxny*65025.0;
  return sum;
}

/* -------------------------------------------------------------------------*/
// compute correlation between two integer images
inline double i_cor(unsigned char *x, unsigned char *y, double nxny, double *wgts) {
  int k, l;
  double dx, dy, mx[3], my[3], sdx[3], sdy[3], suml[3], sum=0.0;
  for (l=0; l<3; l++) {
    mx[l] = 0.0;
    my[l] = 0.0;
    suml[l] = 0.0;
  }
  for (k=0; k<nxny; k++)
    for (l=0; l<3; l++) {
      mx[l] += x[4*k+l];
      my[l] += y[4*k+l];
    }
  for (l=0; l<3; l++) {
    mx[l] /= nxny;
    my[l] /= nxny;
    sdx[l] = 0.0;
    sdy[l] = 0.0;
  }
  for (k=0; k<nxny; k++)
    for (l=0; l<3; l++) {
      dx = x[4*k+l]-mx[l];
      dy = y[4*k+l]-my[l];
      suml[l] += dx*dy;
      sdx[l] += dx*dx;
      sdy[l] += dy*dy;
    }
  for (l=0; l<3; l++) {
    sdx[l] = sqrt(sdx[l]/(nxny-1.0));
    sdy[l] = sqrt(sdy[l]/(nxny-1.0));
    suml[l] /= sdx[l]*sdy[l]*(nxny-1.0);
    sum += wgts[l]*suml[l];
  }
  sum /= 3.0;
  return sum;
}

/* -------------------------------------------------------------------------*/
// compute distance between two integer images
inline double i_dist(unsigned char *x, unsigned char *y, double nxny, double *wgts) {
  int k, l, i;
  double sum=0.0, sum0;
  for (k=0; k<nxny; k++) {
    sum0 = 0.0;
    for (l=0; l<4; l++) {
      i = 4*k+l;
      sum0 += wgts[l]*(x[i]-y[i])*(x[i]-y[i]);
    }
    sum += sqrt(sum0);
  }
  sum /= nxny*255.0;
  return sum;
}

/* -------------------------------------------------------------------------*/
SEXP
distRGB(SEXP im1, SEXP im2, SEXP weights, SEXP method, SEXP verbose) {
  int i, j, nx, ny, nz1, nz2, nprotect=0, mthd, verb;
  unsigned char *dat1, *dat2;
  double *c, sum, *wgts, nxny;
  SEXP res, dm;
  
  nx =  INTEGER(GET_DIM(im1))[0];
  ny =  INTEGER(GET_DIM(im1))[1];
  nxny = nx*ny;
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
  for (i=0; i<nz1*nz2; i++) c[i] = R_PosInf;

  if (verb) Rprintf("%d frames in 'x': ", nz1);
  for (i=0; i<nz1; i++) {
    if (verb) Rprintf("*");
    for (j=0; j<nz2; j++) {
      if (im1==im2 && c[i+j*nz1]<R_PosInf) continue;
      dat1 = (unsigned char *)&(INTEGER(im1)[i*nx*ny]);
      dat2 = (unsigned char *)&(INTEGER(im2)[j*nx*ny]);
      switch (mthd) {
        // dot product
        case 1:
          sum = i_dot(dat1, dat2, nxny, wgts);
          break;
        // Pearson correlation
        case 2:
          sum = i_cor(dat1, dat2, nxny, wgts);
          break;
        // distance
        default:
          sum = i_dist(dat1, dat2, nxny, wgts);
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
