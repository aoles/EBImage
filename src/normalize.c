#include "normalize.h"
#include "tools.h"

/* -------------------------------------------------------------------------
Frame-based image normalization

Copyright (c) 2007 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include <R_ext/Error.h>

/*----------------------------------------------------------------------- */

double min, max, diff = 0;

void range (double *src, double *tgt, int n, int frame) {
  int i;
  char *msg;
  min = DBL_MAX;
  max = -DBL_MAX;
  for ( i = 0; i < n; i++ ) {
    if ( src[i] < min ) min = src[i];
    if ( src[i] > max ) max = src[i];
  }
  diff = max - min;
  if ( diff == 0 ) {
    memcpy(tgt, src, n * sizeof(double));
    msg = "cannot be normalized as its diff(range) is 0";
    if ( frame==-1 )
      warning("image %s", msg);
    else
      warning("frame %d %s", frame, msg);
  }
}

SEXP
normalize (SEXP x, SEXP separate, SEXP outrange, SEXP inrange) {
  int nprotect, nx, ny, nz, im, i, sep;
  double *src, *tgt, from, to;
  SEXP res;

  nprotect = 0;

  sep = LOGICAL(separate)[0];
  
  nx = INTEGER (GET_DIM(x))[0];
  ny = INTEGER (GET_DIM(x))[1];
  nz = getNumberOfFrames(x,0);
  
  PROTECT( res = allocVector(REALSXP, XLENGTH(x)) );
  nprotect++;
  DUPLICATE_ATTRIB(res, x);
  
  src = REAL(x);
  tgt = REAL(res);
  
  from = to = 0;
  
  if ( inrange != R_NilValue ) {
    min  = REAL(inrange)[0];
    max  = REAL(inrange)[1];
    for ( i = 0; i < nx * ny * nz; i++ ) {
      tgt[i] = ( src[i] < min ) ? min : src[i];
      tgt[i] = ( src[i] > max ) ? max : src[i];
    }
  }
  else if ( !sep ) {
    range(src, tgt, nx*ny*nz, -1);
  }

  // normalize only if normalization range is valid
  if ( outrange != R_NilValue && (diff!=0 || sep) ) {
    from = REAL(outrange)[0];
    to   = REAL(outrange)[1];
  
    for ( im = 0; im < nz; im++ ) {
      src = &( REAL(x)[ im * nx * ny ] );
      tgt = &( REAL(res)[ im * nx * ny ] );
      if ( sep ) 
        range(src, tgt, nx * ny, im+1);
      if ( diff != 0 ) {
        for ( i = 0; i < nx * ny; i++ )
          tgt[i] = ( src[i] - min ) / diff * (to - from) + from;
      }
    }
  }
  UNPROTECT (nprotect);
  return res;
}
