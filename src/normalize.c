#include "normalize.h"
#include "tools.h"

/* -------------------------------------------------------------------------
Frame-based image normalization

Copyright (c) 2007 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include <R_ext/Error.h>

/*----------------------------------------------------------------------- */
SEXP
normalize (SEXP x, SEXP separate, SEXP range) {
  int nprotect, nx, ny, nz, im, i, sep;
  double * data, from, to, min, max, diff;
  SEXP res;

  nprotect = 0;

  from = REAL(range)[0];
  to   = REAL(range)[1];
  sep = INTEGER(separate)[0];

  nx = INTEGER (GET_DIM(x))[0];
  ny = INTEGER (GET_DIM(x))[1];
  nz = getNumberOfFrames(x,0);

  PROTECT ( res = Rf_duplicate(x) );
  nprotect++;

  diff = 0;
  min = 1e16;
  max = -1e16;
  if ( !sep ) {
    data = REAL(res);
    for ( i = 0; i < nx * ny * nz; i++ ) {
      if ( data[i] < min ) min = data[i];
      if ( data[i] > max ) max = data[i];
    }
    diff = max - min;
    if ( diff == 0 )
      warning( "image can not be normalized, its diff(range) is 0" );
  }

  for ( im = 0; im < nz; im++ ) {
    data = &( REAL(res)[ im * nx * ny ] );
    if ( sep ) {
      min = 1e16;
      max = -1e16;
      for ( i = 0; i < nx * ny; i++ ) {
        if ( data[i] < min ) min = data[i];
        if ( data[i] > max ) max = data[i];
      }
      diff = max - min;
      if ( diff == 0 ) {
        warning( "frame can not be normalized, its diff(range) is 0" );
        continue;
      }        
    }
    for ( i = 0; i < nx * ny; i++ )
      data[i] = ( data[i] - min ) / diff * (to - from) + from;
  }

  UNPROTECT (nprotect);
  return res;
}
