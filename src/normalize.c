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
normalize (SEXP x, SEXP separate, SEXP outrange, SEXP inrange) {
  int nprotect, nx, ny, nz, im, i, sep;
  double * data, from, to, min, max, diff;
  SEXP res;

  nprotect = 0;

  from = REAL(outrange)[0];
  to   = REAL(outrange)[1];
  sep  = INTEGER(separate)[0];
  min  = REAL(inrange)[0];
  max  = REAL(inrange)[1];

  nx = INTEGER (GET_DIM(x))[0];
  ny = INTEGER (GET_DIM(x))[1];
  nz = getNumberOfFrames(x,0);

  PROTECT ( res = Rf_duplicate(x) );
  nprotect++;

  data = REAL(res);

  diff  = max - min;  
  if(diff == 0){ // auto-calculate intensity range
    if ( !sep ) {
      min = DBL_MAX;
      max = -DBL_MAX;
      for ( i = 0; i < nx * ny * nz; i++ ) {
        if ( data[i] < min ) min = data[i];
        if ( data[i] > max ) max = data[i];
      }
      diff = max - min;
      if ( diff == 0 )
        warning( "image can not be normalized, its diff(range) is 0" );
    }
  }
  else{
    sep = 0; // do not process single frames separately
    // clip values outside specified range
    for ( i = 0; i < nx * ny * nz; i++ ) {
        if ( data[i] < min ) data[i] = min;
        if ( data[i] > max ) data[i] = max;
      }
  }
  
  // normalize only if normalization range is avlid
  if ( (to-from)!=0 && (diff!=0 || sep) ) {  
    for ( im = 0; im < nz; im++ ) {
      data = &( REAL(res)[ im * nx * ny ] );
      if ( sep ) {
        min = DBL_MAX;
        max = -DBL_MAX;
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
  }
  
  UNPROTECT (nprotect);
  return res;
}
