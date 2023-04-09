#include "normalize.h"
#include "tools.h"

#include <stdint.h>
#include <R_ext/Error.h>

/* -------------------------------------------------------------------------
as.nativeRaster C implementation

Copyright (c) 2015 Andrzej Oles
------------------------------------------------------------------------- */

SEXP nativeRaster (SEXP _in) {
  int nprotect = 0, x, y, c, i, j;
  uint32_t *out;
  SEXP res, nch;
  
  x = INTEGER (GET_DIM(_in))[0];
  y = INTEGER (GET_DIM(_in))[1];
  c = COLOR_MODE(_in);
  
  // pointers to color channels
  double *rgba[4] = {NULL, NULL, NULL, NULL};
  
  if (c != MODE_COLOR)
    rgba[0] = rgba[1] = rgba[2] = REAL(_in);
  else {
    int nc = getNumberOfChannels(_in, c);
    if (nc > 4) nc = 4;
    for(int i = 0; i < nc; i++)
      rgba[i] = &( REAL(_in)[ i * x * y ] );
  }
  
  res = PROTECT( allocMatrix(INTSXP, y, x) );
  nprotect++;
  
  // initialize
  out = (uint32_t *) INTEGER(res);
  memset (out, 0, x*y*sizeof(uint32_t) );
  
  for(j=0; j<4; j++) {
    double *data = rgba[j];
    int mul = (int) pow(2, j*8);
    
    for (i=0; i<x*y; i++) {
      int nel = 0; //zero fill empty channels      
      
      if (data!=NULL) {        
        double el = data[i];
        // clip
        if (el < 0.0) el = 0;
        else if (el > 1.0) el = 1.0;
        // cast to integer representation
        nel = (int) round(el * 255);
      }
      // full opaque if alpha channel missing
      else if (j==3) 
        nel = 255;
      
      out[i] += nel * mul;
    }
  }
  
  //set class
  setAttrib(res, R_ClassSymbol, mkString("nativeRaster"));
  nch = PROTECT( ScalarInteger(4) ); nprotect++;
  setAttrib(res, install("channels"), nch);
  
  UNPROTECT (nprotect);
  
  return res;
}
