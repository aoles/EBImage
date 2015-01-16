#include "normalize.h"
#include "tools.h"

/* -------------------------------------------------------------------------
as.nativeRaster C implementation

Copyright (c) 2015 Andrzej Oles
------------------------------------------------------------------------- */

#include <R_ext/Error.h>

SEXP nativeRaster (SEXP _in) {
  int nprotect = 0, x, y, c, i, j;
  double *in;
  int *out;
  SEXP res, class, channels;

  x = INTEGER (GET_DIM(_in))[0];
  y = INTEGER (GET_DIM(_in))[1];
  c = getColorMode(_in);

  // pointers to color channels
  double *rgba[4] = {NULL, NULL, NULL, NULL};
  
  if (c != MODE_COLOR)
    rgba[0] = rgba[1] = rgba[2] = REAL(_in);
  else 
    for(int i = getNumberOfChannels(_in)-1; i>=0; i--)
      rgba[i] = &( REAL(_in)[ i * x * y ] );
    
  res = PROTECT( allocMatrix(INTSXP, y, x) );
  nprotect++;
  
  // initialize
  out = INTEGER(res);
  memset (out, 0, x*y*sizeof(int) );
    
  for(j=0; j<4; j++) {
    double *data = rgba[j];
    int mul = (int) pow(2, j*8); //this could be probably further optimized by performing byte shifts
        
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
  
  for (i=0; i<x*y; i++) out[i] -= 4294967296;
  
  //set class 
  class = PROTECT( allocVector(STRSXP, 1) );
  nprotect++;
  SET_STRING_ELT(class, 0, mkChar("nativeRaster"));
  classgets(res, class);
  
  channels = PROTECT(allocVector(INTSXP, 1));
  nprotect++;
  INTEGER(channels)[0] = 4;
  setAttrib(res, install("channels"), channels);
  
  UNPROTECT (nprotect);
  
  return res;
}
