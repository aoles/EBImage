#include "getFrames.h"
#include "tools.h"

/* -------------------------------------------------------------------------
Extract frames from image stack

Copyright (c) 2017 Andrzej Oles
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include <R_ext/Error.h>

/*----------------------------------------------------------------------- */
SEXP getFrames (SEXP x, SEXP i, SEXP _type) {
  int n, nx, ny, nc, nd, d, type, mode;
  int j, nprotect, isimage;
  int* ids;
  SEXP res, frame, dm, names, dnames;
  
  nprotect = 0;
  isimage = isImage(x);
  
  ids  = INTEGER(i);
  n    = length(i);
  type = INTEGER(_type)[0];
  mode = COLOR_MODE(x);
  
  nx = INTEGER(GET_DIM(x))[0];
  ny = INTEGER(GET_DIM(x))[1];
  
  if ( type==0 ) {  
    nc = 1; 
    mode = MODE_GRAYSCALE;
  } else {
    nc = getNumberOfChannels(x, mode);
  }
  
  /* allocate memory for frame list */
  PROTECT ( res = allocVector(VECSXP, n) );
  nprotect++;
  
  /* frame length */
  d = nx * ny * nc;
  
  /* set frame dimensions */
  nd = ( mode==MODE_COLOR && length(GET_DIM(x))>2 ) ? 3 : 2;
  
  PROTECT ( dm = allocVector( INTSXP, nd) );
  nprotect++;
  
  INTEGER(dm)[0] = nx;
  INTEGER(dm)[1] = ny;
  if ( nd == 3 )
    INTEGER(dm)[2] = nc;
  
  /* set dimnames */
  if ( GET_DIMNAMES(x) != R_NilValue ) {
    PROTECT ( dnames = allocVector(VECSXP, nd) );
    nprotect++;   
    
    for (j=0; j<nd; j++)
      SET_VECTOR_ELT(dnames, j, VECTOR_ELT(GET_DIMNAMES(x), j)); 
    
    if ( GET_NAMES(GET_DIMNAMES(x)) != R_NilValue ) {
      PROTECT ( names = allocVector(STRSXP, nd) );
      nprotect++;
      
      for (j=0; j<nd; j++)
        SET_STRING_ELT(names, j, STRING_ELT(GET_NAMES(GET_DIMNAMES(x)), j));
      
      SET_NAMES(dnames, names);
    }
  } else {
    dnames = R_NilValue;
  }
  
  for (j=0; j<n; j++) {
    PROTECT(frame = allocVector(TYPEOF(x), d));
    nprotect++;
    
    DUPLICATE_ATTRIB(frame, x);
    SET_DIM(frame, dm);
    SET_DIMNAMES(frame, dnames);
    
    if (isimage) frame = SET_SLOT( frame, Image_colormode, ScalarInteger(mode) );
    
    // copy pixel data
    switch( TYPEOF(x) ) {
    case LGLSXP:
    case INTSXP:
      memcpy( INTEGER(frame), &(INTEGER(x)[d*(ids[j]-1)]), d * sizeof(int));
      break;
    case REALSXP:
      memcpy( REAL(frame), &(REAL(x)[d*(ids[j]-1)]), d * sizeof(double));
      break;
    }
    
    SET_VECTOR_ELT(res, j, frame);    
  }
  
  UNPROTECT (nprotect);
  return res;
}

SEXP getFrame (SEXP x, SEXP i, SEXP type) {
  return VECTOR_ELT(getFrames(x, i, type), 0);
}
