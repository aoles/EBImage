#include "getFrames.h"
#include "tools.h"

/* -------------------------------------------------------------------------
Extract frames from image stack

Copyright (c) 2017 Andrzej Oles
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include <R_ext/Error.h>

/*----------------------------------------------------------------------- */
SEXP
getFrames (SEXP x, SEXP i, SEXP type) {
  int nprotect, nx, ny, nc, nd, typ, n, j, d, cm;
  int* ids;
  SEXP res, img, frame, tmp, dm, dnames, ndnames, names, nnames, dx;

  nprotect = 0;

  ids = INTEGER(i);
  n   = length(i);
  typ = INTEGER(type)[0];
  
  dx = GET_DIM(x);
  nx = INTEGER(dx)[0];
  ny = INTEGER(dx)[1];
  dnames = GET_DIMNAMES(x);
  
  if ( typ==0 ) {  
    nc = 1; 
    cm = MODE_GRAYSCALE;
  } else {
    nc = getNumberOfChannels(x);
    cm = COLOR_MODE(x);
  }

  /* allocate memory for frame list */
  PROTECT ( res = allocVector(VECSXP, n) );
  nprotect++;
  
  /* frame length */
  d = nx * ny * nc;
  
  /* create frame template */
  PROTECT( tmp = shallow_duplicate(x) );
  nprotect++;

  /* set frame dimensions */
  nd = ( cm==MODE_COLOR && length(dx)>2 ) ? 3 : 2;
  
  PROTECT ( dm = allocVector( INTSXP, nd) );
  nprotect++;
  
  INTEGER(dm)[0] = nx;
  INTEGER(dm)[1] = ny;
  if ( nd == 3 )
    INTEGER(dm)[2] = nc;
  
  img = PROTECT(allocArray(TYPEOF(x), dm));
  nprotect++;
  
  if ( dnames != R_NilValue ) {
    PROTECT ( ndnames = allocVector(VECSXP, nd) );
    nprotect++;   
  
    for (j=0; j<nd; j++)
      SET_VECTOR_ELT(ndnames, j, VECTOR_ELT(dnames, j)); 
    
    names = GET_NAMES(dnames);
    if ( names != R_NilValue ) {
      PROTECT ( nnames = allocVector(STRSXP, nd) );
      nprotect++;
      
      for (j=0; j<nd; j++)
        SET_STRING_ELT(nnames, j, STRING_ELT(names, j));
      
      SET_NAMES(ndnames, nnames);
    }
    
    SET_DIMNAMES(img, ndnames);
  }

  if ( isImage(x) ) {
    tmp = SET_SLOT( tmp, Image_Data, img);
    tmp = SET_SLOT( tmp, Image_colormode, ScalarInteger(cm) );
  }
  else {
    tmp = img;
  }
  
  for (j=0; j<n; j++) {
    PROTECT( frame = shallow_duplicate(tmp) );
    nprotect++;
    
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
