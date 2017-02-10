#include "getFrames.h"
#include "tools.h"

/* -------------------------------------------------------------------------
Convert from Array to list representation

Copyright (c) 2017 Andrzej Oles
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include <R_ext/Error.h>

/*----------------------------------------------------------------------- */
SEXP
getFrames (SEXP x, SEXP i, SEXP type) {
  int nprotect, nx, ny, nc, typ, n, j, d, cm, dim;
  int rendercolor;
  int* ids;
  SEXP res, img, frame, tmp, dm, dimnames, ndimnames, names, nnames;

  nprotect = 0;

  ids = INTEGER(i);
  n   = length(i);
  typ = INTEGER(type)[0];

  rendercolor = COLOR_MODE(x)==MODE_COLOR && typ==1;
  
  nx = INTEGER (GET_DIM(x))[0];
  ny = INTEGER (GET_DIM(x))[1];
  dimnames = GET_DIMNAMES(x);
  
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

  // set frame dimensions
  dim = rendercolor ? 3 : 2;
  
  PROTECT ( dm = allocVector( INTSXP, dim) );
  nprotect++;
  
  INTEGER (dm)[0] = nx;
  INTEGER (dm)[1] = ny;
  if ( dim == 3 )
    INTEGER(dm)[2] = nc;
  
  img = PROTECT(allocArray(TYPEOF(x), dm));
  nprotect++;
  
  if ( dimnames != R_NilValue ) {
    PROTECT ( ndimnames = allocVector(VECSXP, dim) );
    nprotect++;   
  
    for (j=0; j<dim; j++)
      SET_VECTOR_ELT(ndimnames, j, VECTOR_ELT(dimnames, j)); 
    
    names = GET_NAMES(dimnames);
    if ( names != R_NilValue ) {
      PROTECT ( nnames = allocVector(STRSXP, dim) );
      nprotect++;
      
      for (j=0; j<dim; j++)
        SET_STRING_ELT(nnames, j, STRING_ELT(names, j));
      
      SET_NAMES(ndimnames, nnames);
    }
    
    SET_DIMNAMES(img, ndimnames);
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
