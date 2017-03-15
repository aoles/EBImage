#include "transpose.h"
#include "EBImage.h"
#include "tools.h"

/* -------------------------------------------------------------------------
 Cache oblivious transposition of spatial dimensions
 
 Copyright (c) 2017 Andrzej Oles
 See: ../LICENSE for license, LGPL
 ------------------------------------------------------------------------- */

#define BLOCKSIZE 16

template <typename type> void _transpose(type *, type *, int, int, int, int, PointXY);

template <typename type> void _transpose(type * src, type * tgt, int rb, int re, int cb, int ce, PointXY size) {
  int r = re - rb, c = ce - cb;
  if (r <= BLOCKSIZE && c <= BLOCKSIZE) {
    for (int i = rb; i < re; i++) {
      for (int j = cb; j < ce; j++) {
        tgt[j + i*size.y] = src[i + j*size.x];
      }
    }
  } else if (r >= c) {
    _transpose<type>(src, tgt, rb, rb + (r / 2), cb, ce, size);
    _transpose<type>(src, tgt, rb + (r / 2), re, cb, ce, size);
  } else {
    _transpose<type>(src, tgt, rb, re, cb, cb + (c / 2), size);
    _transpose<type>(src, tgt, rb, re, cb + (c / 2), ce, size);
  }
}

SEXP transpose (SEXP x) {
  validImage(x, 0);
  
  int nprotect = 0;
  
  SEXP res = PROTECT( allocVector(TYPEOF(x), XLENGTH(x)) );
  nprotect++;
  DUPLICATE_ATTRIB(res, x);
  
  // swap spatial dimensions
  SEXP dim = PROTECT( duplicate(GET_DIM(x)) );
  nprotect++;
  int * dm = INTEGER(dim);
  
  int tmp;
  tmp = dm[0];
  dm[0] = dm[1];
  dm[1] = tmp;
  
  SET_DIM(res, dim);
  
  // swap dimnames
  if ( GET_DIMNAMES(x) != R_NilValue ) {
    SEXP dnames = PROTECT( duplicate(GET_DIMNAMES(x)) );
    nprotect++;
    SEXP v = PROTECT( VECTOR_ELT(dnames, 0) );
    nprotect++;
    SET_VECTOR_ELT(dnames, 0, VECTOR_ELT(dnames, 1)); 
    SET_VECTOR_ELT(dnames, 1, v); 
    
    if ( GET_NAMES(dnames) != R_NilValue ) {
      SEXP names = PROTECT( duplicate(GET_NAMES(dnames)) );
      nprotect++;
      SEXP s = PROTECT( STRING_ELT(names, 0) );
      nprotect++;
      SET_STRING_ELT(names, 0, STRING_ELT(names, 1)); 
      SET_STRING_ELT(names, 1, s); 
      
      SET_NAMES(dnames, names);
    }
    
    SET_DIMNAMES(res, dnames);
  } 
  
  // transpose
  PointXY size;
  size.x = INTEGER ( GET_DIM(x) )[0];
  size.y = INTEGER ( GET_DIM(x) )[1];
  int nz = getNumberOfFrames(x, 0);
  
  int sizexy = size.x * size.y;
  int offset = 0;
  
  for (int i = 0; i < nz; i++, offset+=sizexy) {
    switch (TYPEOF(x)) {
    case LGLSXP:
      _transpose<int>( &(LOGICAL(x)[offset]), &(LOGICAL(res)[offset]), 0, size.x, 0, size.y, size);
      break;
    case INTSXP:
      _transpose<int>( &(INTEGER(x)[offset]), &(INTEGER(res)[offset]), 0, size.x, 0, size.y, size);
      break;
    case REALSXP:
      _transpose<double>( &(REAL(x)[offset]), &(REAL(res)[offset]), 0, size.x, 0, size.y, size);
      break;
    }
  }
  
  UNPROTECT (nprotect);
  return res;
}
