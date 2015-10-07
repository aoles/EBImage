#ifndef EBIMAGE_MORPH_H
#define EBIMAGE_MORPH_H

#include <R.h>
#include <Rdefines.h>

#ifdef __cplusplus
extern "C" {
#endif
  
  SEXP erode_dilate (SEXP, SEXP, SEXP);
  SEXP opening_closing (SEXP, SEXP, SEXP);
  SEXP tophat (SEXP, SEXP, SEXP);
  
#ifdef __cplusplus
};
#endif

#endif
