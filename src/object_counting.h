#ifndef EBIMAGE_COUNTING_H
#define EBIMAGE_COUNTING_H

#include <R.h>
#include <Rdefines.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP lib_paintFeatures (SEXP, SEXP, SEXP, SEXP);
SEXP lib_getFeatures (SEXP, SEXP);
SEXP lib_matchFeatures (SEXP, SEXP);
SEXP lib_deleteFeatures (SEXP, SEXP);
SEXP lib_stackFeatures (SEXP, SEXP);

#ifdef __cplusplus
};
#endif

#endif
