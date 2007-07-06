#ifndef EBIMAGE_FEATURES_HARALICK_H
#define EBIMAGE_FEATURES_HARALICK_H

#include <R.h>
#include <Rdefines.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP lib_co_occurrence (SEXP, SEXP, SEXP);
SEXP lib_haralick      (SEXP);

#ifdef __cplusplus
};
#endif

#endif
