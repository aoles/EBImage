#ifndef EBIMAGE_HARALICK_H
#define EBIMAGE_HARALICK_H

#include <R.h>
#include <Rdefines.h>

#ifdef __cplusplus
extern "C" {
#endif

  SEXP haralickMatrix(SEXP, SEXP, SEXP);
  SEXP haralickFeatures(SEXP);

#ifdef __cplusplus
};
#endif

#endif
