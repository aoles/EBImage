#ifndef EBIMAGE_MOMENTS_H
#define EBIMAGE_MOMENTS_H

#include <R.h>
#include <Rdefines.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP lib_cmoments (SEXP, SEXP);
SEXP lib_moments  (SEXP, SEXP, SEXP, SEXP);

#ifdef __cplusplus
};
#endif

#endif
