#ifndef EBIMAGE_FLOODFILL_H
#define EBIMAGE_FLOODFILL_H

#include <R.h>
#include <Rdefines.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP floodFill(SEXP, SEXP, SEXP, SEXP);
SEXP fillHull(SEXP);

#ifdef __cplusplus
};
#endif

#endif
