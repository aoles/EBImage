#ifndef EBIMAGE_FLOODFILL_H
#define EBIMAGE_FLOODFILL_H

#include <R.h>
#include <Rdefines.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP lib_floodFill(SEXP, SEXP, SEXP, SEXP);
SEXP lib_fillHull(SEXP);

#ifdef __cplusplus
};
#endif

#endif
