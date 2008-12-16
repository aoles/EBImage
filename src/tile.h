#ifndef EBIMAGE_TILE_H
#define EBIMAGE_TILE_H

#include <R.h>
#include <Rdefines.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP tile (SEXP, SEXP, SEXP);
SEXP untile (SEXP, SEXP, SEXP, SEXP);

#ifdef __cplusplus
};
#endif

#endif
