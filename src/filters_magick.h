#ifndef EBIMAGE_MAGICK_H
#define EBIMAGE_MAGICK_H

#include <R.h>
#include <Rdefines.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP lib_filterMagick (SEXP, SEXP, SEXP);
/* SEXP lib_filterFill (SEXP, SEXP, SEXP, SEXP, SEXP); */

#ifdef __cplusplus
};
#endif

#endif
