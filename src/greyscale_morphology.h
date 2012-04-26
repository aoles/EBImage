#ifndef EBIMAGE_GREY_MORPH_H
#define EBIMAGE_GREY_MORPH_H

#include <R.h>
#include <Rdefines.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP lib_erode_dilate_greyscale (SEXP, SEXP, SEXP);
SEXP lib_opening_closing_greyscale (SEXP, SEXP, SEXP);
SEXP lib_tophat_greyscale (SEXP, SEXP, SEXP);

#ifdef __cplusplus
};
#endif

#endif
