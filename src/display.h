#ifndef EBIMAGE_DISPLAY_H
#define EBIMAGE_DISPLAY_H

#include <R.h>
#include <Rdefines.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP lib_display(SEXP, SEXP);
SEXP lib_animate (SEXP);

#ifdef __cplusplus
};
#endif

#endif
