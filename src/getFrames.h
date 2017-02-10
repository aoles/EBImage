#ifndef EBIMAGE_GETFRAMES_H
#define EBIMAGE_GETFRAMES_H

#include <R.h>
#include <Rdefines.h>

#include "EBImage.h"

#ifdef __cplusplus
extern "C" {
#endif

SEXP getFrame (SEXP, SEXP, SEXP);
SEXP getFrames (SEXP, SEXP, SEXP);

#ifdef __cplusplus
};
#endif

#endif
