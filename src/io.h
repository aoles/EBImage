#ifndef EBIMAGE_IO_H
#define EBIMAGE_IO_H

#include <R.h>
#include <Rdefines.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP lib_readImages (SEXP, SEXP);
SEXP lib_chooseImages ();
SEXP lib_writeImages (SEXP, SEXP, SEXP);

#ifdef __cplusplus
};
#endif

#endif
