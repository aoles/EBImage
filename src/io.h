#ifndef EBIMAGE_IO_H
#define EBIMAGE_IO_H

#include <R.h>
#include <Rdefines.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP lib_readImages (SEXP, SEXP);
SEXP lib_writeImages (SEXP, SEXP, SEXP);
SEXP readCellomics(const char *);

#ifdef __cplusplus
};
#endif

#endif
