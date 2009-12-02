#ifndef EBIMAGE_OBJECTS_H
#define EBIMAGE_OBJECTS_H

#include <R.h>
#include <Rdefines.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP paintObjects (SEXP, SEXP, SEXP, SEXP);
SEXP rmObjects (SEXP, SEXP);
SEXP stackObjects (SEXP, SEXP, SEXP, SEXP, SEXP);

#ifdef __cplusplus
};
#endif

#endif
