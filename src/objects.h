#ifndef EBIMAGE_OBJECTS_H
#define EBIMAGE_OBJECTS_H

#include <R.h>
#include <Rdefines.h>

#ifdef __cplusplus
extern "C" {
#endif

SEXP lib_paintFeatures (SEXP, SEXP, SEXP, SEXP);
SEXP lib_matchFeatures (SEXP, SEXP);
SEXP lib_deleteFeatures (SEXP, SEXP);

SEXP lib_stack_objects (SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP lib_tile_stack    (SEXP, SEXP, SEXP);
SEXP lib_untile        (SEXP, SEXP, SEXP, SEXP);

#ifdef __cplusplus
};
#endif

#endif
