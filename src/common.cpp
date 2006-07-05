/* -------------------------------------------------------------------------
Common definitions for the EBImage project
Copyright (c) 2006 Oleg Sklyar
See: common.h for license
------------------------------------------------------------------------- */
#include "common.h"

bool verbose = false;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP setVerbose(SEXP value) {
    verbose = LOGICAL(value)[0];
    return value;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
bool assertImage(SEXP rimage) {
    if (strcmp(CHAR(asChar(GET_CLASS(rimage))), "Image") != 0) return false;
    if (LENGTH(GET_DIM(rimage)) != 3) return false;
    return true;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
bool assertImage2D(SEXP rimage) {
    if (!assertImage(rimage)) return false;
    if (INTEGER(GET_DIM(rimage))[2] == 1) return true;
    return false;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
