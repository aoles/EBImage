#include "common.h"

bool verbose = false;

SEXP setVerbose(SEXP value) {
    verbose = LOGICAL(value)[0];
    return value;
}
