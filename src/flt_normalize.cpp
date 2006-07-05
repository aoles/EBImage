/* -------------------------------------------------------------------------
Normalize filter
Copyright (c) 2005 Oleg Sklyar
See flt_normalize.h for license
------------------------------------------------------------------------- */
#include "flt_normalize.h"

#include <R_ext/Error.h>
#include <iostream>

using namespace std;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
void normalizeDataset(double * data, double * range, int length) {
    if (data == NULL || range == NULL || length <= 0) return;
    double max = data[0];
    double min = data[0];
    double value;
    for (int i = 0; i < length; i++) {
        value = data[i];
        if (value < min) min = value;
        if (value > max) max = value;
    }
    if (min == max) return;
    double factor = (range[1] - range[0]) / (max - min);
    for (int i = 0; i < length; i++)
        data[i] = (data[i] - min) * factor + range[0];
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* this function modifies the object - must be copied before if required! */
SEXP normalizeImages(SEXP rimage, SEXP range, SEXP independent) {
    if (!assertImage(rimage))
        error("Wrong argument class, Image expected");
    try {
        int * dim = INTEGER(GET_DIM(rimage));
        int ncol = dim[0];
        int nrow = dim[1];
        int nimages = dim[2];
        /* grayscale images assumed of the type double */
        double * data;
        switch (LOGICAL(independent)[0]) {
            case true: {
                for (int i = 0; i < nimages; i++) {
                    data = &(REAL(rimage)[i * ncol * nrow]);
                    normalizeDataset(data, &(REAL(range)[0]), ncol * nrow);
                }
            }; break;
            default: {
                data = &(REAL(rimage)[0]);
                normalizeDataset(data, &(REAL(range)[0]), ncol * nrow * nimages);
            }
        }
    }
    catch(exception &error_) {
        error(error_.what());
    }
    return rimage;
}

