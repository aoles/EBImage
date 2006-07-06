/* -------------------------------------------------------------------------
Morphological filters: erode, dilate, open and close
Copyright (c) 2005 Oleg Sklyar
See flt_morph.h for license
------------------------------------------------------------------------- */
#include "flt_morph.h"
#include "indexing.h"

#include <R_ext/Error.h>
#include <iostream>

using namespace std;

inline bool match(int * kernel, double * data, Point & size, Point & at, double & mismatch) {
    int i, j, xx, yy;
    for (i = -1; i <= 1; i++)
        for (j = -1; j <= 1; j++) {
            if (!kernel[i + 1 + (j + 1) * 3]) continue;
            xx = at.x + i;
            yy = at.y + j;
            if (xx < 0 || yy < 0 || xx >= size.x || yy >= size.y) return false;
            if (data[xx + yy * size.x] == mismatch) return false;
        }
    return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
// function modifies its argument !!!  
SEXP erodeDilate(SEXP x, SEXP kernel, SEXP iters, SEXP alg) {
    double resetto = 0; // erode - check foreground
    if (INTEGER(alg)[0] != 0)
        resetto = 1.0;  // dilate - check background
    try {
        unsigned int i;
        if (!assertImage(x))
            error("wrong argument class, Image expected");
        if (LOGICAL(GET_SLOT(x, mkString("rgb")))[0])
            error("input must be a binary image");
        if (LENGTH(kernel) != 9)
            error("only 3x3 kernels supported so far");
        Point size(INTEGER(GET_DIM(x))[0], INTEGER(GET_DIM(x))[1]);
        unsigned int nimages = INTEGER(GET_DIM(x))[2];
        int * kern = LOGICAL(kernel);
        int nrepeats = INTEGER(iters)[0];
        Point pt;
        for (i = 0; i < nimages; i++) {
            double * data = &(REAL(x)[i * size.x * size.y]);
            for (int it = 0; it < nrepeats; it++) {
                for (int j = 0; j < size.x * size.y; j++) {
                    if (data[j] == resetto) continue;
                    pt = getpoint(j, size.x);    
                    if (!match(kern, data, size, pt, resetto)) 
                            data[j] = 0.5;
                }
                for (int j = 0; j < size.x * size.y; j++)
                    if (data[j] == 0.5) data[j] = resetto;
            }
        }        
            
    }
    catch(exception &error_) {
        error(error_.what());
    }
    return x;
}
