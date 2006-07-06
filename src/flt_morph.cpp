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

/* kernel size must be odd number: 3, 5, 7 etc */
inline bool match(int * kernel, Point &ksize, double * data, Point & dsize, Point & at, double & mismatch) {
    int i, j, xx, yy;
    int kcx = ksize.x / 2;
    int kcy = ksize.y / 2;
    for (i = -kcx; i <= kcx; i++)
        for (j = -kcy; j <= kcy; j++) {
            if (kernel[(i + kcx) + (j + kcy) * ksize.x] == 0) continue;
            xx = at.x + i;
            yy = at.y + j;
            if (xx < 0 || yy < 0 || xx >= dsize.x || yy >= dsize.y) continue;
            if (data[xx + yy * dsize.x] == mismatch) return false;
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
        Point size(INTEGER(GET_DIM(x))[0], INTEGER(GET_DIM(x))[1]);
        unsigned int nimages = INTEGER(GET_DIM(x))[2];
        int * kern = INTEGER(kernel);
        Point ksize(INTEGER(GET_DIM(kernel))[0], INTEGER(GET_DIM(kernel))[1]);
        int nrepeats = INTEGER(iters)[0];
        Point pt;
        for (i = 0; i < nimages; i++) {
            double * data = &(REAL(x)[i * size.x * size.y]);
            for (int it = 0; it < nrepeats; it++) {
                for (int j = 0; j < size.x * size.y; j++) {
                    if (data[j] == resetto) continue;
                    pt = getpoint(j, size.x);    
                    if (!match(kern, ksize, data, size, pt, resetto)) 
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
