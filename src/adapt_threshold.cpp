#include "adapt_threshold.h"
#include "conversions.h"

#include <R_ext/Error.h>
#include <R_ext/Memory.h>
#include <string>

using namespace std;

SEXP adaptiveThreshold(SEXP rimage, SEXP param) {
    /* R routine must ensure that rimage has correct type;
       param are double and has correct number of parameters and both are non-NULL */
    if (LOGICAL(GET_SLOT(rimage, mkString("rgb")))[0])
        error("this algorithm works for grayscale images only");
    try {
        int w = (int)(REAL(param)[0] / 2.0);
        int h = (int)(REAL(param)[1] / 2.0);
        if (w * h == 0)
            error("width * height must be > 0");
        int * dim = INTEGER(GET_DIM(rimage));
        int ndim = LENGTH(GET_DIM(rimage));
        int ncol = dim[0];
        int nrow = dim[1];
        int nimages = 1;
        if (ndim > 2)
            nimages = dim[2];
        /* grayscale images assumed of the type double */
        double * data;
        int npix = 4 * w * h;
        double offset = REAL(param)[2];
        double sum = 0.0;
        double mean = 0.0;
        SEXP frameSEXP;
        PROTECT(frameSEXP = allocVector(REALSXP, ncol * nrow));
        double * frame = &REAL(frameSEXP)[0];
        for (int i = 0; i < nimages; i++) {
            data = &(REAL(rimage)[i * ncol * nrow]);
            /* ALGORITHM STARTS HERE */
            for (int row = h; row < nrow - h; row++) {
                for (int col = w; col < ncol - w; col++) {
                    if (col == w) {
                        /* collect new sum */
                        sum = 0.0;
                        for (int u = col - w; u < col + w; u++)
                            for (int v = row - h; v < row + h; v++)
                                sum += data[u + v * ncol];
                    }
                    else
                        /* modify sum */
                        for (int v = row - h; v < row + h; v++)
                            sum += data[(col + w - 1) + v * ncol] - data[(col - w - 1) + v * ncol];
                    mean = sum / npix + offset;
                    /* threshold current pixel */
                    frame[col + row * ncol] = (data[col + row * ncol] <= mean)?0.0:1.0;
                    /* if left - threshold row from 0 till w - 1 */
                    if (col == w)
                        for (int u = 0; u < w; u++)
                            frame[u + row * ncol] = (data[u + row * ncol] <= mean)?0.0:1.0;
                    /* if right - threshold row from ncol - w till ncol - 1 */
                    if (col == ncol - w - 1)
                        for (int u = ncol - w; u < ncol; u++)
                            frame[u + row * ncol] = (data[u + row * ncol] <= mean)?0.0:1.0;
                    /* if top - threshold column from 0 till h - 1 */
                    if (row == h)
                        for (int v = 0; v < h; v++)
                            frame[col + v * ncol] = (data[col + v * ncol] <= mean)?0.0:1.0;
                    /* if bottom - threshold column from nrow - h till nrow - 1 */
                    if (row == nrow - h - 1)
                        for (int v = nrow - h; v < nrow; v++)
                            frame[col + v * ncol] = (data[col + v * ncol] <= mean)?0.0:1.0;
                    /* if left-top - threshold the corner */
                    if (col == w && row == h)
                        for (int u = 0; u < w; u++)
                            for (int v = 0; v < h; v++)
                                frame[u + v * ncol] = (data[u + v * ncol] <= mean)?0.0:1.0;
                    /* if right-top - threshold the corner */
                    if (col == ncol - w - 1 && row == h)
                        for (int u = ncol - w; u < ncol; u++)
                            for (int v = 0; v < h; v++)
                                frame[u + v * ncol] = (data[u + v * ncol] <= mean)?0.0:1.0;
                    /* if right-bottom - threshold the corner */
                    if (col == ncol - w - 1 && row == nrow - h - 1)
                        for (int u = ncol - w; u < ncol; u++)
                            for (int v = nrow - h; v < nrow; v++)
                                frame[u + v * ncol] = (data[u + v * ncol] <= mean)?0.0:1.0;
                    /* if left-bottom - threshold the corner */
                    if (col == w && row == nrow - h - 1)
                        for (int u = 0; u < w; u++)
                            for (int v = nrow - h; v < nrow; v++)
                                frame[u + v * ncol] = (data[u + v * ncol] <= mean)?0.0:1.0;
                }
            }
            memcpy(data, frame, ncol * nrow * sizeof(double));
            /* ALGORITHM ENDS HERE */
        }
        UNPROTECT(1);
    }
    catch(...) {
        error("exception within normalizeImages c++ routine");
    }
    return rimage;
}
