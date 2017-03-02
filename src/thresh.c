#include "thresh.h"
#include "tools.h"

/* -------------------------------------------------------------------------
Adaptive thresholding
Uses mean value estimated on a square frame, to speed up calculations
the sum is not calculated for the full frame, but updated on the sum of
a columns added and removed after the frame is replaced

Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include <R_ext/Error.h>

#define BG 0.0
#define FG 1.0

/*----------------------------------------------------------------------- */
SEXP
thresh (SEXP x, SEXP param) {
    int dx, dy, nx, ny, nz, nprotect, * dim, xi, yi, u, v, k, l, ou, nu, i;
    int sx, ex, sy, ey;
    double offset, * tgt, * src, sum, mean, nFramePix;
    SEXP res;
    
    validImage(x,0);

    dim = INTEGER ( GET_DIM(x) );
    nx = dim[0];
    ny = dim[1];
    nz = getNumberOfFrames(x,0);

    dx = (int)( REAL(param)[0] );
    dy = (int)( REAL(param)[1] );
    offset = REAL(param)[2];
    nprotect = 0;
    nFramePix = (2 * dx + 1) * (2 * dy + 1);

    PROTECT ( res = Rf_duplicate(x) );
    nprotect++;
    
    for ( i = 0; i < nz; i++ ) {
        tgt = &( REAL(res)[ i * nx * ny ] );
        src = &( REAL(x)[ i * nx * ny ] );
        for ( yi = 0; yi < ny; yi++ ) {
            sum = 0.0;
            for ( xi = 0; xi < nx; xi++ ) {
                if ( xi == 0 ) {
                /* first position in a row -- collect new sum */
                    for ( k = xi - dx; k <= xi + dx; k++ ) {
                        u = k;
                        if (u < 0) u = 0;
                        else if (u >= nx) u = nx - 1;
                        
                        for ( l = yi - dy; l <= yi + dy; l++ ) {
                            v = l;
                            if (v < 0) v = 0;
                            else if (v >= ny) v = ny - 1;
                            sum += src [u + v * nx]; 
                        }
                    }
                }
                else {
                /* frame moved in the row, modify sum */
                    ou = xi - dx - 1;
                    nu = xi + dx;
                    
                    if (ou < 0) ou = 0;
                    else if (nu >= nx) nu = nx - 1;
                    
                    for ( l = yi - dy; l <= yi + dy; l++ ) {
                        v = l;
                        if (v < 0) v = 0;
                        else if (v >= ny) v = ny - 1;
                        sum += src [nu + v * nx] - src [ ou + v * nx];
                    }
                }
                /* calculate threshold and update tgt data */
                mean = sum / nFramePix + offset;
                
                /* thresh current pixel */
                tgt [xi + yi * nx] = ( src [xi + yi * nx] < mean ) ? BG : FG;
            }
        }
    }

    UNPROTECT (nprotect);
    return res;
}
