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
    int dx, dy, nx, ny, nz, nprotect, * dim, xi, yi, u, v, k, l, ou, nu, ov, nv, i;
    int sx, ex, sy, ey;
    double offset, * tgt, * src, *colsums, sum, mean, nFramePix;
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
    
    /* allocate vector of column sums */
    colsums = (double *) malloc ( nx * sizeof(double) );

    for ( i = 0; i < nz; i++ ) {
        tgt = &( REAL(res)[ i * nx * ny ] );
        src = &( REAL(x)[ i * nx * ny ] );
        
        for ( yi = 0; yi < ny; yi++ ) {
            if ( yi == 0 ) {
            /* initialize column sums */
                for ( k = 0; k < nx; k++ ) {
                    colsums[k] = dy * src[k];
                    
                    for ( l = 0; l <= dy; l++ )
                        colsums[k] += src[k + l * nx];
                }
            }
            else {
            /* update column sums */
                ov = yi - dy - 1;
                nv = yi + dy;
                
                if (ov < 0) ov = 0;
                else if (nv >= ny) nv = ny - 1;
                
                for ( k = 0; k < nx; k++ )
                    colsums[k] += src[k + nv * nx] - src[k + ov * nx];
            }
          
            for ( xi = 0; xi < nx; xi++ ) {
                if ( xi == 0 ) {
                /* first position in a row -- collect new sum */
                    sum = 0.0;
                    for ( k = xi - dx; k <= xi + dx; k++ ) {
                        u = k;
                        if (u < 0) u = 0;
                        else if (u >= nx) u = nx - 1;
                        
                        sum += colsums[u];
                    }
                }
                else {
                /* frame moved in the row, modify sum */
                    ou = xi - dx - 1;
                    nu = xi + dx;
                    
                    if (ou < 0) ou = 0;
                    else if (nu >= nx) nu = nx - 1;
                    
                    sum += colsums[nu] - colsums[ou];
                }
            
                /* calculate threshold and update tgt data */
                mean = sum / nFramePix + offset;
                
                /* thresh current pixel */
                tgt [xi + yi * nx] = ( src [xi + yi * nx] < mean ) ? BG : FG;
            }
        }
    }

    free(colsums);

    UNPROTECT (nprotect);
    return res;
}
