#include "filters_thresh.h"
#include "tools.h"

/* -------------------------------------------------------------------------
Adaptive thresholding, magick-independent implementation
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
lib_filterThresh (SEXP x, SEXP param) {
    int dx, dy, nx, ny, nz, nprotect, * dim, xi, yi, u, v, i;
    int sx, ex, sy, ey;
    double offset, * tgt, * src, sum, mean, nFramePix;
    SEXP res;


    dim = INTEGER ( GET_DIM(x) );
    nx = dim[0];
    ny = dim[1];
    nz = getNumberOfFrames(x,0);

    dx = (int)( REAL(param)[0] );
    dy = (int)( REAL(param)[1] );
    offset = REAL(param)[2];
    nprotect = 0;
    nFramePix = (2 * dx + 1) * (2 * dx + 1);

    PROTECT ( res = Rf_duplicate(x) );
    nprotect++;

    for ( i = 0; i < nz; i++ ) {
        tgt = &( REAL(res)[ i * nx * ny ] );
        src = &( REAL(x)[ i * nx * ny ] );
        for ( yi = dy; yi < ny - dy; yi++ ) {
            sum = 0.0;
            for ( xi = dx; xi < nx - dx; xi++ ) {
                if ( xi == dx) {
                /* first position in a row -- collect new sum */
                    for ( u = xi - dx; u <= xi + dx; u++ )
                        for ( v = yi - dy; v <= yi + dy; v++ )
                            sum += src [u + v * nx];
                }
                else {
                /* frame moved in the row, modify sum */
                    for ( v = yi - dy; v <= yi + dy; v++ )
                        sum += src [xi + dx + v * nx] - src [ xi - dx - 1 + v * nx];
                }
                /* calculate threshold and update tgt data */
                mean = sum / nFramePix + offset;
                sx = xi;
                ex = xi;
                sy = yi;
                ey = yi;
                if ( xi == dx ) {
                    /* left */
                    sx = 0;
                    ex = dx;
                }
                else
                if ( xi == nx - dx - 1 ) {
                    /* right */
                    sx = nx - dx - 1;
                    ex = nx - 1;
                }
                if ( yi == dy ) {
                    /* top */
                    sy = 0;
                    ey = dy;
                }
                else
                if ( yi == ny - dy - 1 ) {
                    /* bottom */
                    sy = ny - dy - 1;
                    ey = ny - 1;
                }
                if ( ex - sx > 0 || ey - sy > 0 ) {
                    for ( u = sx; u <= ex; u++ )
                        for ( v = sy; v <= ey; v++ )
                            tgt [u + v * nx] = ( src [u + v * nx] < mean ) ? BG : FG;
                }
                else /* thresh current pixel only */
                    tgt [xi + yi * nx] = ( src [xi + yi * nx] < mean ) ? BG : FG;
            }
        }
    }

    UNPROTECT (nprotect);
    return res;
}
