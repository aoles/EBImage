/* -------------------------------------------------------------------------
Distance map transform for Image

Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "common.h"

#define BG 0.0

/*----------------------------------------------------------------------- */
int do_distMap (double *, int, int, double, double, int);

/*----------------------------------------------------------------------- */
SEXP
lib_distMap (SEXP x, SEXP tolerance, SEXP minBG, SEXP strict) {
    SEXP res;
    int nprotect, i, nx, ny, nz;
    double tol, minimumBG;
    
    nx = INTEGER ( GET_DIM(x) )[0];
    ny = INTEGER ( GET_DIM(x) )[1];
    nz = INTEGER ( GET_DIM(x) )[2];
    tol = REAL (tolerance)[0];
    minimumBG = REAL (minBG)[0];
    nprotect = 0;
    PROTECT ( res = Rf_duplicate(x) );
    nprotect++;
    
    for ( i = 0; i < nz; i++ ) {
        if ( !do_distMap ( &( REAL(res)[ i * nx * ny ] ), nx, ny, tol, minimumBG, INTEGER(strict)[0] ) ) {
            UNPROTECT (nprotect);
            error ( _("background constitutes less than 5 percent of the image") );
        }
    }
    
    UNPROTECT (nprotect);
    return res;
}

/*----------------------------------------------------------------------- */

#define UPDATE_MAX_D() ( \
  { if ( data [xi + yi * nx] == BG ) { d = 1.0; continue; } \
    if ( d < data [xi + yi * nx] ) data [xi + yi * nx] = d; \
    if ( d < maxd ) d += 1.0; \
  } )
  
int
do_distMap (double * data, int nx, int ny, double tolerance, double minBG, int strict) {
    int xi, yi, u, v, done, found, nBG;
    double d, tmpd;
    double maxd;

    /* full diagonal */    
    maxd = sqrt ( nx * nx + ny * ny);
    nBG = 0;
    /* scan through rows to guess the maximum possible distances within rows */
    for ( yi = 0; yi < ny; yi++ ) {
        /* o--->x initialize */
        /* until we find first BG point */
        d = maxd;
        for ( xi = 0; xi < nx; xi++ ) {
            /* if background, set as such and reset d to 1 */
            if ( data[xi + yi * nx] <= tolerance ) {
                d = 1.0; 
                data [xi + yi * nx] = BG;
                nBG++;;
                continue;
            }
            /* for non bg */
            data [xi + yi * nx] = d;
            /* we move one step away from BG, so we increase this value */
            if ( d < maxd ) d += 1.0;
        }
        /* the same x<---o , but only updating not resetting */
        d = maxd;
        for ( xi = nx - 1; xi >= 0; xi-- ) UPDATE_MAX_D();
    }
    /* return error if too few background pixels */
    if ( nBG < minBG * nx * ny ) return 0;
    /* scan through cols to update the maximum possible distances within cols */
    for ( xi = 0; xi < nx; xi++ ) {
        /* top-bottom initialize */
        d = maxd;
        for ( yi = 0; yi < ny; yi++ ) UPDATE_MAX_D();
        /* bottom-up */
        d = maxd;
        for ( yi = ny - 1; yi >= 0; yi-- ) UPDATE_MAX_D();
    }
    /* scan through all non-BG pixels and find min dist in the square defined by current value */
    for ( xi = 0; xi < nx; xi++ )
        for ( yi = 0; yi < ny; yi++ ) {
            d = data [xi + yi * nx];
            /* this one is either BG of 1-px off, which cannot be smaller */
            if ( d < 1.2 ) continue;
            /* scan 4 squares of current val in all diagonal dirs, excluding vertical
            and horizontal pixels for, scan more or less expanding to stop as soon as smth found */
            done = 0;
            for ( u = 0; u < d && !done; u++ ) {
                for ( v = 0; v < d && !done; v++ ) {
                    found = 0;
                    if ( xi + u < nx && yi + v < ny )
                        if ( data [xi + u + (yi + v) * nx] == BG ) found = 1;
                    if ( xi - u >= 0 && yi + v < ny && !done )
                        if ( data [xi - u + (yi + v) * nx] == BG ) found = 1;
                    if ( xi - u >= 0 && yi - v >= 0 && !done )
                        if ( data [xi - u + (yi - v) * nx] == BG ) found = 1;
                    if ( xi + u < nx && yi - v >= 0 && !done )
                        if ( data [xi + u + (yi - v) * nx] == BG ) found = 1;
                    if ( found ) {
                        tmpd = sqrt ( u * u + v * v ); 
                        if ( tmpd < 1.9 && !strict ) tmpd = 1.0;
                        if ( tmpd < data [xi + yi * nx] )
                            data [xi + yi * nx] = tmpd;
                    }
                }
            }
        }
    return 1;
}
