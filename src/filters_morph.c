/* -------------------------------------------------------------------------
Morphological filters for Image
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "common.h"

/*----------------------------------------------------------------------- */

#define ERODE  0
#define DILATE 1

/*----------------------------------------------------------------------- */
/* forward declaration */
int _match (int *, PointXY *, double *, PointXY *, PointXY *, double);

/*----------------------------------------------------------------------- */
SEXP
lib_erode_dilate (SEXP x, SEXP kernel, SEXP iters, SEXP what) {
    double resetTo, * tgt, * src;
    int nz, nt, i, j, it, nprotect;
    int * dim, * kern;
    PointXY size, ksize, pt;
    SEXP res;
    
    /* value to reset the checked part t */
    if ( INTEGER(what)[0] == ERODE )
        resetTo = 1.0; /* checking background, reseting to 1 */
    else
        resetTo = 0.0; /* checking foreground, reseting to 0 */
    dim = INTEGER ( GET_DIM(x) );
    size.x = dim[0];
    size.y = dim[1];
    nz = dim[2];
    kern = INTEGER (kernel);
    ksize.x = INTEGER ( GET_DIM(kernel) )[0];
    ksize.y = INTEGER ( GET_DIM(kernel) )[1];
    nt = INTEGER (iters)[0];
    nprotect = 0;
    
    PROTECT ( res = Rf_duplicate(x) );
    nprotect++;
    
    for ( i = 0; i < nz; i++ ) {
        tgt = &( REAL(res)[i * size.x * size.y] );
        src = &( REAL(x)[i * size.x * size.y] );
        for ( it = 0; it < nt; it++ )
            for ( j = 0; j < size.x * size.y; j++ ) {
                if ( tgt[j] == resetTo ) continue;
                pt = pointFromIndex (j, size.x);
                if ( !_match(kern, &ksize, src, &size, &pt, resetTo) )
                    tgt[j] = resetTo;
            }
    }

    UNPROTECT (nprotect);
    return res;
}


/*----------------------------------------------------------------------- */
int 
_match (int * kernel, PointXY * ksize, double * data, PointXY * dsize, PointXY * at, double mismatch) {
    int i, j, xx, yy, kcx, kcy;
    
    kcx = ksize->x / 2;
    kcy = ksize->y / 2;
    for ( i = -kcx; i <= kcx; i++ )
        for ( j = -kcy; j <= kcy; j++ ) {
            if ( kernel[ (i + kcx) + (j + kcy) * ksize->x ] == 0) continue;
            xx = at->x + i;
            yy = at->y + j;
            if ( xx < 0 || yy < 0 || xx >= dsize->x || yy >= dsize->y ) continue;
            if ( data[xx + yy * dsize->x] == mismatch ) return 0;
        }
    return 1;
}

