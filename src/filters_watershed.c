/* -------------------------------------------------------------------------
Watershed transform for Image
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "common.h"

/*----------------------------------------------------------------------- */
#define BG 0.0
#define PROGRESS_MAX 60.0

#define EXCLUDE 0
#define STEEPEST 1
#define SMOOTH 2

/* considers image of hills > background of 0.0 and fills them top-down */
/* will generate ObjectImage as result */
/*----------------------------------------------------------------------- */
SEXP
lib_filterInvWS (SEXP x, SEXP ref, SEXP _dodetect, SEXP _alg, SEXP _ext, SEXP _verbose) {
    SEXP res, indexSXP;
    int nprotect, im, i, iend, j, ix, jy, npx, nx, ny, nz, * index, index1, marker, progress, counter, verbose, ext, alg;
    double * src, * tgt, thisBe, el, mel;
    PointXY pt;
    
    nx = INTEGER ( GET_DIM(x) )[0];
    ny = INTEGER ( GET_DIM(x) )[1];
    nz = INTEGER ( GET_DIM(x) )[2];
    nprotect = 0;

    PROTECT ( res = Rf_duplicate(x) );
    nprotect++;
    PROTECT ( indexSXP = allocVector(INTSXP, nx * ny) );
    nprotect++;
    
    index = INTEGER (indexSXP);
    
    ext = INTEGER (_ext)[0];
    alg = INTEGER (_alg)[0];
    verbose = INTEGER (_verbose)[0];
    if ( verbose )
        Rprintf ("  watershed ");
    progress = 0;

    for ( im = 0; im < nz; im++ ) {
    /* ******* LOOP through images ****************************************** */

        src = &( REAL(x)[ im * nx * ny ] );
        tgt = &( REAL(res)[ im * nx * ny ] );
        /* generate pixel index and negate the image -- filling wells */ 
        for ( i = 0; i < nx * ny; i++ ) {
            index [i] = i;
            tgt [i] *= -1.0;
        }
        /* from R includes R_ext/Utils.h */
        /* will resort tg as well */
        rsort_with_index (tgt, index, nx * ny);
        /* reassign tgt as it was reset above  */
        for ( i = 0; i < nx * ny; i++ )
            tgt [i] = - src[i];

        /* loop through the sorted list and pool values */
        marker = 1;
        i = 0;
        while ( i < nx * ny && src[ index[i] ] != BG ) {
            if ( verbose ) {
                counter = floor ( PROGRESS_MAX * (im * nx * ny + i + 1) / (double)( nz * nx * ny) );
                for ( ; progress < counter; progress++)
                    Rprintf (">");
            }
            /* grab the currently lowest value */
            iend = i;
            /* find index at which this value changes -- plato? */
            for ( j = i + 1; j < nx * ny; j++ ) {
                iend = j - 1;
                if ( src[ index[i] ] != src[ index[j] ] ) break;
            }
            /* how many pixels we still have on this plato */
            npx = iend - i + 1;
            /* loop in here until we assigned all the pixels in this plato */
            while ( npx > 0 ) {
                /* try to assign all the border pixels */
                for ( j = i; j <= iend && npx > 0; ) {
                    if ( tgt[ index[j] ] >= 0 ) { /* already assigned in this while loop */
                        j++;
                        continue; 
                    }
                    /* check neighbours and if exist only 1 -- assigned, if more than 1 - assign 0.5 */
                    thisBe = -1;
                    pt = pointFromIndex (index[j], nx);
                    if ( alg == SMOOTH )
                        mel = 1e9;
                    else
                        mel = 0;
                    for ( ix = pt.x - ext; ix <= pt.x + ext; ix++ ) {
                        for ( jy = pt.y - ext; jy <= pt.y + ext; jy++ ) {
                            if ( ix < 0 || jy < 0 || ix >= nx || jy >= ny || (ix == pt.x && jy == pt.y) ) continue;
                            index1 = ix + jy * nx;
                            if ( tgt[index1] > 0.9 ) {
                                switch (alg) {
                                case EXCLUDE: /* FIXME: find a way to keep border at 1 pixel with ext > 1 */
                                    if ( thisBe > 0 && thisBe != tgt[index1] ) 
                                        thisBe = 0.5;
                                    else
                                        thisBe = tgt [index1];
                                break;
                                default:
                                    el = fabs ( fabs( src[index1] ) - fabs( src[ index[j] ] ) );
                                    if ( (el < mel && alg == SMOOTH) || (el > mel && alg == STEEPEST) ) {
                                        thisBe = tgt [index1];
                                        mel = el;
                                    }
                                } // default
                            } // if
                        } // for jy
                    } // for ix
                    if ( thisBe >= 0 ) {
                        /* assign to the existing */
                        tgt[ index[j] ] = thisBe;
                        npx--;
                    }
                    if ( thisBe > 0.9)
                        j = i;
                    else
                        j++;
                }
                /* only those left that could not be assigned */
                /* create 1 new start point and restart the while loop */
                for ( j = i; j <= iend && npx > 0; j++) {
                    if ( tgt[ index[j] ] >= 0 ) continue; /* already assigned in this while loop */
                    /* as we think we assigned all neighbours in the previous loop
                       we can create new seed here */                
                    tgt[ index[j] ] = marker;
                    marker += 1;
                    npx--;
                    break;
                }
            }
            /* move index point behind this plato */
            i = iend + 1;
        }
    
    } /* ******* LOOP through images **************************************** */
    if ( verbose ) {
        for ( ; progress < PROGRESS_MAX; progress++)
            Rprintf (">");
        Rprintf(" *\n");
    }

    /* assign features */
    if ( INTEGER(_dodetect)[0] )
        assign_features (res, ref);
    
    UNPROTECT (nprotect);
    return res;
}

