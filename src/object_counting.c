/* -------------------------------------------------------------------------
Counting objects determined in segmentations like watershed
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "common.h"

/*----------------------------------------------------------------------- */
#define BG 0.0
#define N_FEATURES 6

/* will assign features field to the argument, modifying argument */
/* used in watershed and other routines */
void assign_features (SEXP x, SEXP ref) {
    SEXP features, * fm, * dm;
    int nx, ny, nz, nprotect, im, i, j, nobj, obj, perdone;
    double * data, * refdata, * fmdata;
    
    if ( !isImage(x) ) return;

    nx = INTEGER ( GET_DIM(x) )[0];
    ny = INTEGER ( GET_DIM(x) )[1];
    nz = INTEGER ( GET_DIM(x) )[2];
    nprotect = 0;

    PROTECT ( features = allocVector(VECSXP, nz) ); 
    nprotect++;
    SET_SLOT (x, mkString("features"), features);

    /* will be freed automatically */
    fm = (SEXP *) R_alloc (nz, sizeof(SEXP) );
    dm = (SEXP *) R_alloc (nz, sizeof(SEXP) );
    
    refdata = NULL;
    for ( im = 0; im < nz; im++ ) {
        /* get image data */
        data = &( REAL(x)[ im * nx * ny ] );
        if ( ref != R_NilValue )
            refdata = &( REAL(ref)[ im * nx * ny ] );
        /* get number of objects -- max index */
        nobj = 0;
        for ( i = 0; i < nx * ny; i++ )
            if ( data[i] > nobj ) nobj = data[i];
        /* create features matrix */
        PROTECT ( fm[im] = allocVector(REALSXP, nobj * N_FEATURES) );
        nprotect++;
        fmdata = REAL (fm[im]);
        for ( i = 0; i < nobj * N_FEATURES; i++ )
            fmdata [i] = 0.0;
        /* set dimensions of the feature matrix */
        PROTECT ( dm[im] = allocVector(INTSXP, 2) );
        nprotect++;
        INTEGER ( dm[im] )[0] = nobj;
        INTEGER ( dm[im] )[1] = N_FEATURES;
        SET_DIM ( fm[im], dm[im] );
        /* go through pixels and collect descriptors */
        for ( i = 0; i < nx; i++ )
            for ( j = 0; j < ny; j++ ) {
                obj = (int) data[i + j * nx];
                if ( obj < 1 ) continue;
                /* update x (+0), y (+1) */
                fmdata [obj] += i;
                fmdata [obj + nobj] += j;
                /* size (+2) */
                fmdata [obj + 2 * nobj] += 1.0;
                /* per (+3) */
                perdone = 0;
                if ( i > 0 )
                    if ( (int) data [i - 1 + j * nx] != obj ) {
                        perdone = 1;
                        fmdata [obj + 3 * nobj] += 1.0;
                    }
                if ( j > 0 && !perdone )
                    if ( (int) data [i + (j - 1) * nx] != obj ) {
                        perdone = 1;
                        fmdata [obj + 3 * nobj] += 1.0;
                    }
                if ( i < nx - 1 && !perdone )
                    if ( (int) data [i + 1 + j * nx] != obj ) {
                        perdone = 1;
                        fmdata [obj + 3 * nobj] += 1.0;
                    }
                if ( j < ny - 1 && !perdone )
                    if ( (int) data [i + (j + 1) * nx] != obj ) {
                        perdone = 1;
                        fmdata [obj + 3 * nobj] += 1.0;
                    }
                /* int (+4)  */
                if ( refdata && ref != R_NilValue ) 
                    fmdata [obj + 4 * nobj] += refdata [i + j * nx];
                /* edge (+5) */
                if ( i == 0 || j == 0 || i == nx - 1 || j == ny - 1 )
                    fmdata [obj + 5 * nobj] += 1.0;
                    
            }
        for ( obj = 0; obj < nobj; obj++ )
            if ( fmdata[obj + 2 * nobj] > 0 ) {
                fmdata [obj] = floor (fmdata [obj] / fmdata [obj + 2 * nobj] * 10.0) / 10.0;
                fmdata [obj + nobj] = floor (fmdata [obj + nobj] / fmdata [obj + 2 * nobj] * 10.0) / 10.0;
            }        
        /* put fm into the list */
        SET_VECTOR_ELT (features, im, fm[im] );
    }
    UNPROTECT(nprotect);
}

