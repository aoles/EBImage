#include "object_counting.h"

/* -------------------------------------------------------------------------
Counting objects determined in segmentations like watershed
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tools.h"
#include "colors.h"
#include <R_ext/Error.h>
#include <magick/ImageMagick.h>

/*----------------------------------------------------------------------- */
#define BG 0.0
#define N_FEATURES     5
#define N_ALL_FEATURES     11

/* forward declarations */
SEXP get_all_features (SEXP, SEXP);
SEXP get_features (SEXP);

/*----------------------------------------------------------------------- */
/* will paint features on the target image with given colors and opacs    */
SEXP
lib_paintFeatures (SEXP x, SEXP tgt, SEXP _opac, SEXP _col) {
    SEXP res;
    Image * image, * colors;
    PixelPacket * pixelPtr, * colorPtr;
    int nprotect, nx, ny, nz, im, i, j, tgtmode, index;
    double * data, * imdata, * opac;

    if ( !isImage(x) || !isImage(tgt) ) return tgt;

    nx = INTEGER ( GET_DIM(x) )[0];
    ny = INTEGER ( GET_DIM(x) )[1];
    nz = INTEGER ( GET_DIM(x) )[2];
    nprotect = 0;

    PROTECT ( res = Rf_duplicate(tgt) );
    nprotect++;

    tgtmode = INTEGER ( GET_SLOT(tgt, mkString("colormode") ) )[0];
    opac = REAL (_opac);
    /* will keep colors in a small image -- easier to access - 3 values */
    colors = vector2image1D (_col);
    colorPtr = SetImagePixels (colors, 0, 0, 3, 1);
    for ( i = 0; i < 3; i++ ) {
        colorPtr[i].red *= opac[i];
        colorPtr[i].green *= opac[i];
        colorPtr[i].blue *= opac[i];
    }

    for ( im = 0; im < nz; im++ ) {
        imdata = &( REAL(x)[ im * nx * ny ] );
        for ( j = 0; j < ny; j++ ) {
            data = &( REAL(x)[ im * nx * ny + j * nx ] );
            image = NULL;
            if ( tgtmode == MODE_RGB )
                image = int2image1D ( &(INTEGER(res)[ im * nx * ny + j * nx ]), nx );
            if ( tgtmode == MODE_GRAY )
                image = double2image1D ( &(REAL(res)[ im * nx * ny + j * nx ]), nx );
            if ( image == NULL ) continue;
            for ( i = 0; i < nx; i++ ) {
                if ( data[i] <= 0 ) continue;
                pixelPtr = SetImagePixels (image, i, 0, 1, 1);
                index = 1;
                if ( data[i] < 1.0 || i < 1 || i > nx - 2 || j < 1 || j > ny - 2 )
                    /* pixel is contact */
                    index = 2;
                else
                    /* check if pixel is border, edge is same as contact */
                if ( imdata[ i - 1 + j * nx ] != data[i] || imdata[ i + 1 + j * nx ] != data[i] ||
                    imdata[ i + (j - 1) * nx ] != data[i] || imdata[ i + (j + 1) * nx ] != data[i] )
                    index = 0;
                if ( pixelPtr->red + colorPtr[index].red < QuantumRange )
                    pixelPtr->red += colorPtr[index].red;
                else
                    pixelPtr->red = QuantumRange;
                if ( pixelPtr->green + colorPtr[index].green < QuantumRange )
                    pixelPtr->green += colorPtr[index].green;
                else
                    pixelPtr->green = QuantumRange;
                if ( pixelPtr->blue + colorPtr[index].blue < QuantumRange )
                    pixelPtr->blue += colorPtr[index].blue;
                else
                    pixelPtr->blue = QuantumRange;
            }
            if ( tgtmode == MODE_RGB )
                image1D2int (image, &(INTEGER(res)[ im * nx * ny + j * nx ]), nx );
            if ( tgtmode == MODE_GRAY )
                image1D2double (image, &(REAL(res)[ im * nx * ny + j * nx ]), nx );
            image = DestroyImage (image);
        }
    }

    colors = DestroyImage (colors);

    UNPROTECT (nprotect);
    return res;
}

/*----------------------------------------------------------------------- */
SEXP
lib_getFeatures (SEXP x, SEXP ref) {
    return get_all_features (x, ref);
};

/*----------------------------------------------------------------------- */
SEXP
get_all_features (SEXP x, SEXP ref) {
    SEXP res, features, * fm, * dm;
    double * ftrs, * data, * refdata, * fmdata, thisdistance;
    int nprotect, nx, ny, nz, i, j, im, nobj, obj, isper;

    if ( !isImage(x) ) return R_NilValue;
    if ( !isImage(ref) && ref != R_NilValue ) return R_NilValue;

    nx = INTEGER ( GET_DIM(x) )[0];
    ny = INTEGER ( GET_DIM(x) )[1];
    nz = INTEGER ( GET_DIM(x) )[2];
    nprotect = 0;

     /* will be freed automatically */
    fm = (SEXP *) R_alloc (nz, sizeof(SEXP) );
    for ( im = 0; im < nz; im++ )
        fm[im] = R_NilValue;
    dm = (SEXP *) R_alloc (nz, sizeof(SEXP) );

    PROTECT ( features = get_features(x) );
    nprotect++;

    for ( im = 0; im < nz; im++ ) {
        if ( VECTOR_ELT(features, im) == R_NilValue ) continue;
        /* get image data */
        data = &( REAL(x)[ im * nx * ny ] );
        if ( ref != R_NilValue )
            refdata = &( REAL(ref)[ im * nx * ny ] );
        else
            refdata = NULL;
        /* get number of objects -- max index */
        nobj = 0;
        for ( i = 0; i < nx * ny; i++ )
            if ( data[i] > nobj ) nobj = data[i];
        /* nothing to do */
        if ( nobj < 1 ) continue;
        /* check if features correspond to objects */
        if ( LENGTH( VECTOR_ELT(features, im) ) != nobj * N_FEATURES ) continue;
        ftrs = REAL( VECTOR_ELT(features, im) );
        /* create features matrix */
        PROTECT ( fm[im] = allocVector(REALSXP, nobj * N_ALL_FEATURES) );
        nprotect++;
        fmdata = REAL (fm[im]);
        for ( obj = 0; obj < nobj; obj++ )
            for ( j = 0; j < N_ALL_FEATURES; j++ ) {
                if ( j < N_FEATURES )
                    fmdata [obj + j * nobj] = ftrs [obj + j * nobj];
                else
                /* get effr +5 */
                if ( j == 5 )
                    fmdata [obj + j * nobj] = sqrt (fmdata [obj + 2 * nobj] / M_PI );
                else
                    fmdata [obj + j * nobj] = 0;
            }
        /* set dimensions of the feature matrix */
        PROTECT ( dm[im] = allocVector(INTSXP, 2) );
        nprotect++;
        INTEGER ( dm[im] )[0] = nobj;
        INTEGER ( dm[im] )[1] = N_ALL_FEATURES;
        SET_DIM ( fm[im], dm[im] );
        /* go through pixels and collect descriptors, step 1 */
        for ( i = 0; i < nx; i++ )
            for ( j = 0; j < ny; j++ ) {
                obj = data[i + j * nx];
                if ( obj < 1 ) continue;
                /* all indexes were 1, 2, 3, but C has 0-based indexes!!! */
                obj--;
                // i + 1, j + 1 because of R-based indexes in coordinates
                thisdistance = distancexy(i + 1, j + 1, fmdata[obj], fmdata[obj + nobj]);
                /* int +6 */
                if ( refdata )
                    fmdata [obj + 6 * nobj] += refdata [i + j * nx];
                /* acirc +7 */
                if ( thisdistance > fmdata[obj + 5 * nobj] )
                    fmdata [obj + 7 * nobj] += 1.0;
                /* is it a perimeter point */
                isper = 0;
                if ( i > 0 )
                    if ( (int) data [i - 1 + j * nx] != obj + 1 ) /* +1 because obj-- above */
                        isper = 1;
                if ( j > 0 && !isper )
                    if ( (int) data [i + (j - 1) * nx] != obj + 1 ) /* +1 because obj-- above */
                        isper = 1;
                if ( i < nx - 1 && !isper )
                    if ( (int) data [i + 1 + j * nx] != obj + 1 )  /* +1 because obj-- above */
                        isper = 1;
                if ( j < ny - 1 && !isper )
                    if ( (int) data [i + (j + 1) * nx] != obj + 1 ) /* +1 because obj-- above */
                        isper = 1;
                if ( isper ) {
                    /* mean distance to perimeter +8 */
                    fmdata [obj + 8 * nobj] += thisdistance;
                }
            }
        /* update perimetr mean */
        for ( obj = 0; obj < nobj; obj++ )
            if ( fmdata[obj + 3 * nobj] != 0 )
                fmdata [obj + 8 * nobj] /= fmdata [obj + 3 * nobj];
        /* go through pixels and collect descriptors, step 2 */
        for ( i = 0; i < nx; i++ )
            for ( j = 0; j < ny; j++ ) {
                obj = data[i + j * nx];
                if ( obj < 1 ) continue;
                /* all indexes were 1, 2, 3, but C has 0-based indexes!!! */
                obj--;
                /* is it a perimeter point */
                isper = 0;
                if ( i > 0 )
                    if ( (int) data [i - 1 + j * nx] != obj + 1 ) /* +1 because obj-- above */
                        isper = 1;
                if ( j > 0 && !isper )
                    if ( (int) data [i + (j - 1) * nx] != obj + 1 ) /* +1 because obj-- above */
                        isper = 1;
                if ( i < nx - 1 && !isper )
                    if ( (int) data [i + 1 + j * nx] != obj + 1 )  /* +1 because obj-- above */
                        isper = 1;
                if ( j < ny - 1 && !isper )
                    if ( (int) data [i + (j + 1) * nx] != obj + 1 ) /* +1 because obj-- above */
                        isper = 1;
                if ( isper ) {
                    // i + 1, j + 1 because of R-based indexes in coordinates
                    thisdistance = distancexy(i + 1, j + 1, fmdata[obj], fmdata[obj + nobj]) - fmdata [obj + 8 * nobj];
                    /* perimeter pt SD by effr +9 */
                    fmdata [obj + 9 * nobj] += thisdistance * thisdistance;
                }
            }
        /* update new values that must be scaled by size or perimeter; round values */
        for ( obj = 0; obj < nobj; obj++ ) {
            if ( fmdata[obj + 2 * nobj] != 0 )
                fmdata [obj + 7 * nobj] /= fmdata [obj + 2 * nobj];
            if ( fmdata[obj + 3 * nobj] != 0 )
                fmdata [obj + 9 * nobj] = sqrt (fmdata [obj + 9 * nobj] / fmdata [obj + 3 * nobj]);
            if ( fmdata[obj + 5 * nobj] != 0 ) {
                fmdata [obj + 9 * nobj] /= fmdata[obj + 5 * nobj];
                /* per/2p by effr +10 */
                fmdata [obj + 10 * nobj] = fmdata[obj + 3 * nobj] / ( 2.0 * M_PI * fmdata[obj + 5 * nobj] );
            }
            /* round */
            for ( j = 5; j <= 10; j++ )
                fmdata[obj + j * nobj] = floor (fmdata[obj + j * nobj] * 1000.0) / 1000.0;
        }
    }

    PROTECT ( res = allocVector(VECSXP, nz) );
    nprotect++;

    /* put fm into the list */
    for ( im = 0; im < nz; im++ )
        SET_VECTOR_ELT (res, im, fm[im] );

    UNPROTECT (nprotect);
    return res;
}

/*----------------------------------------------------------------------- */
SEXP
get_features (SEXP x) {
    SEXP res, * fm, * dm;
    int nx, ny, nz, nprotect, im, i, j, nobj, obj, isper;
    double * data, * fmdata;

    if ( !isImage(x) ) return R_NilValue;

    nx = INTEGER ( GET_DIM(x) )[0];
    ny = INTEGER ( GET_DIM(x) )[1];
    nz = INTEGER ( GET_DIM(x) )[2];
    nprotect = 0;

    /* will be freed automatically */
    fm = (SEXP *) R_alloc (nz, sizeof(SEXP) );
    for ( im = 0; im < nz; im++ )
        fm[im] = R_NilValue;
    dm = (SEXP *) R_alloc (nz, sizeof(SEXP) );

    for ( im = 0; im < nz; im++ ) {
        /* get image data */
        data = &( REAL(x)[ im * nx * ny ] );
        /* get number of objects -- max index */
        nobj = 0;
        for ( i = 0; i < nx * ny; i++ )
            if ( data[i] > nobj ) nobj = data[i];
        if ( nobj < 1 ) continue;
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
                obj = data[i + j * nx];
                if ( obj < 1 ) continue;
                /* all indexes were 1, 2, 3, but C has 0-based indexes!!! */
                obj--;
                /* update x (+0), y (+1) */
                fmdata [obj] += i + 1; // +1 because of R-based indexes
                fmdata [obj + nobj] += j + 1; // +1 because of R-based indexes
                /* size (+2) */
                fmdata [obj + 2 * nobj] += 1.0;
                /* is it a perimeter point */
                isper = 0;
                if ( i > 0 )
                    if ( (int) data [i - 1 + j * nx] != obj + 1 ) /* +1 because obj-- above */
                        isper = 1;
                if ( j > 0 && !isper )
                    if ( (int) data [i + (j - 1) * nx] != obj + 1 ) /* +1 because obj-- above */
                        isper = 1;
                if ( i < nx - 1 && !isper )
                    if ( (int) data [i + 1 + j * nx] != obj + 1 )  /* +1 because obj-- above */
                        isper = 1;
                if ( j < ny - 1 && !isper )
                    if ( (int) data [i + (j + 1) * nx] != obj + 1 ) /* +1 because obj-- above */
                        isper = 1;
                /* per (+3) */
                if ( isper )
                    fmdata [obj + 3 * nobj] += 1.0;
                /* edge (+4) */
                if ( i == 0 || j == 0 || i == nx - 1 || j == ny - 1 )
                    fmdata [obj + 4 * nobj] += 1.0;
            }
        for ( obj = 0; obj < nobj; obj++ )
            if ( fmdata[obj + 2 * nobj] != 0 ) {
                /* x */
                fmdata [obj] = floor (fmdata [obj] / fmdata [obj + 2 * nobj] * 10.0) / 10.0;
                /* y */
                fmdata [obj + nobj] = floor (fmdata [obj + nobj] / fmdata [obj + 2 * nobj] * 10.0) / 10.0;
            }
    }
    PROTECT ( res = allocVector(VECSXP, nz) );
    nprotect++;

    /* put fm into the list */
    for ( im = 0; im < nz; im++ )
        SET_VECTOR_ELT (res, im, fm[im] );

    UNPROTECT(nprotect);
    return res;
}

/*----------------------------------------------------------------------- */
SEXP
lib_combineFeatures (SEXP x, SEXP _ext, SEXP _factor, SEXP _seeds) {
    SEXP res, features;
    int nprotect, nx, ny, nz, im, i, j, ix, jy, nobj, ext, from,
        to, * index, obj, leave, * seeds, nseeds, isseed;
    double * data, * ftrs, * corr, per, factor, lfactor;

    if ( !isImage(x) ) return x;

    nx = INTEGER ( GET_DIM(x) )[0];
    ny = INTEGER ( GET_DIM(x) )[1];
    nz = INTEGER ( GET_DIM(x) )[2];
    nprotect = 0;

    ext = INTEGER (_ext)[0];
    if ( ext < 1 ) return x;

    factor = REAL (_factor)[0];

    PROTECT ( res = Rf_duplicate(x) );
    nprotect++;

    /* we need this to know about perimeters etc of currently existing objects */
    PROTECT (features = get_features (res) );
    nprotect++;

    /* if we were not able to determine features correctly, do nothing */
    if ( LENGTH( features ) != nz ) {
        UNPROTECT (nprotect);
        return x;
    }

    for ( im = 0; im < nz; im++ ) {
        if ( VECTOR_ELT(features, im) == R_NilValue ) continue;
        /* get image data */
        data = &( REAL(res)[ im * nx * ny ] );
        /* get number of objects -- max index */
        nobj = 0;
        for ( i = 0; i < nx * ny; i++ )
            if ( data[i] > nobj ) nobj = data[i];
        /* nothing to combine */
        if ( nobj < 2 ) continue;
        /* check if features correspond to objects */
        if ( LENGTH( VECTOR_ELT(features, im) ) != nobj * N_FEATURES ) continue;
        ftrs = REAL( VECTOR_ELT(features, im) );
        /* allocate a square matrix of nobj*nobj which serves as a correlation
        matrix, this is NOT freed automatically  */
        corr = (double *) Calloc (nobj * nobj, double );
        for ( i = 0; i < nobj * nobj; i++ )
            corr [i] = 0;
        /* loop through all 0.1 < val < 0.9 -- edge pixels and add correlations */
        for ( i = 0; i < nx; i++ )
            for ( j = 0; j < ny; j++ ) {
                if ( data[ i + j * nx ] < 0.1 || data[ i + j * nx ] > 0.9 ) continue;
                /* reset the diag of the corr matrix as all found objects go there at each step */
                for ( ix = 0; ix < nobj; ix++ )
                    corr [ix + ix * nobj] = 0;
                /* scan a square this+-ext and add objects, put number of all found objects into
                   the diagonal of the corr matrix*/
                for ( ix = i - ext; ix <= i + ext; ix++ )
                    for ( jy = j - ext; jy <= j + ext; jy++ ) {
                        if ( ix < 0 || jy < 0 || ix >= nx || jy >= ny || (ix == i && jy == j) ) continue;
                        obj = data [ix + jy * nx];
                        if ( obj < 1 ) continue;
                        /* all indexes were 1, 2, 3, but C has 0-based indexes!!! */
                        obj--;
                        corr [ obj + obj * nobj ] = 1;
                    }
                /* add correlations to the matrix */
                for ( ix = 0; ix < nobj; ix++) {
                    if ( corr[ix + ix * nobj] < 1 ) continue;
                    for ( jy = 0; jy < nobj; jy++ ) {
                        if ( ix == jy || corr[jy + jy * nobj] < 1 ) continue;
                        corr [ ix + jy * nobj ] += 1;
                    }
                }
            }
        /* substitute corr values with corr / (ext * per) */
        for ( ix = 0; ix < nobj; ix ++ ) {
            per = ftrs [ix + 3 * nobj];
            if ( per == 0.0 ) continue;
            for ( jy = 0; jy < nobj; jy++ )
                if ( ix != jy )
                    corr [ix + jy * nobj] /= (double)(per * ext);
        }
        /* reset the diag of the corr matrix -- these will now indicate substitutions */
        for ( ix = 0; ix < nobj; ix++ )
            corr [ix + ix * nobj] = ix;
        /* if we have seeds, reset <to> fields to 0 for all that are not in seeds */
        if ( _seeds != R_NilValue ) {
            seeds = INTEGER( VECTOR_ELT(_seeds, im) );
            nseeds = LENGTH( VECTOR_ELT(_seeds, im) );
            for ( jy = 0; jy < nobj; jy++ ) {
                isseed = 0;
                for ( i = 0; i < nseeds && !isseed; i++ )
                    if ( jy + 1 == seeds[i] ) isseed = 1;
                if ( !isseed ) {
                    for ( ix = 0; ix < nobj; ix++ )
                        corr[ix + jy * nobj] = 0;
                }
            }
        }
        /* combine objects with the largest corr to their counterparts */
        /* for each object ix search if it can be combined with jy */
        do {
            from = -1;
            to = -1;
            lfactor = 0.0;
            for ( ix = 0; ix < nobj; ix++ )
                for ( jy = 0; jy < nobj; jy++ ) {
                    if ( ix == jy ) continue;
                    if ( corr[ ix + jy * nobj] > lfactor ) {
                        lfactor = corr[ ix + jy * nobj];
                        from = ix;
                        to = jy;
                    }
                }
            if ( from < 0 || to < 0 || from == to || lfactor < factor ) break;
            /* set that from points to to ot to where to points is not otherwise already set */
            if ( corr[to + to * nobj] != from )
                corr [from + from * nobj] = corr[to + to * nobj];
            /* set all that point to from to point to whereever from points now */
            /* set also corr for all this from to 0 so we do not pick this one any more */
            corr [to + from * nobj] = 0;
            corr [from + to * nobj] = 0;
            for ( jy = 0; jy < nobj; jy++ )
                if ( jy != from && jy != to ) {
                    corr [from + jy * nobj] = 0;
                    if ( corr[jy + jy * nobj] == from )
                        corr[jy + jy * nobj] = corr[from + from * nobj];
                }
        } while ( lfactor >= factor && from >= 0 && to != from);

        /* reset the index */
        index = (int *) Calloc (nobj, int );
        j = 0;
        for ( i = 0; i < nobj; i++ ) {
            if ( corr[i + i * nobj] == i ) {
                index [i] = j;
                j++;
            }
            else
                index [i] = -1;
        }
        for ( i = 0; i < nobj; i++ ) {
            if ( corr [i + i * nobj] < 0 ) continue;
            index [i] = index[ (int)corr[i + i * nobj] ];
        }
        /* we got index, do not need corr any more */
        Free (corr);

        /* reset image values according to new index */
        for ( i = 0; i < nx * ny; i++ ) {
            if ( data[i] < 0.9 ) continue;
            data[i] = index[ (int)data[i] - 1 ] + 1;
        }
        Free (index);
        /* reset borders if it is only for the combined objects */
        for ( i = 0; i < nx; i++ )
            for ( j = 0; j < ny; j++ ) {
                if ( data[ i + j * nx ] < 0.1 || data[ i + j * nx ] > 0.9 ) continue;
                obj = -1;
                leave = 0;
                for ( ix = i - ext; ix <= i + ext && !leave; ix++ )
                    for ( jy = j - ext; jy <= j + ext && !leave; jy++ ) {
                        if ( ix < 0 || jy < 0 || ix >= nx || jy >= ny || (ix == i && jy == j) ) continue;
                        if ( 0.1 < data[ ix + jy * nx] && data[ ix + jy * nx] < 0.9) continue;
                        if ( obj <= 0 )
                            obj = data[ ix + jy * nx];
                        else
                            if ( obj > 0 && obj != data[ ix + jy * nx] ) leave = 1;
                    }
                if ( obj >= 0 && !leave )
                    data [i + j * nx] = obj;
            }

    }

    SET_SLOT (res, mkString("features"), allocVector(VECSXP, 0) );

    UNPROTECT (nprotect);
    return res;
}

/*----------------------------------------------------------------------- */
SEXP
lib_matchFeatures (SEXP x, SEXP ref) {
    SEXP res, xf, * indexes;
    int nprotect, nx, ny, nz, i, ix, jy, im, nobj;
    double * data, * ftrs;

    if ( !isImage(x) || !isImage(ref) ) return x;

    nx = INTEGER ( GET_DIM(x) )[0];
    ny = INTEGER ( GET_DIM(x) )[1];
    nz = INTEGER ( GET_DIM(x) )[2];
    nprotect = 0;

    PROTECT (res = allocVector(VECSXP, nz) );
    nprotect++;
    indexes = (SEXP *) R_alloc (nz, sizeof(SEXP) );

    /* we need this to know centres of objects in x */
    PROTECT (xf = get_features (x) );
    nprotect++;

    for ( im = 0; im < nz; im++ ) {
        /* get image data */
        data = &( REAL(x)[ im * nx * ny ] );
        /* get number of objects -- max index */
        nobj = 0;
        for ( i = 0; i < nx * ny; i++ )
            if ( data[i] > nobj ) nobj = data[i];
        /* create results vector */
        PROTECT ( indexes[im] = allocVector(INTSXP, nobj) );
        nprotect++;
        /* add it to res */
        SET_VECTOR_ELT (res, im, indexes[im] );
        if ( nobj < 1 ) continue;
        if ( VECTOR_ELT(xf, im) == R_NilValue ) continue;
        /* check if features correspond to objects */
        if ( LENGTH( VECTOR_ELT(xf, im) ) != nobj * N_FEATURES ) continue;
        ftrs = REAL( VECTOR_ELT(xf, im) );
        /* reset data to ref */
        data = &( REAL(ref)[ im * nx * ny ] );

        /* scan through objects, collect indexes */
        for ( i = 0; i < nobj; i++ ) {
            ix = ftrs [i] - 1; // from R-based indexes
            jy = ftrs [i + nobj] - 1; // from R-based indexes
            INTEGER (indexes[im])[i] = NA_INTEGER;
            if ( ix >= 0 && jy >= 0 && ix < nx && jy < ny )
                if ( data[ix + jy * nx] > 0.9 )
                    INTEGER (indexes[im])[i] = (int)data[ix + jy * nx];

        }
    }

    UNPROTECT (nprotect);
    return res;
}

/*----------------------------------------------------------------------- */
SEXP
lib_deleteFeatures (SEXP x, SEXP _index, SEXP _ext) {
    SEXP res, index;
    int nprotect, nx, ny, nz, i, j, ix, jy, im, nobj, * indexes, found, ext, leave, obj;
    double * data;

    if ( !isImage(x) ) return x;

    nx = INTEGER ( GET_DIM(x) )[0];
    ny = INTEGER ( GET_DIM(x) )[1];
    nz = INTEGER ( GET_DIM(x) )[2];
    nprotect = 0;

    ext = INTEGER (_ext)[0];
    if ( ext < 1 ) return x;


    PROTECT ( res = Rf_duplicate(x) );
    nprotect++;

    for ( im = 0; im < nz; im++ ) {
        /* get image data */
        data = &( REAL(res)[ im * nx * ny ] );
        index = VECTOR_ELT (_index, im);
        /* get number of objects -- max index */
        nobj = 0;
        for ( i = 0; i < nx * ny; i++ )
            if ( data[i] > nobj ) nobj = data[i];
        indexes = (int *) Calloc (nobj, int );
        for ( i = 0; i < nobj; i++ ) {
            found = 0;
            for ( j = 0; j < LENGTH(index) && !found; j++ )
                if ( i + 1 == INTEGER(index)[j] )
                    found = 1;
            if ( found )
                indexes[i] = 0;
            else
                indexes[i] = i + 1;
        }
        /* shring indexes */
        j = 1;
        for ( i = 0; i < nobj; i++ ) {
            if ( indexes[i] > 0 ) {
                indexes[i] = j;
                j++;
            }
        }
        /* reset image */
        for ( i = 0; i < nx * ny; i++ ) {
            if ( data[i] < 0.9 ) continue;
            data [i] = indexes[ (int)data[i] - 1 ];
        }
        Free (indexes);

        /* reset borders if left after deleting objects */
        for ( i = 0; i < nx; i++ )
            for ( j = 0; j < ny; j++ ) {
                if ( data[ i + j * nx ] < 0.1 || data[ i + j * nx ] > 0.9 ) continue;
                obj = -1;
                leave = 0;
                for ( ix = i - ext; ix <= i + ext && !leave; ix++ )
                    for ( jy = j - ext; jy <= j + ext && !leave; jy++ ) {
                        if ( ix < 0 || jy < 0 || ix >= nx || jy >= ny || (ix == i && jy == j) ) continue;
                        if ( 0.1 < data[ ix + jy * nx] && data[ ix + jy * nx] < 0.9) continue;
                        if ( obj <= 0 )
                            obj = data[ ix + jy * nx];
                        else
                            if ( obj > 0 && obj != data[ ix + jy * nx] ) leave = 1;
                    }
                if ( obj >= 0 && !leave )
                    data [i + j * nx] = obj;
            }

    }
    SET_SLOT (res, mkString("features"), allocVector(VECSXP, 0) );

    UNPROTECT (nprotect);
    return res;
}

