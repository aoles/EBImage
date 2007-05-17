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
lib_matchFeatures (SEXP x, SEXP ref) {
    SEXP res, xf, * indexes;
    int nprotect, nx, ny, nz, i, ix, jy, im, nobj;
    double * data, * ftrs;

    if ( !isImage(x) || !isImage(ref) ) return x;

    nx = INTEGER ( GET_DIM(x) )[0];
    ny = INTEGER ( GET_DIM(x) )[1];
    nz = INTEGER ( GET_DIM(x) )[2];
    nprotect = 0;

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

    if ( nz > 1 ) {
      PROTECT (res = allocVector(VECSXP, nz) );
      nprotect++;
      for ( im = 0; im < nz; im++ )
        SET_VECTOR_ELT (res, im, indexes[im] );
    }
    else
      res = indexes[0];
    UNPROTECT (nprotect);
    return res;
}

/*----------------------------------------------------------------------- */
SEXP
lib_deleteFeatures (SEXP x, SEXP _index) {
    SEXP res, index;
    int nprotect, nx, ny, nz, i, j, im, nobj, * indexes, found;
    double * data;

    if ( !isImage(x) ) return x;

    nx = INTEGER ( GET_DIM(x) )[0];
    ny = INTEGER ( GET_DIM(x) )[1];
    nz = INTEGER ( GET_DIM(x) )[2];
    nprotect = 0;


    PROTECT ( res = Rf_duplicate(x) );
    nprotect++;
    SET_CLASS (res, mkString("IndexedImage") );

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

    }
    SET_SLOT (res, mkString("features"), allocVector(VECSXP, 0) );

    UNPROTECT (nprotect);
    return res;
}

/*----------------------------------------------------------------------- */
SEXP
lib_stackFeatures (SEXP obj, SEXP ref) {
    SEXP res, xf, * stacks, * dims, * modes, * comp, * fltrslt, *ftrslt, * resol;
    int nprotect, nx, ny, nz, i, im, nobj, dx, dy, ix, iy, x, y, colmode;
    double * data, * ftrs;
    double * refd, * stackd; // for grayscale ref
    int * refi, * stacki;    // for TrueColor ref

    if ( !isImage(obj) || !isImage(ref) ) return R_NilValue;

    colmode = INTEGER ( GET_SLOT(ref, mkString("colormode") ) )[0];

    nx = INTEGER ( GET_DIM(obj) )[0];
    ny = INTEGER ( GET_DIM(obj) )[1];
    nz = INTEGER ( GET_DIM(obj) )[2];
    nprotect = 0;

    stacks  = (SEXP *) R_alloc (nz, sizeof(SEXP) );
    dims    = (SEXP *) R_alloc (nz, sizeof(SEXP) );
    modes   = (SEXP *) R_alloc (nz, sizeof(SEXP) );
    comp    = (SEXP *) R_alloc (nz, sizeof(SEXP) );
    fltrslt = (SEXP *) R_alloc (nz, sizeof(SEXP) );
    ftrslt  = (SEXP *) R_alloc (nz, sizeof(SEXP) );
    resol   = (SEXP *) R_alloc (nz, sizeof(SEXP) );

    /* we need this to know the centres of objects in x */
    PROTECT (xf = get_features (obj) );
    nprotect++;

    for ( im = 0; im < nz; im++ ) {
        stacks[ im ] = R_NilValue;
        /* get image data */
        data = &( REAL(obj)[ im * nx * ny ] );
        /* get number of objects -- max index */
        nobj = 0;
        for ( i = 0; i < nx * ny; i++ )
            if ( data[i] > nobj ) nobj = data[i];

        if ( nobj < 1 || VECTOR_ELT(xf, im) == R_NilValue ) continue;
        /* check if features correspond to objects */
        if ( LENGTH( VECTOR_ELT(xf, im) ) != nobj * N_FEATURES ) continue;
        ftrs = REAL( VECTOR_ELT(xf, im) );
        /* get single object image size */
        dx = 0;
        dy = 0;
        for ( ix = 0; ix < nx; ix++ )
            for ( iy = 0; iy < ny; iy++ ) {
                i = (int)data[ ix + iy * nx ] - 1; // object index
                if ( i < 0 || i >= nobj) continue;
                x = (int)ftrs[ i ] - 1;
                y = (int)ftrs[ i + nobj ] - 1;
                if ( abs(x - ix) > dx )
                    dx = abs(x - ix);
                if ( abs(y - iy) > dy )
                    dy = abs(y - iy);
            }
        if ( dx < 1 || dy < 1 ) continue;
        /* create image */
        stacki = NULL; refi = NULL;
        stackd = NULL; refd = NULL;
        if ( colmode == MODE_RGB ) {
            PROTECT( stacks[im] = allocVector( INTSXP, (2 * dx + 1) * (2 * dy + 1) * nobj) );
            nprotect++;
            stacki = INTEGER( stacks[im] );
            for ( i = 0; i < (2 * dx + 1) * (2 * dy + 1) * nobj; i++ )
                stacki[ i ] = 0;
            refi = &( INTEGER(ref)[ im * nx * ny ] );
        }
        else {
            PROTECT( stacks[im] = allocVector( REALSXP, (2 * dx + 1) * (2 * dy + 1) * nobj) );
            nprotect++;
            stackd = REAL( stacks[im] );
            for ( i = 0; i < (2 * dx + 1) * (2 * dy + 1) * nobj; i++ )
                stackd[ i ] = 0.0;
            refd = &( REAL(ref)[ im * nx * ny ] );
        }
        /* class */
        SET_CLASS ( stacks[im], mkString("Image") );
        /* dim */
        PROTECT ( dims[im] = allocVector( INTSXP, 3 ) );
        nprotect++;
        INTEGER (dims[im])[0] = 2 * dx + 1;
        INTEGER (dims[im])[1] = 2 * dy + 1;
        INTEGER (dims[im])[2] = nobj;
        SET_DIM ( stacks[im], dims[im] ) ;
        /* attributes: colormode */
        PROTECT ( modes[im] = allocVector(INTSXP, 1) );
        nprotect++;
        INTEGER ( modes[im] )[0] = colmode;
        SET_SLOT ( stacks[im], mkString("colormode"), modes[im] );
        /* attributes: filename */
        SET_SLOT ( stacks[im], mkString("filename"), mkString("none") );
        /* copy attributes: compression */
        PROTECT ( comp[im] = allocVector(STRSXP, 1) );
        nprotect++;
        SET_STRING_ELT (comp[im], 0, mkChar("ZIP") );
        SET_SLOT ( stacks[im], mkString("compression"), comp[im] );
        /* copy attributes: filter */
        PROTECT ( fltrslt[im] = allocVector(STRSXP, 1) );
        nprotect++;
        SET_STRING_ELT ( fltrslt[im], 0, mkChar("lanczos") );
        SET_SLOT ( stacks[im], mkString("filter"), fltrslt[im] );
        /* copy attributes: resolution */
        PROTECT ( resol[im] = allocVector(REALSXP, 2) );
        nprotect++;
        REAL (resol[im])[0] = REAL ( GET_SLOT(ref, mkString("resolution") ) )[0];
        REAL (resol[im])[1] = REAL ( GET_SLOT(ref, mkString("resolution") ) )[1];
        SET_SLOT ( stacks[im], mkString("resolution"), resol[im] );
        /* empty feature list */
        PROTECT ( ftrslt[im] = allocVector(VECSXP, 0) );
        nprotect++;
        SET_CLASS ( ftrslt[im], mkString("list") );
        SET_SLOT ( stacks[im], mkString("features"), ftrslt[im] );

        /* copy ref into stacks */
        for ( ix = 0; ix < nx; ix++ )
            for ( iy = 0; iy < ny; iy++ ) {
                i = (int)data[ ix + iy * nx ] - 1; // object index
                if ( i < 0 || i >= nobj) continue;
                x = dx + 1 + (ix - (int)ftrs[ i ] + 1);
                y = dy + 1 + (iy - (int)ftrs[ i + nobj ] + 1);
                if ( colmode == MODE_RGB )
                    stacki[ x + (y + i * (2 * dy + 1)) * (2 * dx + 1) ] = refi[ ix + iy * nx ];
                else
                    stackd[ x + (y + i * (2 * dy + 1)) * (2 * dx + 1) ] = refd[ ix + iy * nx ];
            }
    }
    if ( nz > 1 ) {
      PROTECT (res = allocVector(VECSXP, nz) );
      nprotect++;
      for ( im = 0; im < nz; im++ )
        SET_VECTOR_ELT (res, im, stacks[im] );
    }
    else
      res = stacks[0];

    UNPROTECT (nprotect);
    return res;
}

