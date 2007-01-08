/* -------------------------------------------------------------------------
Counting objects determined in segmentations like watershed
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "common.h"

/*----------------------------------------------------------------------- */
#define BG 0.0
#define N_FEATURES 6

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
            DestroyImage (image);
        }
    }

    DestroyImage (colors);

    UNPROTECT (nprotect);
    return res;    
}

/*----------------------------------------------------------------------- */
SEXP
lib_assignFeatures (SEXP x, SEXP ref) {
    SEXP res;

    PROTECT ( res = Rf_duplicate(x) );
    assign_features (res, ref);
    UNPROTECT (1);
    return res;    
};

/*----------------------------------------------------------------------- */
/* will assign features field to the argument, modifying argument
   used in watershed and other routines. We assume that supplied x is
   ALREADY a duplicate of that sent from R, so we modify it */
void 
assign_features (SEXP x, SEXP ref) {
    SEXP features, * fm, * dm;
    int nx, ny, nz, nprotect, im, i, j, nobj, obj, perdone;
    double * data, * refdata, * fmdata;
    
    if ( !isImage(x) ) return;
    if ( !isImage(ref) && ref != R_NilValue ) return;
    
    nx = INTEGER ( GET_DIM(x) )[0];
    ny = INTEGER ( GET_DIM(x) )[1];
    nz = INTEGER ( GET_DIM(x) )[2];
    nprotect = 0;

    /* will be freed automatically */
    fm = (SEXP *) R_alloc (nz, sizeof(SEXP) );
    for ( im = 0; im < nz; im++ )
        fm[im] = R_NilValue;
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
                fmdata [obj] += i;
                fmdata [obj + nobj] += j;
                /* size (+2) */
                fmdata [obj + 2 * nobj] += 1.0;
                /* per (+3) */
                perdone = 0;
                if ( i > 0 )
                    if ( (int) data [i - 1 + j * nx] != obj + 1 ) { /* +1 because obj-- above */
                        perdone = 1;
                        fmdata [obj + 3 * nobj] += 1.0;
                    }
                if ( j > 0 && !perdone )
                    if ( (int) data [i + (j - 1) * nx] != obj + 1 ) { /* +1 because obj-- above */
                        perdone = 1;
                        fmdata [obj + 3 * nobj] += 1.0;
                    }
                if ( i < nx - 1 && !perdone )
                    if ( (int) data [i + 1 + j * nx] != obj + 1 ) { /* +1 because obj-- above */
                        perdone = 1;
                        fmdata [obj + 3 * nobj] += 1.0;
                    }
                if ( j < ny - 1 && !perdone )
                    if ( (int) data [i + (j + 1) * nx] != obj + 1 ) { /* +1 because obj-- above */
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
    }
    PROTECT ( features = allocVector(VECSXP, nz) );
    nprotect++;
    /* put fm into the list */
    for ( im = 0; im < nz; im++ )
        SET_VECTOR_ELT (features, im, fm[im] );
    SET_SLOT (x, mkString("features"), features);
    UNPROTECT(nprotect);
}

/*----------------------------------------------------------------------- */
/* takes an indexed image and reindexes it removing indexes that do not have
   corresponding objects. We assume that supplied x is
   ALREADY a duplicate of that sent from R, so we modify it */
   
void
remap_features (SEXP x) {

}

/*----------------------------------------------------------------------- */
SEXP
lib_combineFeatures (SEXP x, SEXP _ext, SEXP _factor) {
    SEXP res, features;
    int nprotect, nx, ny, nz, im, i, j, ix, jy, nobj, ext, from, to, * index, obj, leave;
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
    assign_features (res, R_NilValue);
    
    /* if we were not able to determine features correctly, do nothing */
    if ( LENGTH( GET_SLOT (res, mkString("features") ) ) != nz ) {
        UNPROTECT (nprotect);
        return x;
    }
    
    for ( im = 0; im < nz; im++ ) {
        /* get image data */
        data = &( REAL(res)[ im * nx * ny ] );
        /* get number of objects -- max index */
        nobj = 0;
        for ( i = 0; i < nx * ny; i++ )
            if ( data[i] > nobj ) nobj = data[i];
        /* nothing to combine */
        if ( nobj < 2 ) continue;
        /* check if features correspond to objects */
        features = VECTOR_ELT( GET_SLOT (res, mkString("features") ), im);
        if ( LENGTH( features ) != nobj * N_FEATURES ) continue;
        ftrs = REAL (features);
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
/* FIXME :debug output 

for(ix=0;ix<nobj;ix++)
  for(jy=0;jy<nobj;jy++)
    if (corr[ix+jy*nobj]>0)
      Rprintf("%d:%d - %f\n",ix,jy,corr[ix+jy*nobj]);

*/
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
            if ( from < 0 || to < 0 || from == to || lfactor < factor ) continue;
            /* set that from points to to ot to where to points is not otherwise already set */
            if ( corr[to + to * nobj] != from )
                corr [from + from * nobj] = corr[to + to * nobj];
            /* set all that point to from to point to whereever from points now */
            /* set also corr for all this from to 0 so we do not pick this one any more */
/* FIXME :debug output 

Rprintf("%d: Substitution: %d to %d (level: %f)\n",im+1,from,to,lfactor);
Rprintf("%d: now %d points to %d\n", im+1,from, (int)corr[from+from*nobj]);

*/
            corr [to + from * nobj] = 0;
            corr [from + to * nobj] = 0;
            for ( jy = 0; jy < nobj; jy++ )
                if ( jy != from && jy != to ) {
                    corr [from + jy * nobj] = 0; 
                    if ( corr[jy + jy * nobj] == from ) {
                        corr[jy + jy * nobj] = corr[from + from * nobj];
/* FIXME :debug output 

Rprintf("%d: now %d points to %d\n", im+1,jy, (int)corr[from+from*nobj]);

*/
                    }
                }
        } while ( lfactor >= factor && from >= 0 && to != from);
        
        /* reset the index */
        index = (int *) Calloc (nobj, int );
        to = 0;
        for ( i = 0; i < nobj; i++ ) {
            if ( corr[i + i * nobj] == i ) {
                index [i] = to;
                to++;
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
                obj = 0;
                leave = 0;
                for ( ix = i - ext; ix <= i + ext && !leave; ix++ )
                    for ( jy = j - ext; jy <= j + ext && !leave; jy++ ) {
                        if ( ix < 0 || jy < 0 || ix >= nx || jy >= ny || (ix == i && jy == j) ) continue;
                        if ( data[ ix + jy * nx] < 0.9) continue;
                        if ( obj == 0 ) 
                            obj = data[ ix + jy * nx];
                        else
                            if ( obj != data[ ix + jy * nx] ) leave = 1;
                    }
                if ( obj > 0 && !leave ) 
                    data [i + j * nx] = obj;
            }
        
    }
    
    SET_SLOT (res, mkString("features"), allocVector(VECSXP, 0) );

    UNPROTECT (nprotect);
    return res;
}

