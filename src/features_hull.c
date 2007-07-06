#include "features_hull.h"

/* -------------------------------------------------------------------------
Calculating image moments from images and indexed images
Copyright (c) 2007 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tools.h"
#include <R_ext/Error.h>

/* calculates basic hull features --------------------------------------- */
SEXP
lib_basic_hull (SEXP obj) {
  SEXP res, * fm, * dm;
  int nx, ny, nz, nprotect, im, x, y, nobj, index, is_per, nf=10;
  double * data, * fmdata, d;

  if ( !isImage(obj) ) return R_NilValue;
  nx = INTEGER ( GET_DIM(obj) )[0];
  ny = INTEGER ( GET_DIM(obj) )[1];
  nz = INTEGER ( GET_DIM(obj) )[2];
  nprotect = 0;

  /* will be freed automatically, list of feature matrices */
  fm = (SEXP *) R_alloc (nz, sizeof(SEXP) );
  for ( im = 0; im < nz; im++ ) fm[im] = R_NilValue;
  /* will be freed automatically, list of dimensions for feature matrices */
  dm = (SEXP *) R_alloc (nz, sizeof(SEXP) );

  for ( im = 0; im < nz; im++ ) {
    /* get image data */
    data = &( REAL(obj)[ im * nx * ny ] );
    /* get number of objects -- max index */
    nobj = 0;
    for ( index = 0; index < nx * ny; index++ )
      if ( data[index] > nobj ) nobj = data[index];
    if ( nobj < 1 ) continue;
    /* create features matrix */
    PROTECT ( fm[im] = allocVector(REALSXP, nobj * nf) );
    nprotect++;
    /* initialize feature matrix with 0 */
    fmdata = REAL (fm[im]);
    for ( index = 0; index < nobj * nf; index++ ) fmdata [index] = 0.0;
    /* set dimensions of the feature matrix */
    PROTECT ( dm[im] = allocVector(INTSXP, 2) );
    nprotect++;
    INTEGER ( dm[im] )[0] = nobj;
    INTEGER ( dm[im] )[1] = nf;
    SET_DIM ( fm[im], dm[im] );
    /* go through pixels and collect primary descriptors */
    for ( x = 0; x < nx; x++ )
      for ( y = 0; y < ny; y++ ) {
        index = data[x + y * nx]; /* index of the object, R-style = 1-based */
        if ( index < 1 ) continue;
        /* all indexes were 1, 2, 3, but C has 0-based indexes!!! */
        index--;
        /* update x (+0), y (+1), accumulated -- need normalization later */
        fmdata [index] += x; // coordinates in C-style, 0-based
        fmdata [index + nobj] += y; // coordinates in C-style, 0-based
        /* size (+2) */
        fmdata [index + 2 * nobj] += 1.0;
        /* is it a perimeter point */
        is_per = 0;
        // for nicer code we rely on C standard - if first (within one if) false, rest (in that if) is not checked
        if      ( x > 0             && (int) data [x-1 + y    *nx] - 1 != index ) is_per = 1;
        else if ( y > 0  && !is_per && (int) data [x   + (y-1)*nx] - 1 != index ) is_per = 1;
        else if ( x < nx && !is_per && (int) data [x+1 + y    *nx] - 1 != index ) is_per = 1;
        else if ( y < ny && !is_per && (int) data [x   + (y+1)*nx] - 1 != index ) is_per = 1;
        /* per (+3) */
        if ( is_per ) fmdata [index + 3 * nobj] += 1.0;
        /* edge (+9) */
        if ( x == 0 || y == 0 || x == nx - 1 || y == ny - 1 )
          fmdata [index + 9 * nobj] += 1.0;
      }
    /* calculate some secondary descriptors */
    for ( index = 0; index < nobj; index++ ) {
      if ( fmdata[index + 2 * nobj] > 0 ) {
        /* normalize x (+0), y (+1) */
        fmdata [index] /= fmdata[index + 2 * nobj];
        fmdata [index + nobj] /= fmdata[index + 2 * nobj];
        /* shape factor +8 */
        fmdata [index + 8 * nobj] = 0.5 * fmdata [index + 3 * nobj] / sqrt( M_PI * fmdata [index + 2 * nobj] );
      }
      /* effr +6 */
      fmdata [index + 6 * nobj] = sqrt ( fmdata[index + 2 * nobj] / M_PI );
    }
    /* go through pixels and collect secondary descriptors */
    for ( x = 0; x < nx; x++ )
      for ( y = 0; y < ny; y++ ) {
        index = data[x + y * nx]; /* index of the object, R-style = 1-based */
        if ( index < 1 ) continue;
        /* all indexes were 1, 2, 3, but C has 0-based indexes!!! */
        index--;
        /* get the distance from the pixel to the centre of the corresponding object, in tools */
        d = distancexy(x, y, fmdata[index], fmdata[index + nobj]);
        /* acircularity (+7) */
        if ( d > fmdata[index + 6 * nobj] ) fmdata [index + 7 * nobj] += 1.0;
        /* is it a perimeter point */
        is_per = 0;
        // for nicer code we rely on C standard - if first (within one if) false, rest (in that if) is not checked
        if      ( x > 0             && (int) data [x-1 + y    *nx] - 1 != index ) is_per = 1;
        else if ( y > 0  && !is_per && (int) data [x   + (y-1)*nx] - 1 != index ) is_per = 1;
        else if ( x < nx && !is_per && (int) data [x+1 + y    *nx] - 1 != index ) is_per = 1;
        else if ( y < ny && !is_per && (int) data [x   + (y+1)*nx] - 1 != index ) is_per = 1;
        if ( !is_per ) continue;
        /* per mean +4 */
        fmdata [index + 4 * nobj] += d;
      }
    /* update some secondary descriptors, 1st run */
    for ( index = 0; index < nobj; index++ ) {
      /* acircularity +7 */
      if ( fmdata[index + 2 * nobj] > 0 )
        fmdata [index + 7 * nobj] /= fmdata[index + 2 * nobj];
      /* per mean +4 */
      if ( fmdata[index + 3 * nobj] > 0 )
        fmdata [index + 4 * nobj] /= fmdata[index + 3 * nobj];
    }
    /* go through pixels and collect secondary descriptors, 2nd run (per sd) */
    for ( x = 0; x < nx; x++ )
      for ( y = 0; y < ny; y++ ) {
        index = data[x + y * nx]; /* index of the object, R-style = 1-based */
        if ( index < 1 ) continue;
        /* all indexes were 1, 2, 3, but C has 0-based indexes!!! */
        index--;
        /* is it a perimeter point */
        is_per = 0;
        // for nicer code we rely on C standard - if first (within one if) false, rest (in that if) is not checked
        if      ( x > 0             && (int) data [x-1 + y    *nx] - 1 != index ) is_per = 1;
        else if ( y > 0  && !is_per && (int) data [x   + (y-1)*nx] - 1 != index ) is_per = 1;
        else if ( x < nx && !is_per && (int) data [x+1 + y    *nx] - 1 != index ) is_per = 1;
        else if ( y < ny && !is_per && (int) data [x   + (y+1)*nx] - 1 != index ) is_per = 1;
        if ( !is_per ) continue;
        /* get the distance from the pixel to the centre of the corresponding object, in tools */
        d = distancexy(x, y, fmdata[index], fmdata[index + nobj]);
        /* per var +5 */
        fmdata [index + 5 * nobj] += (d - fmdata [index + 4 * nobj]) * (d - fmdata [index + 4 * nobj]);
      }
    /* update some secondary descriptors, 2nd run */
    for ( index = 0; index < nobj; index++ ) {
      /* per sd +5 */
      if ( fmdata[index + 3 * nobj] > 0 )
        fmdata [index + 5 * nobj] = sqrt(fmdata [index + 5 * nobj] / fmdata[index + 3 * nobj]);
    }
  }

  if ( nz == 1 ) {
    UNPROTECT( nprotect );
    return fm[0];
  }

  PROTECT ( res = allocVector(VECSXP, nz) );
  nprotect++;

  /* put fm into the list */
  for ( im = 0; im < nz; im++ )
   SET_VECTOR_ELT (res, im, fm[im] );

  UNPROTECT( nprotect );
  return res;
}

/* calculates edge profile ----------------------------------------------- */
/* partial calculation, the rest is done in R */
SEXP
lib_edge_profile (SEXP obj, SEXP xy_list) {
  SEXP res, edg, xys, * pm, * dm;
  int nx, ny, nz, nprotect, im, x, y, nobj, index, is_per, nper, counter;
  double * data, * edata, * xy, * pmdata, xx, yy;

  if ( !isImage(obj) || xy_list == R_NilValue) return R_NilValue;
  nx = INTEGER ( GET_DIM(obj) )[0];
  ny = INTEGER ( GET_DIM(obj) )[1];
  nz = INTEGER ( GET_DIM(obj) )[2];
  nprotect = 0;

  PROTECT( edg = Rf_duplicate(obj) );
  nprotect++;

  /* will be freed automatically, list of perimeter point matrices (keeps obj index, angle and distance) */
  pm = (SEXP *) R_alloc (nz, sizeof(SEXP) );
  for ( im = 0; im < nz; im++ ) pm[im] = R_NilValue;
  /* will be freed automatically, list of dimensions for feature matrices */
  dm = (SEXP *) R_alloc (nz, sizeof(SEXP) );

  for ( im = 0; im < nz; im++ ) {
    /* get image data */
    data = &( REAL(obj)[ im * nx * ny ] );
    /* get number of objects -- max index */
    nobj = 0;
    for ( index = 0; index < nx * ny; index++ )
      if ( data[index] > nobj ) nobj = data[index];
    if ( nobj < 1 ) continue;
    /* get xy */
    if ( nz == 1 ) xys = xy_list;
    else xys = VECTOR_ELT(xy_list, im);
    if ( xys == R_NilValue || INTEGER(GET_DIM(xys))[0] != nobj || INTEGER(GET_DIM(xys))[1] < 2 ) continue;
    xy = REAL(xys);
    /* get edge image data */
    edata = &( REAL(edg)[ im * nx * ny ] );
    nper = 0;
    /* unset all non-perimeter points */
    for ( x = 0; x < nx; x++ )
      for ( y = 0; y < ny; y++ ) {
        index = data[x + y * nx]; /* index of the object, R-style = 1-based */
        if ( index < 1 ) continue;
        /* all indexes were 1, 2, 3, but C has 0-based indexes!!! */
        index--;
        is_per = 0;
        if      ( x > 0             && (int) data [x-1 + y    *nx] - 1 != index ) is_per = 1;
        else if ( y > 0  && !is_per && (int) data [x   + (y-1)*nx] - 1 != index ) is_per = 1;
        else if ( x < nx && !is_per && (int) data [x+1 + y    *nx] - 1 != index ) is_per = 1;
        else if ( y < ny && !is_per && (int) data [x   + (y+1)*nx] - 1 != index ) is_per = 1;
        if ( is_per ) nper++;
        else edata[x + y * nx] = 0.0;
      }
    /* allocate memory */
    PROTECT( pm[im] = allocVector(REALSXP, 3 * nper) );
    nprotect++;
    pmdata = REAL( pm[im] );
    for ( index = 0; index < 3 * nper; index++ ) pmdata[index] = 0;
    PROTECT( dm[im] = allocVector(INTSXP, 2) );
    nprotect++;
    INTEGER( dm[im] )[0] = nper;
    INTEGER( dm[im] )[1] = 3;
    SET_DIM( pm[im], dm[im] );
    /* go through all edge points */
    counter = 0;
    for ( x = 0; x < nx; x++ )
      for ( y = 0; y < ny; y++ ) {
        index = edata[x + y * nx]; /* index of the object, R-style = 1-based */
        if ( index < 1 ) continue;
        /* all indexes were 1, 2, 3, but C has 0-based indexes!!! */
        index--;
        xx = xy[index];
        yy = xy[index + nobj];
        pmdata[counter] = index + 1;
        pmdata[counter + nper] = sqrt( (x - xx) * (x - xx) + (y - yy) * (y - yy) );
        pmdata[counter + 2 * nper] = atan2(y - yy, x - xx);
        counter++;
      }
  }

  if ( nz == 1 ) {
    UNPROTECT( nprotect );
    return pm[0];
  }

  PROTECT ( res = allocVector(VECSXP, nz) );
  nprotect++;

  /* put fm into the list */
  for ( im = 0; im < nz; im++ )
   SET_VECTOR_ELT (res, im, pm[im] );

  UNPROTECT( nprotect );
  return res;
}

