#include "features_hull.h"

/* -------------------------------------------------------------------------
Calculating image moments from images and indexed images
Copyright (c) 2007 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include <R_ext/Error.h>

int _is_perimeter(int x, int y, int index, double * data, int nx, int ny) {
  if ( x+1 >= nx ) return 1; else if ( floor(data[(x+1) + y * nx]) != index + 1 ) return 1;
  if ( x-1 < 0 )   return 1; else if ( floor(data[(x-1) + y * nx]) != index + 1 ) return 1;
  if ( y+1 >= ny ) return 1; else if ( floor(data[x + (y+1) * nx]) != index + 1 ) return 1;
  if ( y-1 < 0 )   return 1; else if ( floor(data[x + (y-1) * nx]) != index + 1 ) return 1;
  return 0;
}

/* calculates basic hull features --------------------------------------- */
SEXP
lib_basic_hull (SEXP obj) {
  SEXP res, fm, dm;
  int nx, ny, nz, nprotect, im, x, y, nobj, index, nf=10;
  double * data, * fmdata, d;
  enum { XCT, YCT, SIZ, PER, PMN, PSD, EFR, ACR, SHF, EDG };

  nx = INTEGER ( GET_DIM(obj) )[0];
  ny = INTEGER ( GET_DIM(obj) )[1];
  nz = INTEGER ( GET_DIM(obj) )[2];
  nprotect = 0;

  PROTECT ( res = allocVector(VECSXP, nz) );
  nprotect++;

  for ( im = 0; im < nz; im++ ) {
    /* get image data */
    data = &( REAL(obj)[ im * nx * ny ] );
    /* get number of objects -- max index */
    nobj = 0;
    for ( index = 0; index < nx * ny; index++ )
      if ( data[index] > nobj ) nobj = floor( data[index] );
    if ( nobj < 1 ) {
      SET_VECTOR_ELT (res, im, R_NilValue );
      continue;
    }
    /* create features matrix */
    SET_VECTOR_ELT( res, im, (fm = allocVector(REALSXP, nobj * nf)) );
    /* initialize feature matrix with 0 */
    fmdata = REAL( fm );
    for ( index = 0; index < nobj * nf; index++ ) fmdata [index] = 0.0;
    /* set dimensions of the feature matrix */
    PROTECT ( dm = allocVector(INTSXP, 2) );
    nprotect++;
    INTEGER( dm )[0] = nobj;
    INTEGER( dm )[1] = nf;
    SET_DIM( fm, dm );
    UNPROTECT( 1 ); nprotect--;
    /* go through pixels and collect primary descriptors */
    for ( x = 0; x < nx; x++ )
      for ( y = 0; y < ny; y++ ) {
        index = floor( data[x + y * nx] ); /* index of the object, R-style = 1-based */
        if ( index < 1 ) continue;
        /* all indexes were 1, 2, 3, but C has 0-based indexes!!! */
        index--;
        /* update x, y accumulated -- need normalization later */
        fmdata [index + XCT * nobj] += x; // coordinates in C-style, 0-based
        fmdata [index + YCT * nobj] += y; // coordinates in C-style, 0-based
        /* size */
        fmdata [index + SIZ * nobj] += 1.0;
        /* per */
        if ( _is_perimeter(x, y, index, data, nx, ny) ) 
          fmdata [index + PER * nobj] += 1.0;
        /* edge */
        if ( x == 0 || y == 0 || x == nx - 1 || y == ny - 1 )
          fmdata [index + EDG * nobj] += 1.0;
      }
    /* calculate some secondary descriptors */
    for ( index = 0; index < nobj; index++ ) {
      if ( fmdata[index + SIZ * nobj] > 0 ) {
        /* normalize x, y */
        fmdata [index] /= fmdata[index + SIZ * nobj];
        fmdata [index + nobj] /= fmdata[index + SIZ * nobj];
        /* shape factor */
        fmdata [index + SHF * nobj] = 0.5 * fmdata [index + PER * nobj] / sqrt( M_PI * fmdata [index + SIZ * nobj] );
      }
      /* effr */
      fmdata [index + EFR * nobj] = sqrt ( fmdata[index + SIZ * nobj] / M_PI );
    }
    /* go through pixels and collect secondary descriptors */
    for ( x = 0; x < nx; x++ )
      for ( y = 0; y < ny; y++ ) {
        index = floor( data[x + y * nx] ); /* index of the object, R-style = 1-based */
        if ( index < 1 ) continue;
        /* all indexes were 1, 2, 3, but C has 0-based indexes!!! */
        index--;
        /* get the distance from the pixel to the centre of the corresponding object, in tools */
        d = sqrt( (x - fmdata[index + XCT * nobj])*(x - fmdata[index + XCT * nobj]) + 
                  (y - fmdata[index + YCT * nobj])*(y - fmdata[index + YCT * nobj]));
        /* acircularity */
        if ( d > fmdata[index + EFR * nobj] ) fmdata [index + ACR * nobj] += 1.0;
        /* per mean */
        if ( _is_perimeter(x, y, index, data, nx, ny) ) 
          fmdata [index + PMN * nobj] += d; 
      }
    /* update some secondary descriptors, 1st run */
    for ( index = 0; index < nobj; index++ ) {
      /* acircularity */
      if ( fmdata[index + SIZ * nobj] > 0 )
        fmdata [index + ACR * nobj] /= fmdata[index + SIZ * nobj];
      /* per mean */
      if ( fmdata[index + PER * nobj] > 0 )
        fmdata [index + PMN * nobj] /= fmdata[index + PER * nobj];
    }
    /* go through pixels and collect secondary descriptors, 2nd run (per sd) */
    for ( x = 0; x < nx; x++ )
      for ( y = 0; y < ny; y++ ) {
        index = floor( data[x + y * nx] ); /* index of the object, R-style = 1-based */
        if ( index < 1 ) continue;
        /* all indexes were 1, 2, 3, but C has 0-based indexes!!! */
        index--;
        /* is it a perimeter point */
        if ( !_is_perimeter(x, y, index, data, nx, ny) ) continue;
        /* get the distance from the pixel to the centre of the corresponding object, in tools */
        d = sqrt( (x - fmdata[index + XCT * nobj])*(x - fmdata[index + XCT * nobj]) + 
                  (y - fmdata[index + YCT * nobj])*(y - fmdata[index + YCT * nobj]));
        /* per sd sum squares here */
        fmdata [index + PSD * nobj] += (d - fmdata [index + PMN * nobj]) * (d - fmdata [index + PMN * nobj]);
      }
    /* update some secondary descriptors, 2nd run */
    for ( index = 0; index < nobj; index++ ) {
      /* per sd */
      if ( fmdata[index + PER * nobj] > 0 )
        fmdata [index + PSD * nobj] = sqrt(fmdata [index + PSD * nobj] / fmdata[index + PER * nobj]);
    }
  }

  UNPROTECT( nprotect );
  if ( nz == 1 ) return VECTOR_ELT( res, 0 );
  return res;
}

/* calculates edge profile ----------------------------------------------- */
/* partial calculation, the rest is done in R */
SEXP
lib_edge_profile (SEXP obj, SEXP xy_list) {
  SEXP res, edg, xys, pm, dm;
  int nx, ny, nz, nprotect, im, x, y, nobj, index, nper, counter;
  double * data, * edata, * xy, * pmdata, xx, yy;

  nx = INTEGER ( GET_DIM(obj) )[0];
  ny = INTEGER ( GET_DIM(obj) )[1];
  nz = INTEGER ( GET_DIM(obj) )[2];
  nprotect = 0;

  PROTECT( edg = Rf_duplicate(obj) );
  nprotect++;
  PROTECT ( res = allocVector(VECSXP, nz) );
  nprotect++;

  for ( im = 0; im < nz; im++ ) {
    /* get image data */
    data = &( REAL(obj)[ im * nx * ny ] );
    /* get number of objects -- max index */
    nobj = 0;
    for ( index = 0; index < nx * ny; index++ )
      if ( data[index] > nobj ) nobj = floor( data[index] );
    if ( nobj < 1 ) {
      SET_VECTOR_ELT (res, im, R_NilValue );
      continue;
    }
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
        index = floor( data[x + y * nx] ); /* index of the object, R-style = 1-based */
        if ( index < 1 ) continue;
        /* all indexes were 1, 2, 3, but C has 0-based indexes!!! */
        index--;
        if ( _is_perimeter(x, y, index, data, nx, ny) ) nper++;
        else edata[x + y * nx] = 0.0;
      }
    /* allocate memory */
    SET_VECTOR_ELT( res, im, (pm = allocVector(REALSXP, 3 * nper)) );
    pmdata = REAL( pm );
    for ( index = 0; index < 3 * nper; index++ ) pmdata[index] = 0;
    PROTECT( dm = allocVector(INTSXP, 2) );
    nprotect++;
    INTEGER( dm )[0] = nper;
    INTEGER( dm )[1] = 3;
    SET_DIM( pm, dm );
    UNPROTECT( 1 ); nprotect--;
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

  UNPROTECT( nprotect );
  if ( nz == 1 ) return VECTOR_ELT( res, 0 );
  return res;
}

