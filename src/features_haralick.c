#include "features_haralick.h"

/* -------------------------------------------------------------------------
Calculating Haralick image features
Copyright (c) 2007 Oleg Sklyar, Mike Smith
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tools.h"
#include <R_ext/Error.h>

# define  IND(I,X,Y) (X) + (Y)*(nc) + (I)*(nc)*(nc)

/* calculates haralick cooccurrence matrix ---------------------------------- */
SEXP
lib_co_occurrence (SEXP obj, SEXP ref, SEXP cgrades) {
  SEXP res, cm, dm;
  int nx, ny, nz, nprotect, im, x, y, nobj, index, i, nc, colthis, colthat, no_objects, * ncomp;
  double * data, * refdata, * cmdata;

  if ( !validImage(obj,1) || !validImage(ref,1) ) return R_NilValue;

  nx = INTEGER ( GET_DIM(obj) )[0];
  ny = INTEGER ( GET_DIM(obj) )[1];
  nz = getNumberOfFrames(obj,0);
  nprotect = 0;

  if ( INTEGER(GET_DIM(ref))[0] != nx || INTEGER(GET_DIM(ref))[1] != ny ||
       getNumberOfFrames(ref,0) != nz )
    error( "'ref' image has different size than 'obj'" );

  nc = INTEGER(cgrades)[0];
  if ( nc < 2 ) error( "the number of color grades must be larger than 1" );

  PROTECT ( res = allocVector(VECSXP, nz) );
  nprotect++;

  for ( im = 0; im < nz; im++ ) {
    /* get image data */
    data = &( REAL(obj)[ im * nx * ny ] );
    refdata = &( REAL(ref)[ im * nx * ny ] );
    /* get number of objects -- max index */
    nobj = 0;
    for ( index = 0; index < nx * ny; index++ )
      if ( data[index] > nobj ) nobj = floor( data[index] );
    if ( nobj < 1 ) {
      no_objects = 1;
      nobj = 0; /* if no objects, create a 0-row matrix */
    }
    else no_objects = 0;
    /* create features matrix */
    SET_VECTOR_ELT( res, im, (cm = allocVector(REALSXP, nobj * nc * nc)) );
    /* initialize feature matrix with 0 */
    cmdata = REAL( cm );
    for ( index = 0; index < nobj * nc * nc; index++ ) cmdata [index] = 0.0;
    /* set dimensions of the feature matrix */
    PROTECT( dm = allocVector(INTSXP, 3) );
    nprotect++;
    INTEGER( dm )[0] = nc;
    INTEGER( dm )[1] = nc;
    INTEGER( dm )[2] = nobj;
    SET_DIM( cm, dm );
    UNPROTECT( 1 ); nprotect--; // dm

    /* return empty matrix (go to next image) with 1 line if error (no objects) */
    if ( no_objects ) continue;

    /* number of comparisons for each object */
    ncomp = (int *) R_alloc (nobj, sizeof(int) );
    for ( index = 0; index < nobj; index++ )  ncomp[index] = 0;
    
    /* go through pixels and collect primary descriptors */
    /* reason to skip lines: we compare from this to: right, botoom, right-botoom, left-bottom */
    for ( x = 1; x < nx - 1; x++ )      // skip leftmost and rightmost cols
      for ( y = 0; y < ny - 1; y++ ) {  // skip bottom row
        index = floor( data[x + y * nx] ); /* index of the object, R-style = 1-based */
        if ( index < 1 ) continue;
        /* all indexes were 1, 2, 3, but C has 0-based indexes!!! */
        index--;
        colthis = floor(refdata[x + y * nx] * (nc - 1));
        /* we compare from this to: right, botoom, right-botoom, left-bottom */
        if ( data[x+1 + y * nx] - 1 == index ) {     // 1. right
          colthat = floor(refdata[x+1 + y * nx] * (nc - 1));
          cmdata[IND(index,colthis,colthat)] += 1.0;
          cmdata[IND(index,colthat,colthis)] += 1.0;
          ncomp[index] += 2;
        }
        if ( data[x + (y+1) * nx] - 1 == index ) {   // 2. bottom
          colthat = floor(refdata[x + (y+1) * nx] * (nc - 1));
          cmdata[IND(index,colthis,colthat)] += 1.0;
          cmdata[IND(index,colthat,colthis)] += 1.0;
          ncomp[index] += 2;
        }
        if ( data[x+1 + (y+1) * nx] - 1 == index ) { // 3. right-bottom
          colthat = floor(refdata[x+1 + (y+1) * nx] * (nc - 1));
          cmdata[IND(index,colthis,colthat)] += 1.0;
          cmdata[IND(index,colthat,colthis)] += 1.0;
          ncomp[index] += 2;
        }
        if ( data[x-1 + (y+1) * nx] - 1 == index ) { // 4. left-bottom
          colthat = floor(refdata[x-1 + (y+1) * nx] * (nc - 1));
          cmdata[IND(index,colthis,colthat)] += 1.0;
          cmdata[IND(index,colthat,colthis)] += 1.0;
          ncomp[index] += 2;
        }
      }
    for ( index = 0; index < nobj; index++ )
      for ( i = 0; i < nc * nc; i++ ) 
	    if ( ncomp[index] > 0 ) cmdata[i + index * nc * nc] /= ncomp[index];
  }

  UNPROTECT( nprotect );
  if ( nz == 1 ) return VECTOR_ELT(res, 0 );
  return res;
}

/* -------------------------------------------------------------------------- */
# define  SMALL 1e-7 //stops log2() breaking if the data is 0

/* given an array consisting of layers of square co-occurrence matrices,
   one layer per object, this function returns a matrix of selected haralick
   features (rows - objects, columns - features) -----------------------------*/
SEXP 
lib_haralick ( SEXP cm ) {
  SEXP res, dm;
  int nprotect, nc, nobj, index, i, j, n, nonZeros, no_objects, nf=13;
  double mu, tmp;
  double *p, *f;   // p for co-occurrence matrix -- probability; f for features
  double *px;      // partial probability density: rows summed, = py as matrix symmetrical
  double * Pxpy, * Pxmy;
  double HXY1, HXY2, entpx;  //used in calculating IMC1 and IMC2
  /* offsets in the res matrix for each feature, calculated from nobj */
  enum { ASM, CON, COR, VAR, IDM, SAV, SVA, SEN, ENT, DVA, DEN, IMC1, IMC2 };

  if ( cm == R_NilValue ) return R_NilValue;
  
  nprotect = 0;
  /* n = number of colors, nc, determined by the size of the haralick matrix */
  nc = INTEGER(GET_DIM(cm))[0];
  if ( nc != INTEGER(GET_DIM(cm))[1] || nc < 2 ) 
    error( "Haralick matrix is not square or too small" );
  /* return NULL if no objects */
  nobj = INTEGER(GET_DIM(cm))[2];

  if ( nobj < 1 ) return R_NilValue;
  /* after all the checks we hope we can collect the results, alloc mem */
  PROTECT( res = allocVector(REALSXP, nobj * nf) );
  nprotect++;
  f = REAL(res);
  for ( i = 0; i < nobj * nf; i++ ) f[i] = 0.0;
  PROTECT( dm = allocVector(INTSXP, 2) );
  nprotect++;
  INTEGER(dm)[0] = nobj;
  INTEGER(dm)[1] = nf;
  SET_DIM( res, dm );
  if ( nobj == 1) {
    /* check if all zeros (co-occurance matrix for 0 objects ), 
       return empty matrix if yes */
    no_objects = 1;
    p = REAL(cm); /* for the first object */
    for ( i = 0; i < nc && no_objects; i++ )
      for ( j = 0; j < nc && no_objects; j++ )
        if ( (tmp = p[i + j * nc]) > 0 ) no_objects = 0;
    if ( no_objects ) {
      UNPROTECT( nprotect );
      return res;
    }
  }
  /* temp vars */
  px   = (double *) R_alloc (nc, sizeof(double) );
  Pxpy = (double *) R_alloc(2 * nc + 10, sizeof(double) ); // +10 is to be safe with limits
  Pxmy = (double *) R_alloc(2 * nc + 10, sizeof(double) ); // +10 is to be safe with limits
  /* GO through objects and calculate features */
  for ( index = 0; index < nobj; index++ ) {
    p = &( REAL(cm)[index * nc * nc] );
    /* total number of non zeros to get mu,
       angular second moment (ASM),
       inverse diff mom      (IDM),
       entropy               (ENT) */
    /* calculate Pxpy and Pxmy */
      //  indexing (adding 1 to i and j) is based on Boland and netpbm,
      //  makes no sense to me
    nonZeros = 0;
    for ( i = 0; i < 2 * nc + 10; i++ ) Pxpy[i] = Pxmy[i] = 0;
    for ( i = 0; i < nc; i++ )
      for ( px[i]=0.0, j = 0; j < nc; j++ )
        if ( (tmp = p[i + j * nc]) > 0 ) {
          f[index + ASM * nobj] += tmp * tmp;
          f[index + IDM * nobj] += tmp / (double)(1 + (i-j)*(i-j));
          f[index + ENT * nobj] -= (tmp >= SMALL) ? (tmp * log10(tmp)) : (tmp * log10(SMALL));
          nonZeros++;
          Pxpy[i + j + 2] += tmp;
          Pxmy[abs(i - j)] += tmp;
          px[i] += tmp;
        }
    /* no sense to do anything if no non zero elements */
    if ( nonZeros < 1 ) continue; 
    /* contrast              (CON) */
    for ( n = 1; n < nc; n++ ) { // was from 0, but n^2 at n=0 is 0, so nonsense
      tmp = 0.0;
      for ( i = 0; i < nc; i++ )
        for ( j = 0; j < nc; j++ )
          if ( abs(i - j) == n ) tmp += p[i + j * nc];
      f[index + CON * nobj] += n * n * tmp;
    }
    /* correlation           (COR) */
//  the calculation of mu and sd is based on Boland and netpbm, 
//  but it does not make any sense to me
    /* reset px and recalculate it */
    /* the code assumes mux=muy, sdx=sdy */
    for ( mu=0, tmp=0,  i = 0; i < nc; i++ ) {
      mu += i * px[i];     // why is it this way, no idea
      tmp += i * i * px[i]; // sum of squares, why it is i^2 -- no idea
    }
    if ( tmp - mu * mu > 0 ) { // tmp - mu * mu = sd^2
      for ( i = 0; i < nc; i++ )
        for ( j = 0; j < nc; j++ ) f[index + COR * nobj] += i * j * p[i + j * nc];
      f[index + COR * nobj] = ( f[index + COR * nobj] - mu * mu ) / ( tmp - mu * mu );
    }
    /* variance             (VAR) */    
//  the calculation of mu is based on Boland and netpbm,
//  but it and indexing do not make any sense to me
    for ( mu=0,  i = 0; i < nc; i++ )
      for ( j = 0; j < nc; j++ ) mu += i * p[i + j * nc];
    for ( i = 0; i < nc; i++ )
      for ( j = 0; j < nc; j++ )
        f[index + VAR * nobj] += (i + 1 - mu) * (i + 1 - mu) * p[i + j * nc];
    /* sum average          (SAV)
       sum entropy          (SEN) */
    for ( i = 2; i <= 2 * nc; i++ ) {
      f[index + SAV * nobj] += i * Pxpy[i];
      f[index + SEN * nobj] -= (Pxpy[i] >= SMALL) ? (Pxpy[i] * log10(Pxpy[i])) : (Pxpy[i] * log10(SMALL));
    }
    /* sum variance         (SVA) */
    for ( i = 2; i <= 2 * nc; i++ )
      f[index + SVA * nobj] += (i - f[index + SEN * nobj]) * (i - f[index + SEN * nobj]) * Pxpy[i];
    /* difference Variance  (DVA)
       difference entropy   (DEN) */
    for ( i = 0; i < nc - 1; i++ ) { // top: Nc - 1
      f[index + DVA * nobj] += i * i * Pxmy[i];
      f[index + DEN * nobj] -= (Pxmy[i] >= SMALL) ? (Pxmy[i] * log10(Pxmy[i])) : (Pxmy[i] * log10(SMALL));
    }
    /* Info Measure of Correlation 1 and 2 */
    for ( HXY1=0, HXY2=0, entpx=0,   i = 0; i < nc; i++ ) {
      entpx -= (px[i] >= SMALL) ? (px[i] * log10(px[i])) : (px[i] * log10(SMALL));
      for( j = 0; j < nc; j++ ) {
        tmp = px[i] * px[j];
        HXY1 -= (tmp >= SMALL) ? (p[i + j * nc] * log10(tmp)) : (p[i + j * nc] * log10(SMALL));
        HXY2 -= (tmp >= SMALL) ? (tmp * log10(tmp)) : (tmp * log10(SMALL));
      }
    }
//    FIXME: why is the following negative (if fabs removed)?
    f[index + IMC1 * nobj] = (entpx != 0) ? ( fabs(f[index + ENT * nobj] - HXY1) / entpx) : 0.0;
    tmp = 1.0 - exp( -2.0 * (HXY2 - f[index + ENT * nobj]));
    f[index + IMC2 * nobj] = ( tmp >= 0 ) ? sqrt(tmp) : 0.0;
  }
  UNPROTECT( nprotect );
  return res;
}        
