#include "features_moments.h"

/* -------------------------------------------------------------------------
Calculating image moments from images and indexed images
Copyright (c) 2007 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tools.h"
#include <R_ext/Error.h>

# define  IND(I,X,Y) (X) + (Y)*(N+1) + (I)*(N+1)*(N+1)

/* calculates size, M00, M10/M00, M01/M00 for all indexed objects */
/* obj is an Image here and ref is a grayscale one */
SEXP
lib_cmoments (SEXP obj, SEXP ref) { 
  int nprotect, nx, ny, nz, im, i, x, y, nobj, no_objects;
  double * data, * refd, * m, val;
  SEXP res, moments, dm, nm, dmnm;
  
  nx = INTEGER (GET_DIM(obj))[0];
  ny = INTEGER (GET_DIM(obj))[1];
  nz = getNumberOfFrames(obj,0);
  nprotect = 0;

  if (validImage(ref,1) )
    if ( INTEGER(GET_DIM(ref))[0] != nx || INTEGER(GET_DIM(ref))[1] != ny ||
         getNumberOfFrames(ref,0) != nz )
      error( "'ref' image is present, but has different size than 'obj'" );
  
  PROTECT( res = allocVector(VECSXP, nz) );
  nprotect++;

  /* prepare dimnames, will be duplicated */
  PROTECT( dmnm = allocVector(VECSXP, 2) );
  nprotect++;
  PROTECT( nm = allocVector(STRSXP, 4) );
  nprotect++;
  SET_STRING_ELT( nm, 0, mkChar("m.pxs") );
  SET_STRING_ELT( nm, 1, mkChar("m.int") );
  SET_STRING_ELT( nm, 2, mkChar("m.x") );
  SET_STRING_ELT( nm, 3, mkChar("m.y") );
  SET_VECTOR_ELT( dmnm, 1, nm );
  UNPROTECT( 1 ); nprotect--; // nm

  for ( im = 0; im < nz; im++ ) {
    /* get pointers */
    data = &( REAL(obj)[ im * nx * ny ] );
    if ( ref != R_NilValue ) refd = &( REAL(ref)[ im * nx * ny ] );
    else refd = NULL;
    
    nobj = 0;
    /* get nobj */
    for ( i = 0; i < nx * ny; i++ )
      if ( data[i] > nobj ) nobj = floor( data[i] );
    if ( nobj < 1 ) {
      no_objects = 1;
      nobj = 1; /* if no objects, create a matrix for 1 and fill all 0 */
      warning("Image contains no objects");
    }
    else no_objects = 0;
    /* create result storage */
    SET_VECTOR_ELT( res, im, (moments = allocVector(REALSXP, 4 * nobj)) );
    /* reset result */
    m = REAL( moments );
    for ( i = 0; i < 4 * nobj; i++ ) m[i] = 0.0;
    /* set dim, etc */
    PROTECT( dm = allocVector(INTSXP, 2) );
    nprotect++;
    INTEGER( dm )[0] = nobj;
    INTEGER( dm )[1] = 4;
    SET_DIM( moments, dm );
    UNPROTECT( 1 ); nprotect--;
    /* set dim names */
    setAttrib( moments, R_DimNamesSymbol, Rf_duplicate(dmnm) );

    /* return empty matrix (go to next image) with 1 line if error (no objects) */
    if ( no_objects ) continue;
    /* moment calculations for M00, M10, M01 */
    for ( x = 0; x < nx; x++ )
      for ( y = 0; y < ny; y++ ) {
        i = floor( data[ x + y * nx ] );
        if ( i < 1 ) continue;
        i--;
        if ( ref != R_NilValue )
          val = refd[ x + y * nx ];
        else
          val = 1.0;
        m[i           ] += 1.0;       /* pxs - size in pixels */
        m[i +     nobj] += val;       /* int / M00 - mass     */
        m[i + 2 * nobj] += x * val;   /* M10 - x moment       */
        m[i + 3 * nobj] += y * val;   /* M01 - y moment       */
      }
    
    /* convert M10, M01 to xm and ym */
    for ( i = 0; i < nobj; i++ ) {
      if ( (val = m[i + nobj]) == 0.0 ) continue;
      m[i + 2 * nobj] /= val;
      m[i + 3 * nobj] /= val;
    }
  }
  
  UNPROTECT( nprotect );
  if ( nz == 1 ) return VECTOR_ELT(res, 0 );
  return res;
}


/* calculates all requested moments */
/* obj is an Image here and ref is a grayscale one */
SEXP
lib_moments (SEXP obj, SEXP ref, SEXP pw, SEXP what) { 
  int nprotect, nx, ny, nz, im, i, x, y, ix, iy, nobj, N, alg;
  double * data, * refd, * m, * rm,* cm, dx, dy, val, powx, powy, tmp, * fct = NULL;
  SEXP res, ctrlist, ctr;
  SEXP moments, rmoments, dm, dmnm, nmx, nmy;
  char label[32];
  
  N   = INTEGER (pw)[0];
  alg = INTEGER (what)[0];

  nx  = INTEGER (GET_DIM(obj))[0];
  ny  = INTEGER (GET_DIM(obj))[1];
  nz  = getNumberOfFrames(obj,0);
  nprotect = 0;
  
  PROTECT( ctrlist = lib_cmoments(obj, ref) ); /* this also checks x and ref */
  nprotect++;

  strcpy(label, "X0");

  /* preset dim names, will be duplicate later */
  if ( alg < 3 ) {
    PROTECT( dmnm = allocVector(VECSXP, 3) );
    nprotect++;
    PROTECT( nmx = allocVector(STRSXP, N+1) );
    nprotect++;
    PROTECT( nmy = allocVector(STRSXP, N+1) );
    nprotect++;
    for ( int i = 0; i <= N; i++ ) {
      label[0] = 'x';
      label[1] = (char)(i+'0');
      label[2] = 0;
      SET_STRING_ELT( nmx, i, Rf_duplicate(mkChar(label)) ); 
      label[0] = 'y';
      SET_STRING_ELT( nmy, i, Rf_duplicate(mkChar(label)) ); 
    }
    SET_VECTOR_ELT( dmnm, 0, nmx );
    SET_VECTOR_ELT( dmnm, 1, nmy );
    UNPROTECT( 2 ); nprotect -= 2; // nmx, nmy
  }
  else {
    PROTECT( dmnm = allocVector(VECSXP, 2) );
    nprotect++;
    /* set dim colnames, all these vars unset and unused if !(alg < 3) */
    PROTECT( nmx = allocVector(STRSXP, 7) );
    nprotect++;
    label[0] = 'm';
    label[1] = '.';
    label[2] = 'I';
    label[4] = 0;
    for ( int i = 0; i < 7; i++ ) {
      label[3] = (char)(i+'1');
      SET_STRING_ELT( nmx, i, mkChar(label) ); 
    }
    SET_VECTOR_ELT( dmnm, 1, nmx );
    UNPROTECT( 1 ); nprotect--; // nmx
  }
  
  PROTECT( res = allocVector(VECSXP, nz) );
  nprotect++;

  for ( im = 0; im < nz; im++ ) {
    /* get pointers */
    data = &( REAL(obj)[ im * nx * ny ] );
    if ( ref != R_NilValue ) refd = &( REAL(ref)[ im * nx * ny ] );
    else refd = NULL;

    /* get centers from ctrlist, which is already set if nz == 1 */
    if ( nz > 1 ) ctr = VECTOR_ELT (ctrlist, im);
    else ctr = ctrlist;
    
    /* get nobj */
    if ( LENGTH(ctr) < 1 ) {
      /* nobj = 0, nothind to fill */
      SET_VECTOR_ELT( res, im, allocVector(REALSXP, 0) );
      continue;
    }
    nobj = INTEGER( GET_DIM(ctr) )[0]; /* will be 1 if no objects, empty matrix */
    cm = REAL (ctr);
    
    /* precalculate u00=1/M00^(1+(i+j)/2) for i+j >= 2 for all objects */
    if ( alg > 1 ) {
      fct = (double *) R_alloc ((2*N + 1) * nobj, sizeof(double) );
      for ( ix = 2; ix <= 2 * N; ix++ )
        for ( i = 0; i < nobj; i++ )
          if ( (tmp = pow( cm[i + nobj], 1 + 0.5*ix)) != 0.0 ) 
            fct[i + ix * nobj] = 1.0 / tmp;
    }
    
    /* create result storage */
    PROTECT( moments = allocVector(REALSXP, (N+1)*(N+1)*nobj) );
    nprotect++;
    /* reset result */
    m = REAL( moments );
    for ( i = 0; i < (N+1)*(N+1)*nobj; i++ ) m[i] = 0.0;

    PROTECT( dm = allocVector(INTSXP, 3) );
    nprotect++;
    INTEGER( dm )[0] = N+1;
    INTEGER( dm )[1] = N+1;
    INTEGER( dm )[2] = nobj;
    SET_DIM( moments, dm );
    UNPROTECT( 1 ); nprotect--; // dm

    /* set dim names if not rotation invariant, then different return */
    if ( alg < 3 )
      setAttrib( moments, R_DimNamesSymbol, Rf_duplicate(dmnm) );

    /* calculation of central moments */
    for ( x = 0; x < nx; x++ )
      for ( y = 0; y < ny; y++ ) {
        i = floor( data[ x + y * nx ] );
        if ( i < 1 ) continue;
        i--;
        if ( ref != R_NilValue )
          val = refd[ x + y * nx ];
        else
          val = 1.0;
        powx = 1.0; powy = 1.0;
        dx = x - cm[i + 2 * nobj];
        dy = y - cm[i + 3 * nobj];
        for ( ix = 0; ix <= N; ix++ ) {
          if ( ix == 0 ) powx = 1.0;
          else powx *= dx;
          for ( iy = 0; iy <= N; iy++ ) {
            if ( iy == 0 ) powy = 1.0;
            else powy *= dy;
            if ( ix + iy >= 2 && alg > 1 ) 
              m[IND(i,ix,iy)] += powx * powy * val * fct[ i + (ix + iy) * nobj ];
            else
              m[IND(i,ix,iy)] += powx * powy * val;
          }  
        }  
      }
      
    /* reset M01 and M10 */
    for ( i = 0; i < nobj; i++ ) {
      m[IND(i,1,0)] = 0.0;
      m[IND(i,0,1)] = 0.0;
    }
      
    if ( alg < 3 ) {
      SET_VECTOR_ELT( res, im, moments );
      UNPROTECT( 1 ); nprotect--; // moments if not used below
      continue;
    }
    
    /* rotation invariant moments, alg 3 means scale inv are done already: alg > 1 */
    PROTECT( rmoments = allocVector(REALSXP, 7 * nobj) );
    nprotect++;
    rm = REAL( rmoments );
    PROTECT( dm = allocVector(INTSXP, 2) );
    nprotect++;
    INTEGER( dm )[0] = nobj;
    INTEGER( dm )[1] = 7;
    SET_DIM( rmoments, dm );
    UNPROTECT( 1 ); nprotect--; // dm

    /* set dim names */
    setAttrib( rmoments, R_DimNamesSymbol, Rf_duplicate(dmnm) );
    
    /* calculate */
    for ( i = 0; i < nobj; i++ ) {
      /* I1 */
      rm[i] = m[IND(i,2,0)] + m[IND(i,0,2)];
      /* I2 */
      rm[i + nobj] = pow(m[IND(i,2,0)] - m[IND(i,0,2)], 2) + 4.0 * pow(m[IND(i,1,1)], 2);
      /* I3 */
      rm[i + 2*nobj] = pow(m[IND(i,3,0)] - 3.0 * m[IND(i,1,2)], 2) +
                       pow(3.0 * m[IND(i,2,1)] - m[IND(i,0,3)], 2);
      /* I4 */
      rm[i + 3*nobj] = pow(m[IND(i,3,0)] + m[IND(i,1,2)], 2) +
                       pow(m[IND(i,2,1)] + m[IND(i,0,3)], 2);
      /* I5 */
      rm[i + 4*nobj] = (m[IND(i,3,0)] - 3.0 * m[IND(i,1,2)]) * (m[IND(i,3,0)] + m[IND(i,1,2)]) *
                       (pow(m[IND(i,3,0)] + m[IND(i,1,2)], 2) - 3.0 * pow(m[IND(i,2,1)] + m[IND(i,0,3)], 2)) +
                       (3.0 * m[IND(i,2,1)] - m[IND(i,0,3)]) * (m[IND(i,2,1)] + m[IND(i,0,3)]) *
                       (3.0 * pow(m[IND(i,3,0)] + m[IND(i,1,2)], 2) - pow(m[IND(i,2,1)] + m[IND(i,0,3)], 2));
      /* I6 */
      rm[i + 5*nobj] = (m[IND(i,2,0)] - m[IND(i,0,2)]) * ( pow(m[IND(i,3,0)] + m[IND(i,1,2)], 2) -
                       pow(m[IND(i,2,1)] + m[IND(i,0,3)], 2) + 4.0 * m[IND(i,1,1)] * 
                       (m[IND(i,3,0)] + m[IND(i,1,2)]) * (m[IND(i,2,1)] + m[IND(i,0,3)]) );
      /* I7 */
      rm[i + 6*nobj] = (3.0 * m[IND(i,2,1)] - m[IND(i,0,3)]) * (m[IND(i,3,0)] + m[IND(i,1,2)]) *
                       (pow(m[IND(i,3,0)] + m[IND(i,1,2)], 2) - 3.0 * pow(m[IND(i,2,1)] + m[IND(i,0,3)], 2)) +
                       (m[IND(i,3,0)] - 3.0 * m[IND(i,1,2)]) * (m[IND(i,2,1)] + m[IND(i,0,3)]) *
                       (3.0 * pow(m[IND(i,3,0)] + m[IND(i,1,2)], 2) - pow(m[IND(i,2,1)] + m[IND(i,0,3)], 2));
    }
    SET_VECTOR_ELT( res, im, rmoments );
    UNPROTECT( 2 ); nprotect -= 2; // moments and rmoments
  }
  
  UNPROTECT( nprotect );
  if ( nz == 1 ) return VECTOR_ELT( res, 0 );
  return res;
}

