#include "features_moments.h"

/* -------------------------------------------------------------------------
Calculating image moments from images and indexed images
Copyright (c) 2007 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tools.h"
#include <R_ext/Error.h>

/* calculates size, M00, M10/M00, M01/M00 for all indexed objects */
/* obj is an IndexedImage here and ref is a grayscale one */
SEXP
lib_cmoments (SEXP obj, SEXP ref) { 
  int nprotect, nx, ny, nz, im, i, x, y, nobj;
  double * data, * refd, * m, val;
  SEXP res, * moments, * dims, * dimnames, * names;
  
  nx = INTEGER (GET_DIM(obj))[0];
  ny = INTEGER (GET_DIM(obj))[1];
  nz = INTEGER (GET_DIM(obj))[2];

  if ( isImage(ref) )
    if ( INTEGER(GET_DIM(ref))[0] != nx || INTEGER(GET_DIM(ref))[1] != ny ||
         INTEGER(GET_DIM(ref))[2] != nz )
      error( "'ref' image is present, but has different size from 'obj'" );
  nprotect = 0;

  moments  = (SEXP *) R_alloc(nz, sizeof(SEXP));
  dims     = (SEXP *) R_alloc(nz, sizeof(SEXP));
  dimnames = (SEXP *) R_alloc(nz, sizeof(SEXP));
  names    = (SEXP *) R_alloc(nz, sizeof(SEXP));

  for ( im = 0; im < nz; im++ ) {
    /* get pointers */
    data = &( REAL(obj)[ im * nx * ny ] );
    if ( ref != R_NilValue )
      refd = &( REAL(ref)[ im * nx * ny ] );
    else
      refd = NULL;
    
    nobj = 0;
    /* get nobj */
    for ( i = 0; i < nx * ny; i++ )
      if ( data[i] > nobj ) nobj = (int) data[i];
    /* create result storage */
    PROTECT( moments[im] = allocVector(REALSXP, 4 * nobj) );
    nprotect++;
    /* if no data to fill, continue */
    if ( nobj == 0 ) continue;
    PROTECT( dims[im] = allocVector(INTSXP, 2) );
    nprotect++;
    INTEGER( dims[im] )[0] = nobj;
    INTEGER( dims[im] )[1] = 4;
    SET_DIM( moments[im], dims[im] );

    /* set dim names */
    PROTECT( names[im] = allocVector(STRSXP, 4) );
    nprotect++;
    SET_STRING_ELT( names[im], 0, mkChar("pxs") );
    SET_STRING_ELT( names[im], 1, mkChar("int") );
    SET_STRING_ELT( names[im], 2, mkChar("x") );
    SET_STRING_ELT( names[im], 3, mkChar("y") );
    PROTECT( dimnames[im] = allocVector(VECSXP, 2) );
    nprotect++;
    SET_VECTOR_ELT( dimnames[im], 1, names[im] );
    setAttrib( moments[im], R_DimNamesSymbol, dimnames[im] );

    /* reset result */
    m = REAL( moments[im] );
    for ( i = 0; i < 4 * nobj; i++ ) m[i] = 0.0;

    /* moment calculations for M00, M10, M01 */
    for ( x = 0; x < nx; x++ )
      for ( y = 0; y < ny; y++ ) {
        i = (int) data[ x + y * nx ];
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
      if ( m[i + nobj] == 0.0 ) continue;
      m[i + 2 * nobj] /= m[i + nobj];
      m[i + 3 * nobj] /= m[i + nobj];
    }
  }
  
  if ( im > 1 ) {
    PROTECT( res = allocVector(VECSXP, nz) );
    nprotect++;
    for ( im = 0; im < nz; im++ )
      SET_VECTOR_ELT(res, im, moments[im] );
  }
  else
    res = moments[0];
    
  UNPROTECT( nprotect );
  return res;
}


/* calculates all requested moments */
/* obj is an IndexedImage here and ref is a grayscale one */
SEXP
lib_moments (SEXP obj, SEXP ref, SEXP pw, SEXP what) { 
  int nprotect, nx, ny, nz, im, i, x, y, ix, iy, nobj, N, alg;
  double * data, * refd, * m, * rm,* cm, dx, dy, val, powx, powy, * fct = NULL;
  SEXP res, ctrlist, ctr;
  SEXP * moments, * dims, * dimnames, * namesx, * namesy, * rmoments = NULL, * rdims = NULL;
  char label[3] = "X0";
  
  nx  = INTEGER (GET_DIM(obj))[0];
  ny  = INTEGER (GET_DIM(obj))[1];
  nz  = INTEGER (GET_DIM(obj))[2];
  N   = INTEGER (pw)[0];
  alg = INTEGER (what)[0];
  nprotect = 0;
  
  PROTECT( ctrlist = lib_cmoments(obj, ref) ); /* this also checks x and ref */
  nprotect++;
  ctr = ctrlist; /* this remains unchanged if nz == 1, matrix not a list */

  moments  = (SEXP *) R_alloc(nz, sizeof(SEXP));
  dims     = (SEXP *) R_alloc(nz, sizeof(SEXP));
  dimnames = (SEXP *) R_alloc(nz, sizeof(SEXP));
  namesx   = (SEXP *) R_alloc(nz, sizeof(SEXP));
  namesy   = (SEXP *) R_alloc(nz, sizeof(SEXP));
  if ( alg > 2 ) { /* rotation invariant moments */
    rmoments = (SEXP *) R_alloc(nz, sizeof(SEXP));
    rdims    = (SEXP *) R_alloc(nz, sizeof(SEXP));
  }

# define  IND(I,X,Y) (X) + (Y)*(N+1) + (I)*(N+1)*(N+1)

  for ( im = 0; im < nz; im++ ) {
    /* get pointers */
    data = &( REAL(obj)[ im * nx * ny ] );
    if ( ref != R_NilValue )
      refd = &( REAL(ref)[ im * nx * ny ] );
    else
      refd = NULL;

    /* get centers from ctrlist, which is already set if nz == 1 */
    if ( nz > 1 )
      ctr = VECTOR_ELT (ctrlist, im);
    /* get nobj */
    if ( LENGTH(ctr) < 1 ) {
      /* nobj = 0, nothind to fill */
      PROTECT( moments[im] = allocVector(REALSXP, 0) );
      nprotect++;
      continue;
    }
    nobj = INTEGER( GET_DIM(ctr) )[0];
    cm = REAL (ctr);
    
    /* precalculate u00=1/M00^(1+(i+j)/2) for i+j >= 2 for all objects */
    if ( alg > 1 ) {
      fct = (double *) R_alloc ((2*N + 1) * nobj, sizeof(double) );
      for ( ix = 2; ix <= 2 * N; ix++ )
        for ( i = 0; i < nobj; i++ )
          fct[i + ix * nobj] = 1.0 / pow( cm[i + nobj], 1 + 0.5*ix);
    }
    
    /* create result storage */
    PROTECT( moments[im] = allocVector(REALSXP, (N+1)*(N+1)*nobj) );
    nprotect++;

    PROTECT( dims[im] = allocVector(INTSXP, 3) );
    nprotect++;
    INTEGER( dims[im] )[0] = N+1;
    INTEGER( dims[im] )[1] = N+1;
    INTEGER( dims[im] )[2] = nobj;
    SET_DIM( moments[im], dims[im] );

    /* set dim names if not rotation invariant, then different return */
    if ( alg < 3 ) {
      PROTECT( namesx[im] = allocVector(STRSXP, N+1) );
      nprotect++;
      PROTECT( namesy[im] = allocVector(STRSXP, N+1) );
      nprotect++;
      for ( int i = 0; i <= N; i++ ) {
        label[1] = (char)(i+48); /* 48 should 0 */
        label[0] = 'x';
        SET_STRING_ELT( namesx[im], i, mkChar(label) ); 
        label[0] = 'y';
        SET_STRING_ELT( namesy[im], i, mkChar(label) ); 
      }
      PROTECT( dimnames[im] = allocVector(VECSXP, 3) );
      nprotect++;
      SET_VECTOR_ELT( dimnames[im], 0, namesx[im] );
      SET_VECTOR_ELT( dimnames[im], 1, namesy[im] );
      setAttrib( moments[im], R_DimNamesSymbol, dimnames[im] );
    }
    
    /* reset result */
    m = REAL( moments[im] );
    for ( i = 0; i < (N+1)*(N+1)*nobj; i++ ) m[i] = 0.0;

    /* calculation of central moments */
    for ( x = 0; x < nx; x++ )
      for ( y = 0; y < ny; y++ ) {
        i = (int) data[ x + y * nx ];
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
      
    if ( alg < 3 ) continue;
    
    /* rotation invariant moments, alg 3 means scale inv are done already: alg > 1 */
    PROTECT( rmoments[im] = allocVector(REALSXP, 7 * nobj) );
    nprotect++;
    PROTECT( rdims[im] = allocVector(INTSXP, 2) );
    nprotect++;
    INTEGER( rdims[im] )[0] = nobj;
    INTEGER( rdims[im] )[1] = 7;
    SET_DIM( rmoments[im], rdims[im] );

    rm = REAL( rmoments[im] );

    /* set dim colnames, all these vars unset and unused if !(alg < 3) */
    PROTECT( namesx[im] = allocVector(STRSXP, 7) );
    nprotect++;
    label[0] = 'I';
    for ( int i = 0; i < 7; i++ ) {
      label[1] = (char)(i+49); /* 49 should 1 */
      SET_STRING_ELT( namesx[im], i, mkChar(label) ); 
    }
    PROTECT( dimnames[im] = allocVector(VECSXP, 2) );
    nprotect++;
    SET_VECTOR_ELT( dimnames[im], 1, namesx[im] );
    setAttrib( rmoments[im], R_DimNamesSymbol, dimnames[im] );
    
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
  }
  
  if ( im > 1 ) {
    PROTECT( res = allocVector(VECSXP, nz) );
    nprotect++;
    if ( alg > 2 ) { /* rotation invariant moments */
      for ( im = 0; im < nz; im++ )
        SET_VECTOR_ELT(res, im, rmoments[im] );
    }
    else {
      for ( im = 0; im < nz; im++ )
        SET_VECTOR_ELT(res, im, moments[im] );
    }
  }
  else {
    if ( alg > 2 )
      res = rmoments[0];
    else
      res = moments[0];
  }
    
  UNPROTECT( nprotect );
  return res;
}

