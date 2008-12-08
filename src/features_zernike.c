#include "features_zernike.h"

/* -------------------------------------------------------------------------
Calculating image moments from images and indexed images
Copyright (c) 2007 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tools.h"
#include "Rmath.h"
#include <R_ext/Error.h>

/* ---------------------------------------------------------------------- */
/* generates Z index from n, l */
int nl_index(int n, int l) {
  int i, sum = 0;
  if ( n < 2 ) return n;
  for ( i = 1; i <= n / 2; i++ ) sum += i;
  if ( n % 2 == 0 ) 
    return 2 * sum + l / 2 + 0;
  return 2 * sum + l / 2 + n / 2 + 1;
}

int p_nl_index(int n, int l) {
  return ((n * (n + 1)) / 2 + l);
}

/* ---------------------------------------------------------------------- */
double factorial(int x) {
  return floor( gammafn( (double)(x + 1) ) );
}

/* ---------------------------------------------------------------------- */
/* run-through index of features, from n, l */
#define ZIND(n,l)   ((l) + (n)*(nmax + 1))
/* run-through index of V-factors, from n, l, m */
#define VIND(n,l,m) ((l) + (n)*(nmax + 1) + (m)*(nmax + 1) * (nmax + 1) )

/* ---------------------------------------------------------------------- */
SEXP
lib_zernike ( SEXP obj, SEXP ref, SEXP xy_list, SEXP R, SEXP N, SEXP applyGaussian) {
  SEXP res, dup, xys, mRe, mIm, dm, dmnm, nm;
  int nx, ny, nz, nprotect, im, index, x, y, nobj, l, m, n, i, nmax, msize, doGauss, no_objects;
  double * data, * xy, * ddata, * dmRe, * dmIm;
  double r2To2sigma2, d2, r, newx, newy, vnl, theta;
  char label[7] = "z.0000";

  double * v; /* array of Vnl factors for n, l, m (everything but pow(d,...) and e^ilT) */
  int    * z; /* array of Z feature indexes from n, l */

  validImage(obj,0);

  nx = INTEGER ( GET_DIM(obj) )[0];
  ny = INTEGER ( GET_DIM(obj) )[1];
  nz = getNumberOfFrames(obj,0);
  nprotect = 0;

  doGauss = INTEGER(applyGaussian)[0];
  r = REAL(R)[0];
  r2To2sigma2 = 0.5 / 0.64; // s = 0.8 * r => r^2 / ( 2 *(0.8 *r)^2 ) = 1/(2*0.64)
  nmax = INTEGER(N)[0];
  msize = nl_index(nmax, nmax) + 1; // inlcude n = 0
  
  PROTECT ( res = allocVector(VECSXP, nz) );
  nprotect++;

  /* if Gaussian is applied, the image is modified, thus we need a duplicate */
  if ( doGauss ) {
    PROTECT( dup = Rf_duplicate(ref) );
    nprotect++;
  }
  else dup = ref;

  /* precalculates v's and z's */
  v = (double *) R_alloc((nmax + 1) * (nmax + 1) * (nmax/2 + 1), sizeof(double) );
  z = (int *)    R_alloc((nmax + 1) * (nmax + 1), sizeof(int));
  /* precreate dimnames to duplicate onto each matrix later */
  PROTECT( dmnm = allocVector(VECSXP, 2) );
  nprotect++;
  PROTECT( nm = allocVector(STRSXP, msize) );
  nprotect++;
  for ( n = 0; n <= nmax; n++ )
    for ( l = 0; l <= n; l++ ) {
      if ( (n - l) % 2 != 0 ) continue;
      i = nl_index(n, l);
      /* z(n,l) cached */
      z[ ZIND(n,l) ] = i;
      label[2] = (char)(n / 10 + 48);
      label[3] = (char)(n - (n / 10) * 10 + 48);
      label[4] = (char)(l / 10 + 48);
      label[5] = (char)(l - (l / 10) * 10 + 48);
      SET_STRING_ELT( nm, i, Rf_duplicate(mkChar(label)) );
      for (m = 0; m <= (n - l) / 2; m++ )
        /* v(n,l,m) cached */
        if (n == l) 
          v[ VIND(n,l,m) ] = (n + 1) / M_PI;
        else
          v[ VIND(n,l,m) ] = (n + 1) * ((m%2==0)?(1.0):(-1.0)) * factorial(n - m) / 
                           ( M_PI * factorial(m) * factorial(0.5*(n - 2*m + l)) 
                             * factorial(0.5*(n - 2*m - l)) );
    }
  SET_VECTOR_ELT( dmnm, 1, nm );
  UNPROTECT( 1 ); nprotect--; // nm

  for ( im = 0; im < nz; im++ ) {
    /* get image data */
    data  = &( REAL(obj)[ im * nx * ny ] );
    ddata = &( REAL(dup)[ im * nx * ny ] );
    /* get number of objects -- max index */
    nobj = 0;
    for ( index = 0; index < nx * ny; index++ )
      if ( data[index] > nobj ) nobj = floor( data[index] );
    if ( nobj < 1 ) {
      no_objects = 1;
      nobj = 1; /* if no objects, create a matrix for 1 and fill all 0 */
      warning("Image contains no objects");
    }
    else no_objects = 0;
    
    SET_VECTOR_ELT( res, im, (mRe = allocVector(REALSXP, nobj * msize)) );
    dmRe = REAL( mRe );
    for ( i = 0; i < nobj * msize; i++ ) dmRe[i] = 0.0;
    if ( !no_objects ) {
      PROTECT( mIm = allocVector(REALSXP, nobj * msize) );
      nprotect++;
      dmIm = REAL( mIm );
      for ( i = 0; i < nobj * msize; i++ ) dmIm[i] = 0.0;
    }
    else {
      mIm = R_NilValue;
      dmIm = NULL;
    }
    /* dimension of results matrix */
    PROTECT( dm = allocVector(INTSXP, 2) );
    nprotect++;
    INTEGER(dm)[0] = nobj; 
    INTEGER(dm)[1] = msize;
    SET_DIM( mRe, dm );
    if ( !no_objects ) SET_DIM( mIm, Rf_duplicate(dm) );
    UNPROTECT( 1 ); nprotect--; // dm
    setAttrib( mRe, R_DimNamesSymbol, Rf_duplicate(dmnm) );

    /* return empty matrix (go to next image) with 1 line if error (no objects) */
    if ( no_objects ) continue;
    /* get xy values for this frame */
    if ( nz == 1 ) xys = xy_list;
    else xys = VECTOR_ELT(xy_list, im);
    if ( xys == R_NilValue || INTEGER(GET_DIM(xys))[0] != nobj || INTEGER(GET_DIM(xys))[1] < 2 ) continue;
    xy = REAL(xys);

    /* loop through x/y, calculate features*/
    for ( x = 0; x < nx; x++ )
      for ( y = 0; y < ny; y++ ) {
        index = floor( data[x + y * nx] ); /* index of the object, R-style = 1-based */
        if ( index < 1 ) continue;
        /* all indexes were 1, 2, 3, but C has 0-based indexes!!! */
        index--;
        /* get object's centre */
    		/* normalise co-ordinates */
		    newx = (x - xy[index]       ) / r;
    		newy = (y - xy[index + nobj]) / r;
    	  d2 = newx * newx + newy * newy;
    		/* only use pixels within normalisd unit circle */
    		if ( d2 > 1.0 ) continue;
        /* apply Gaussian transform */
    	  if ( doGauss ) ddata[x + y * nx] *= exp( -d2 * r2To2sigma2 );
        theta = atan2(newy, newx); 
        for ( n = 0; n <= nmax; n++ )
        	for ( l = 0; l <= n; l++ ) {
	          /* only want values when (n - l) is even */
        	  if ( (n - l) % 2 != 0 ) continue;
            vnl = 0.0;
            for (m = 0; m  <= (n - l) / 2; m++ )
              vnl += v[ VIND(n,l,m) ] * pow(d2, 0.5 * n - m);
            vnl *= ddata[x + y * nx];
            i = index + z[ZIND(n,l)] * nobj;
            /* no need to do e^i*l*theta if l=0 */
            dmRe[i] += vnl * cos(l * theta);
            dmIm[i] += vnl * sin(l * theta);
          }
      }
    /* loop through mRe, mIm, get abs and mult by (n+1)/pi, save in mRe, which is returned! */ 
    for ( i = 0; i < nobj * msize; i++ )
      dmRe[i] = sqrt(dmRe[i]*dmRe[i] + dmIm[i]*dmIm[i]);
      
    if ( mIm != R_NilValue ) { /* i.e. if protected */
      UNPROTECT( 1 ); nprotect--; // mIm
    }
  }

  UNPROTECT( nprotect );
  if ( nz == 1 ) return VECTOR_ELT( res, 0 );
  return res;   
}

SEXP
lib_pseudo_zernike ( SEXP obj, SEXP ref, SEXP xy_list, SEXP R, SEXP N, SEXP applyGaussian) {
  SEXP res, dup, xys, mRe, mIm, dm, dmnm, nm;
  int nx, ny, nz, nprotect, im, index, x, y, nobj, l, m, n, i, nmax, msize, doGauss, no_objects;
  double * data, * xy, * ddata, * dmRe, * dmIm;
  double r2To2sigma2, d2, r, newx, newy, vnl, theta;
  char label[8] = "pz.0000";

  double * v; /* array of Vnl factors for n, l, m (everything but pow(d,...) and e^ilT) */
  int    * z; /* array of Z feature indexes from n, l */

  validImage(obj,0);

  nx = INTEGER ( GET_DIM(obj) )[0];
  ny = INTEGER ( GET_DIM(obj) )[1];
  nz = getNumberOfFrames(obj,0);
  nprotect = 0;

  doGauss = INTEGER(applyGaussian)[0];
  r = REAL(R)[0];
  r2To2sigma2 = 0.5 / 0.64; // s = 0.8 * r => r^2 / ( 2 *(0.8 *r)^2 ) = 1/(2*0.64)
  nmax = INTEGER(N)[0];
  msize = p_nl_index(nmax, nmax) + 1; // inlcude n = 0
  
  PROTECT ( res = allocVector(VECSXP, nz) );
  nprotect++;

  /* if Gaussian is applied, the image is modified, thus we need a duplicate */
  if ( doGauss ) {
    PROTECT( dup = Rf_duplicate(ref) );
    nprotect++;
  }
  else dup = ref;

  /* precalculates v's and z's */
  v = (double *) R_alloc(((nmax + 1) + nmax * (nmax + 1) + nmax * (nmax + 1) * (nmax + 1)), sizeof(double) );
  z = (int *)    R_alloc((nmax + 1) * (nmax + 2) + (nmax + 1), sizeof(int));
  /* precreate dimnames to duplicate onto each matrix later */
  PROTECT( dmnm = allocVector(VECSXP, 2) );
  nprotect++;
  PROTECT( nm = allocVector(STRSXP, msize) );
  nprotect++;
  for ( n = 0; n <= nmax; n++ )
    for ( l = 0; l <= n; l++ ) {
      i = p_nl_index(n, l);
      /* z(n,l) cached */
      z[ ZIND(n,l) ] = i;
      label[3] = (char)(n / 10 + 48);
      label[4] = (char)(n - (n / 10) * 10 + 48);
      label[5] = (char)(l / 10 + 48);
      label[6] = (char)(l - (l / 10) * 10 + 48);
      SET_STRING_ELT( nm, i, Rf_duplicate(mkChar(label)) );
      for (m = 0; m <= (n - l); m++ )
        /* v(n,l,m) cached */
        if (n == l)
          v[ VIND(n,l,m) ] = (n + 1) / M_PI;
        else
          v[ VIND(n,l,m) ] = (n + 1) * ((m%2==0)?(1.0):(-1.0)) * factorial(2*n + 1 - m) / 
                           ( M_PI * factorial(m) * factorial(n + l + 1 - m) 
                             * factorial(n - l - m) );
    }
  SET_VECTOR_ELT( dmnm, 1, nm );
  UNPROTECT( 1 ); nprotect--; // nm

  for ( im = 0; im < nz; im++ ) {
    /* get image data */
    data  = &( REAL(obj)[ im * nx * ny ] );
    ddata = &( REAL(dup)[ im * nx * ny ] );
    /* get number of objects -- max index */
    nobj = 0;
    for ( index = 0; index < nx * ny; index++ )
      if ( data[index] > nobj ) nobj = floor( data[index] );
    if ( nobj < 1 ) {
      no_objects = 1;
      nobj = 1; /* if no objects, create a matrix for 1 and fill all 0 */
      warning("Image contains no objects");
    }
    else no_objects = 0;
    
    SET_VECTOR_ELT( res, im, (mRe = allocVector(REALSXP, nobj * msize)) );
    dmRe = REAL( mRe );
    for ( i = 0; i < nobj * msize; i++ ) dmRe[i] = 0.0;
    if ( !no_objects ) {
      PROTECT( mIm = allocVector(REALSXP, nobj * msize) );
      nprotect++;
      dmIm = REAL( mIm );
      for ( i = 0; i < nobj * msize; i++ ) dmIm[i] = 0.0;
    }
    else {
      mIm = R_NilValue;
      dmIm = NULL;
    }
    /* dimension of results matrix */
    PROTECT( dm = allocVector(INTSXP, 2) );
    nprotect++;
    INTEGER(dm)[0] = nobj; 
    INTEGER(dm)[1] = msize;
    SET_DIM( mRe, dm );
    if ( !no_objects ) SET_DIM( mIm, Rf_duplicate(dm) );
    UNPROTECT( 1 ); nprotect--; // dm
    setAttrib( mRe, R_DimNamesSymbol, Rf_duplicate(dmnm) );

    /* return empty matrix (go to next image) with 1 line if error (no objects) */
    if ( no_objects ) continue;
    /* get xy values for this frame */
    if ( nz == 1 ) xys = xy_list;
    else xys = VECTOR_ELT(xy_list, im);
    if ( xys == R_NilValue || INTEGER(GET_DIM(xys))[0] != nobj || INTEGER(GET_DIM(xys))[1] < 2 ) continue;
    xy = REAL(xys);

    /* loop through x/y, calculate features*/
    for ( x = 0; x < nx; x++ )
      for ( y = 0; y < ny; y++ ) {
        index = floor( data[x + y * nx] ); /* index of the object, R-style = 1-based */
        if ( index < 1 ) continue;
        /* all indexes were 1, 2, 3, but C has 0-based indexes!!! */
        index--;
        /* get object's centre */
    		/* normalise co-ordinates */
		    newx = (x - xy[index]       ) / r;
    		newy = (y - xy[index + nobj]) / r;
    	  d2 = newx * newx + newy * newy;
    		/* only use pixels within normalisd unit circle */
    		if ( d2 > 1.0 ) continue;
        /* apply Gaussian transform */
    	  if ( doGauss ) ddata[x + y * nx] *= exp( -d2 * r2To2sigma2 );
        theta = atan2(newy, newx);
        for ( n = 0; n <= nmax; n++ )
        	for ( l = 0; l <= n; l++ ) {
            vnl = 0.0;
            for (m = 0; m <= (n - l); m++ )
              vnl += v[ VIND(n,l,m) ] * pow(d2, n - m);
            vnl *= ddata[x + y * nx];
            i = index + z[ZIND(n,l)] * nobj;
            /* no need to do e^i*l*theta if l=0 */
            dmRe[i] += vnl * cos(l * theta);
            dmIm[i] += vnl * sin(l * theta);
          }
      }
    /* loop through mRe, mIm, get abs, save in mRe, which is returned! */ 
    for ( i = 0; i < nobj * msize; i++ )
      dmRe[i] = sqrt(dmRe[i]*dmRe[i] + dmIm[i]*dmIm[i]);
      
    if ( mIm != R_NilValue ) { /* i.e. if protected */
      UNPROTECT( 1 ); nprotect--; // mIm
    }
  }

  UNPROTECT( nprotect );
  if ( nz == 1 ) return VECTOR_ELT( res, 0 );
  return res;   
}

