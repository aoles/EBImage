#include "features_zernike.h"

/* -------------------------------------------------------------------------
Calculating image moments from images and indexed images
Copyright (c) 2007 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tools.h"
#include "Rmath.h"
#include <R_ext/Error.h>

int nl_index(int n, int l) {
  int i, sum = 0;
  if ( n < 2 ) return n;
  for ( i = 1; i <= n / 2; i++ ) sum += i;
  if ( n % 2 == 0 ) 
    return 2 * sum + l / 2 + 0;
  return 2 * sum + l / 2 + n / 2 + 1;
}

double factorial(int x) {
  return floor(gammafn( (double)(x + 1) ));
}

/* calculates zernike features --------------------------------------- */

void Vnl(double x, double y, int n, int l, double vnl[2]) {
  int m;
  double denom, theta, value, m1; 
  value = 0.0;
  for (m = 0; m <= 0.5 * (n - l); m++ ) {
    /* denominator */
    denom = factorial(m) * factorial(0.5 * (n - 2*m + l)) * 
            factorial(0.5 * (n - 2*m - l));
    if ( denom == 0.0 ) continue;
    /* numerator */
    m1 = ( m % 2 == 0) ? 1.0 : (-1.0);
    value += m1 * factorial(n - m) * pow(x * x + y * y, 0.5 * n - m) / denom;
  }
  theta = atan2(y, x);
  vnl[0] = value * cos(l * theta);
  vnl[1] = value * sin(l * theta);
}

SEXP
lib_zernike ( SEXP obj, SEXP ref, SEXP xy_list, SEXP R, SEXP N, SEXP applyGaussian) {
  SEXP res, dup, xys, mRe, mIm, dm, dmnm, nm;
  int nx, ny, nz, nprotect, im, index, x, y, nobj, l, m, n, i, nmax, msize, doGauss;
  double * data, * xy, * ddata, * dmRe, * dmIm;
  double cy, cx, oneTo2sigma2, d2, r, newx, newy, vnl, theta, denom, m1;
  char label[7] = "z.0000";
  
  if ( !isImage(obj) ) return R_NilValue;
  nx = INTEGER ( GET_DIM(obj) )[0];
  ny = INTEGER ( GET_DIM(obj) )[1];
  nz = INTEGER ( GET_DIM(obj) )[2];
  nprotect = 0;

  doGauss = INTEGER(applyGaussian)[0];
  r = REAL(R)[0];
  oneTo2sigma2 = 0.5 / (r * r * 0.64);
  nmax = INTEGER(N)[0];
  msize = nl_index(nmax, nmax) + 1; // inlcude n = 0
  
  PROTECT ( res = allocVector(VECSXP, nz) );
  nprotect++;

  if ( doGauss ) {
    PROTECT( dup = Rf_duplicate(ref) );
    nprotect++;
  }
  else dup = ref;

  /* precreate dimnames to duplicate onto each matrix later */
  PROTECT( dmnm = allocVector(VECSXP, 2) );
  nprotect++;
  PROTECT( nm = allocVector(STRSXP, msize) );
  nprotect++;
  for ( n = 0; n <= nmax; n++ )
    for ( l = 0; l <= n; l++ ) {
      if ( (n - l) % 2 != 0 ) continue;
      i = nl_index(n, l);
      label[2] = (char)(n / 10 + 48);
      label[3] = (char)(n - (n / 10) * 10 + 48);
      label[4] = (char)(l / 10 + 48);
      label[5] = (char)(l - (l / 10) * 10 + 48);
      SET_STRING_ELT( nm, i, Rf_duplicate(mkChar(label)) );
    }
  SET_VECTOR_ELT( dmnm, 1, nm );
  UNPROTECT( 1 ); nprotect--; // nm

  for ( im = 0; im < nz; im++ ) {
    /* get image data */
    data  = &( REAL(obj)[ im * nx * ny ] );
    ddata = &( REAL(dup)[ im * nx * ny] );
    /* get number of objects -- max index */
    nobj = 0;
    for ( index = 0; index < nx * ny; index++ )
      if ( data[index] > nobj ) nobj = floor( data[index] );
    if ( nobj < 1 ) {
      SET_VECTOR_ELT( res, im, R_NilValue );
      continue;
    }
    /* get xy values for this frame */
    if ( nz == 1 ) xys = xy_list;
    else xys = VECTOR_ELT(xy_list, im);
    if ( xys == R_NilValue || INTEGER(GET_DIM(xys))[0] != nobj || INTEGER(GET_DIM(xys))[1] < 2 ) continue;
    xy = REAL(xys);

    SET_VECTOR_ELT( res, im, (mRe = allocVector(REALSXP, nobj * msize)) );
    dmRe = REAL( mRe );
    PROTECT( mIm = allocVector(REALSXP, nobj * msize) );
    nprotect++;
    dmIm = REAL( mIm );
    for ( i = 0; i < nobj * msize; i++ ) {
      dmRe[i] = 0.0;
      dmIm[i] = 0.0;
    }

    /* dimension of results matrix */
    PROTECT( dm = allocVector(INTSXP, 2) );
    nprotect++;
    INTEGER(dm)[0] = nobj; 
    INTEGER(dm)[1] = msize;
    SET_DIM( mRe, dm );
    SET_DIM( mIm, Rf_duplicate(dm) );
    UNPROTECT( 1 ); nprotect--; // dm
    setAttrib( mRe, R_DimNamesSymbol, Rf_duplicate(dmnm) );

    /* loop through x/y, calculate features*/
    for ( x = 0; x < nx; x++ )
      for ( y = 0; y < ny; y++ ) {
        index = floor( data[x + y * nx] ); /* index of the object, R-style = 1-based */
        if ( index < 1 ) continue;
        /* all indexes were 1, 2, 3, but C has 0-based indexes!!! */
        index--;
        /* get object's centre */
        cx = xy[index];
        cy = xy[index + nobj];
    		/* normalise co-ordinates */
		    newx = (x - cx) / r;
    		newy = (y - cy) / r;
    	  d2 = newx * newx + newy * newy;
    		/* only use pixels within normalisd unit circle */
    		if ( d2 > 1.0 ) continue;
        /* apply Gaussian transform */
    	  if ( doGauss ) ddata[x + y * nx] *= exp( -d2 * r*r * oneTo2sigma2 );
        for ( n = 0; n <= nmax; n++ )
        	for ( l = 0; l <= n; l++ ) {
	          /* only want values when (n - l) is even */
        	  if ( (n - l) % 2 != 0 ) continue;
            vnl = 0.0;
            for (m = 0; m <= (n - l) / 2; m++ ) {
              /* denominator */
              denom = factorial(m) * factorial(0.5 * (n - 2*m + l)) * 
                      factorial(0.5 * (n - 2*m - l));
              if ( denom == 0.0 ) continue;
              /* numerator */
              m1 = ( m % 2 == 0) ? 1.0 : (-1.0);
              vnl += m1 * factorial(n - m) * pow(d2, 0.5 * n - m) / denom;
            }
            theta = atan2(newy, newx);
            vnl *= ddata[x + y * nx] * (n + 1) / M_PI;
            i = index + nl_index(n,l) * nobj;
            dmRe[i] = vnl * cos(l * theta);
            dmIm[i] = vnl * sin(l * theta);
          }
      }
    /* loop through mRe, mIm, get abs and mult by (n+1)/pi, save in mRe, which is returned! */ 
    for ( i = 0; i < nobj * msize; i++ )
      dmRe[i] = sqrt(dmRe[i]*dmRe[i] + dmIm[i]*dmIm[i]);
      
    UNPROTECT( 1 ); nprotect--; // mIm
  }

  UNPROTECT( nprotect );
  if ( nz == 1 ) return VECTOR_ELT( res, 0 );
  return res;   
}
