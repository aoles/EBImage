#include "objects.h"

/* -------------------------------------------------------------------------
Counting objects determined in segmentations like watershed
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tools.h"
#include <R_ext/Error.h>
#include <stdio.h>

/*----------------------------------------------------------------------- */
/* paints features on the target image with given colors and opacs    */
SEXP
paintObjects (SEXP x, SEXP tgt, SEXP _opac, SEXP _col, SEXP _thick) {
    SEXP res;
    int nprotect, nx, ny, nz, im, index, thick;
    int i, j;
    double *opac, *col;
    double *dx, *dres, dp;
    int redstride, greenstride, bluestride;
    int xcolormode;

    validImage(x,0);
    validImage(tgt,0);

    nx = INTEGER(GET_DIM(x))[0];
    ny = INTEGER(GET_DIM(x))[1];
    nz = getNumberOfFrames(x, 0);
    nprotect = 0;
    xcolormode = getColorMode(x);
    if (xcolormode != MODE_GRAYSCALE) error("'x' must be in 'Grayscale' color mode");

    PROTECT ( res = Rf_duplicate(tgt) );
    nprotect++;

    opac = REAL (_opac);
    col = REAL(_col);
    thick = INTEGER(_thick)[0];
    

    for (im = 0; im < nz; im++) {
      dx = &( REAL(x)[ im * nx * ny ] );
      dres = REAL(res);
      getColorStrides(tgt, im, &redstride, &greenstride, &bluestride);
      
      for ( j = 0; j < ny; j++ ) {
        for ( i = 0; i < nx; i++ ) {	    
                    
          if(thick) {
            if (  ( i > 0       && dx[j*nx + i-1] != dx[j*nx + i] ) ||
                  ( i < nx - 1  && dx[j*nx + i+1] != dx[j*nx + i] ) ||
                  ( j > 0       && dx[(j-1)*nx + i] != dx[j*nx + i] ) ||
                  ( j < ny - 1  && dx[(j+1)*nx + i] != dx[j*nx + i] ) ) 
              index = 0;
            else {
              /* if it is on the border */
              if ( i < 1 || i > nx - 2 || j < 1 || j > ny - 2 ) {
                if ( dx[j*nx + i] > 0 ) index = 2;
                else continue;
              }
              else {
                if ( dx[j*nx + i] > 1 ) index = 1;
                else continue;
              }
            }
           
          }
          
          else {
        	  /* pixel is contact */
        	  index = 1;
        	  if ( dx[j*nx + i]<=0 ) continue;
        	  if ( dx[j*nx + i] < 1.0 || i < 1 || i > nx - 2 || j < 1 || j > ny - 2 ) index = 2;
        	  else {
        	    /* check if pixel is border, edge is same as contact */
        	    if ( dx[j*nx + i-1] != dx[j*nx + i] ||  dx[j*nx + i+1] != dx[j*nx + i] ||
        		 dx[(j-1)*nx + i] != dx[j*nx + i] || dx[(j+1)*nx + i] != dx[j*nx + i]) index = 0;
        	  }	  
          }
          
          if (redstride!=-1) {
            dp=dres[redstride+j*nx + i]*(1-opac[index]) + col[index]*opac[index];		
        	  dres[redstride+j*nx + i]=dp;
        	}
        	if (greenstride!=-1) {
        	  dp=dres[greenstride+j*nx + i]*(1-opac[index]) + col[index+3]*opac[index];
        	  dres[greenstride+j*nx + i]=dp;
        	}
        	if (bluestride!=-1) {
        	  dp=dres[bluestride+j*nx + i]*(1-opac[index]) + col[index+3*2]*opac[index];	
        	  dres[bluestride+j*nx + i]=dp;
        	}
        }
      }
    }

    UNPROTECT (nprotect);
    return res;
}


/*----------------------------------------------------------------------- */
SEXP
rmObjects (SEXP x, SEXP _index, SEXP _reenum) {
    SEXP res, index;
    int nprotect, nx, ny, nz, i, j, im, nobj, * indexes, found;
    double * data;

    validImage(x,0);

    nx = INTEGER ( GET_DIM(x) )[0];
    ny = INTEGER ( GET_DIM(x) )[1];
    nz = getNumberOfFrames(x,0);
    nprotect = 0;


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
        /* reenumerate object indices */
        if ( INTEGER(_reenum)[0] ) {
          j = 1;
          for ( i = 0; i < nobj; i++ ) {
              if ( indexes[i] > 0 ) {
                  indexes[i] = j;
                  j++;
              }
          }
        }
        /* reset image */
        for ( i = 0; i < nx * ny; i++ ) {
            if ( data[i] < 0.9 ) continue;
            data [i] = indexes[ (int)data[i] - 1 ];
        }
        Free (indexes);

    }

    UNPROTECT (nprotect);
    return res;
}


/*----------------------------------------------------------------------- */
SEXP
stackObjects (SEXP obj, SEXP ref, SEXP _bgcol, SEXP xy_list, SEXP extension) {
  SEXP res, st=NULL, dim, xys;
  int nx, ny, nz, nprotect, im, x, y, i, j, pxi, nobj, index;
  double *dobj, *dref, *xy, xx, yy,  *bgcol;
  double * dst;
  int ext = floor(REAL(extension)[0]);
  int snx = 2 * ext + 1;
  int sny = 2 * ext + 1;
  int mode = getColorMode(ref);
  int redstride, greenstride, bluestride;
  int redstridet, greenstridet, bluestridet;
 
  nx = INTEGER ( GET_DIM(obj) )[0];
  ny = INTEGER ( GET_DIM(obj) )[1];
  nz = getNumberOfFrames(obj,0);
  bgcol = REAL(_bgcol);
  nprotect = 0;

  if (nz == 1) {
    PROTECT(res = Rf_duplicate(_bgcol));
    nprotect++;
  }
  else {
    PROTECT(res = allocVector(VECSXP, nz));
    nprotect++;
    for (im = 0; im < nz; im++) SET_VECTOR_ELT(res, im, Rf_duplicate(_bgcol) );
  }
 
  for (im = 0; im < nz; im++) {
    // set dobj, dref and strides
    dobj = &(REAL(obj)[im * nx * ny]);
    dref = REAL(ref);
    getColorStrides(ref, im, &redstridet, &greenstridet, &bluestridet);

    // get number of objects = max index
    nobj = 0;
    for (i = 0; i < nx * ny; i++) if (dobj[i] > nobj) nobj = dobj[i];

    if (nobj>0) {
      // create stack
      if (mode==MODE_GRAYSCALE) {
	PROTECT(st = allocVector(REALSXP, nobj * snx * sny));
	nprotect++;
	dst = REAL(st);

	// bg color initialization
	for (i = 0; i < nobj * snx * sny; i++) dst[i] = bgcol[0];
      
	// set dims
	PROTECT (dim = allocVector( INTSXP, 3 ));
	nprotect++;
	INTEGER (dim)[0] = snx;
	INTEGER (dim)[1] = sny;
	INTEGER (dim)[2] = nobj;
	SET_DIM (st, dim);
	UNPROTECT(1); nprotect--; // dim
      }
      else if (mode==MODE_COLOR)  {
	// create stack
	PROTECT(st = allocVector(REALSXP, nobj * snx * sny * 3));
	nprotect++;
	dst = REAL( st );

	// bg color initialization
	for (j=0; j<nobj; j++) {
	  redstride = j*3*snx*sny + 0*snx*sny;
	  greenstride = j*3*snx*sny + 1*snx*sny;
	  bluestride = j*3*snx*sny + 2*snx*sny;
	  for (i=0; i<snx*sny; i++) {
	    dst[i+redstride] = bgcol[0];
	    dst[i+greenstride] = bgcol[1];
	    dst[i+bluestride] = bgcol[2];
	  }
	}
      
	// set dims
	PROTECT (dim = allocVector( INTSXP, 4));
	nprotect++;
	INTEGER (dim)[0] = snx;
	INTEGER (dim)[1] = sny;
	INTEGER (dim)[2] = 3;
	INTEGER (dim)[3] = nobj;
	SET_DIM (st, dim);
	UNPROTECT(1); nprotect--; // dim
      }

      // set slot
      if (nz == 1 ) res = SET_SLOT(res, install(".Data"), st);
      else SET_VECTOR_ELT(res, im, SET_SLOT(VECTOR_ELT(res, im), install(".Data"), st) );
      UNPROTECT( 1 ); nprotect--; // st

      // get xy
      if (nz == 1) xys = xy_list;
      else xys = VECTOR_ELT(xy_list, im);
      if (xys == R_NilValue || INTEGER(GET_DIM(xys))[0] != nobj || INTEGER(GET_DIM(xys))[1] < 2) continue;
      xy = REAL(xys);

      if (nz == 1) dst = REAL(res);
      else dst = REAL(VECTOR_ELT(res, im));
      
      // copy ref
      for (x = 0; x < nx; x++) {
	for (y = 0; y < ny; y++) {
	  index = dobj[x + y * nx] - 1; // background index 0 is not kept
	  if (index < 0) continue;
	 
	  // target frame x, y coordinates
	  xx = x - floor(xy[index]) + ext + 1 ;
	  yy = y - floor(xy[index + nobj]) + ext + 1;
	  
	  if ( xx < 0 || xx >= snx || yy < 0 || yy >= sny ) continue;
	  else {
	    pxi = xx + yy * snx;
	    if (mode==MODE_GRAYSCALE) dst[ pxi + index * sny * snx ] = dref[x + y * nx + im*nx*ny];
	    else { 
	      redstride = index*3*snx*sny + 0*snx*sny;
	      greenstride = index*3*snx*sny + 1*snx*sny;
	      bluestride = index*3*snx*sny + 2*snx*sny;
	      dst[pxi + redstride] = dref[x + y * nx + redstridet];
	      dst[pxi + greenstride] = dref[x + y * nx + greenstridet];
	      dst[pxi + bluestride] = dref[x + y * nx + bluestridet];
	    }	  
	  }
	}
      }
    } // nobj>0
    else {
      // set slot 
      if (nz == 1 ) res = SET_SLOT(res, install(".Data"), R_NilValue);
      else SET_VECTOR_ELT(res, im, SET_SLOT(VECTOR_ELT(res, im), install(".Data"), R_NilValue));
    } 
  } // im

  UNPROTECT( nprotect );
  return res;
}
