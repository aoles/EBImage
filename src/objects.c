#include "objects.h"

/* -------------------------------------------------------------------------
Counting objects determined in segmentations like watershed
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "EBImage.h"
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
    double *dx, *dres, dp, val;
    int redstride, greenstride, bluestride;
    int xcolormode;

    validImage(x,0);
    validImage(tgt,0);

    nx = INTEGER(GET_DIM(x))[0];
    ny = INTEGER(GET_DIM(x))[1];
    nz = getNumberOfFrames(x, 0);
    nprotect = 0;
    xcolormode = COLOR_MODE(x);
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
          val = dx[j*nx + i];
          
          if (thick) {
            /* object border */
            if (  ( i > 0       && dx[j*nx + i-1] != val ) ||
                  ( i < nx - 1  && dx[j*nx + i+1] != val ) ||
                  ( j > 0       && dx[(j-1)*nx + i] != val ) ||
                  ( j < ny - 1  && dx[(j+1)*nx + i] != val ) ) 
              index = 0;
            else {
              /* background */
              if ( val <= 0 )
                continue;
              /* if image edge index=2, if object body index=1 */
              else
                index = (i==0 || i==nx-1 || j==0 || j==ny-1 || val < 1) ? 2 : 1;
            }
          }
          
          else {
            /* background */
            if ( val <= 0 )
              continue;
            else {
              /* object border */
              if (  ( i > 0       && dx[j*nx + i-1] != val ) ||
                    ( i < nx - 1  && dx[j*nx + i+1] != val ) ||
                    ( j > 0       && dx[(j-1)*nx + i] != val ) ||
                    ( j < ny - 1  && dx[(j+1)*nx + i] != val ) ) 
                index = 0;
              /* if image edge index=2, if object body index=1 */
              else
                index = (i==0 || i==nx-1 || j==0 || j==ny-1 || val < 1) ? 2 : 1;
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
    int nx, ny, nz, sizexy, i, j, im, nobj, * indexes, idx, val, reenum;
    int * src, * tgt;

    validImage(x,0);

    nx = INTEGER ( GET_DIM(x) )[0];
    ny = INTEGER ( GET_DIM(x) )[1];
    nz = getNumberOfFrames(x,0);
    
    reenum = LOGICAL(_reenum)[0];
    
    PROTECT( res = allocVector(INTSXP, XLENGTH(x)) );
    DUPLICATE_ATTRIB(res, x);
  
    sizexy = nx * ny;
  
    for ( im = 0; im < nz; im++ ) {
        /* get image data */
        src = &( INTEGER(x)[ im * sizexy ] );
        tgt = &( INTEGER(res)[ im * sizexy ] );
        
        /* get number of objects -- max index */
        nobj = 0;
        for ( i = 0; i < sizexy; i++ )
            if ( src[i] > nobj ) nobj = src[i]; // NA_integer_ is -2147483648
            
        indexes = (int *) calloc((nobj + 1), sizeof(int));
        
        /* reset indices of removed objects */
        if ( _index!=R_NilValue ) {
          index = VECTOR_ELT (_index, im);
          
          for ( i = 0; i <= nobj; i++ )
            indexes[i] = i;
          
          for ( i = 0; i < LENGTH(index); i++ ) {
            idx = INTEGER(index)[i];
            if (idx > 0 && idx <= nobj)
              indexes[idx] = 0;
          }
        }
        else {
          for ( i = 0; i < sizexy; i++ ) {
            val = src[i];
            if ( val > 0 )
              indexes[val] = val;
          }
        }
        
        /* reenumerate object indices */
        if ( reenum ) {
          j = 1;
          for ( i = 1; i <= nobj; i++ ) {
              if ( indexes[i] > 0 ) {
                  indexes[i] = j;
                  j++;
              }
          }
        }
        
        /* reset image */
        for ( i = 0; i < sizexy; i++ ) {
            val = src[i];
            tgt[i] = ( val > 0 ) ? indexes[val] : val; // support NA's
        }
        
        Free (indexes);
    }

    UNPROTECT (1);
    return res;
}


/*----------------------------------------------------------------------- */
SEXP
stackObjects (SEXP obj, SEXP ref, SEXP _bgcol, SEXP xy_list, SEXP extension) {
  SEXP res, st=NULL, dim, xys, img;
  int nx, ny, nz, nc, nprotect, im, x, y, i, j, pxi, nobj, index;
  double *dobj, *dref, *xy, xx, yy,  *bgcol;
  double * dst;
  int ext = floor(REAL(extension)[0]);
  int snx = 2 * ext + 1;
  int sny = 2 * ext + 1;
  int mode = COLOR_MODE(ref);
  int nbChannels = getNumberOfChannels(ref, mode);
  int stride, shift;
 
  nx = INTEGER ( GET_DIM(obj) )[0];
  ny = INTEGER ( GET_DIM(obj) )[1];
  nz = getNumberOfFrames(obj, 0);
  bgcol = REAL(_bgcol);
  nprotect = 0;

  // allow only up to 3 color channels
  if (nbChannels>3) nbChannels = 3;

  if (nz > 1) {
    PROTECT(res = allocVector(VECSXP, nz));
    nprotect++;
  }
  
  for (im = 0; im < nz; im++) {
    // set dobj, dref and strides
    dobj = &(REAL(obj)[im * nx * ny]);
    dref = REAL(ref);

    // get number of objects = max index
    nobj = 0;
    for (i = 0; i < nx * ny; i++) if (dobj[i] > nobj) nobj = dobj[i];

    if (nobj>0) {
      // create stack
      PROTECT(st = allocVector(REALSXP, nobj * snx * sny * nbChannels));
      nprotect++;
      dst = REAL(st);
      
      // bg color initialization
      for (j=0; j<nobj; j++) {
        for(nc=0; nc<nbChannels; nc++) {
          shift = j*nbChannels*snx*sny + nc*snx*sny;
          for (i=0; i<snx*sny; i++) dst[i+shift] = bgcol[nc];
        }
      }
      
      if (mode==MODE_GRAYSCALE) {
      	PROTECT (dim = allocVector( INTSXP, 3 ));
      	nprotect++;
      	INTEGER (dim)[0] = snx;
      	INTEGER (dim)[1] = sny;
      	INTEGER (dim)[2] = nobj;
      }
      else if (mode==MODE_COLOR) {
      	PROTECT (dim = allocVector( INTSXP, 4));
      	nprotect++;
      	INTEGER (dim)[0] = snx;
      	INTEGER (dim)[1] = sny;
      	INTEGER (dim)[2] = nbChannels;
      	INTEGER (dim)[3] = nobj;
      }
      SET_DIM (st, dim);
      
      PROTECT(img = Rf_duplicate(_bgcol));
      nprotect++;
      
      // set slot
      if (isImage(_bgcol)) {
        PROTECT(img = Rf_duplicate(_bgcol));
        nprotect++;
        img = SET_SLOT(img, Image_Data, st);
      }
      else {
        img = st;
      }
      
      // get xy
      if (nz == 1) xys = xy_list;
      else xys = VECTOR_ELT(xy_list, im);
      if (xys == R_NilValue || INTEGER(GET_DIM(xys))[0] != nobj || INTEGER(GET_DIM(xys))[1] < 2) continue;
      xy = REAL(xys);

      dst = REAL(img);
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
            for(nc=0; nc<nbChannels; nc++) {
              stride = im*nbChannels*nx*ny+nc*nx*ny;
              shift = index*nbChannels*snx*sny + nc*snx*sny;
              dst[pxi + shift] = dref[x + y * nx + stride];
            }  
      	  }
      	}
      }
    } // nobj>0
    else {
      img = R_NilValue;
    }
    if (nz == 1) res = img;
    else SET_VECTOR_ELT(res, im, img);
  } // im

  UNPROTECT( nprotect );
  return res;
}
