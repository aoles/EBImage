#include <R.h>
#include <Rdefines.h>
#include "spatial.h"
#include "tools.h"

static inline double peekpixel(int x, int y, int z, int w, int h, double *a, double bg) {
  return ((x<0 || x>=w || y<0 || y>=h) ? bg : a[x + y*w + z*w*h]);
}

SEXP affine(SEXP _a, SEXP _b, SEXP _m, SEXP _filter, SEXP _antialias) {
  int width, height, nz;
  int owidth, oheight;
  int filter, antialias;
  double *a, *m, *b;

  // check image validity
  validImage(_a, 0); 

  // initialize width, height, nz
  width = INTEGER(GET_DIM(_a))[0];
  height = INTEGER(GET_DIM(_a))[1];
  nz = getNumberOfFrames(_a, 0);
 
  // initialize a, m, filter
  a = REAL(_a);
  m = REAL(_m);
  
  filter = INTEGER(_filter)[0];
  antialias = INTEGER(_antialias)[0];
  // get output image b data
  owidth = INTEGER(GET_DIM(_b))[0];
  oheight = INTEGER(GET_DIM(_b))[1];
  b = REAL(_b);
  
  // apply transform
  for (int z=0, i=0; z<nz; z++) {
    for (int y=0; y<oheight; y++) { 
      for (int x=0; x<owidth; x++) {
        double pa, bg = b[i];
        
      	// bilinear filter?
      	if (filter==1) {
      	  double tx = m[0]*(x+.5) + m[1]*(y+.5) + m[2];
      	  double ty = m[3]*(x+.5) + m[4]*(y+.5) + m[5];
      	  int ftx = floor(tx);
      	  int fty = floor(ty);
      	  double dx = tx-ftx;
      	  double dy = ty-fty;
      	  pa = peekpixel(ftx, fty, z, width, height, a, bg);
      	  
      	  int minx = -2, miny = -2, maxx = width, maxy = height;
      	  
      	  if (antialias==0) {
            ++minx;
      	    ++miny;
      	    --maxx;
      	    --maxy;
      	  }
      	  
      	  if ( ftx>minx && fty>miny && ftx<maxx && fty<maxy) {
      	    double pb = peekpixel(ftx+1, fty, z, width, height, a, bg);
      	    double pc = peekpixel(ftx, fty+1, z, width, height, a, bg);
      	    double pd = peekpixel(ftx+1, fty+1, z, width, height, a, bg);
      	    pa = (1-dy)*(pa*(1-dx) + pb*dx) + dy*(pc*(1-dx) + pd*dx);
      	  }
      	}
      	else {
      	  double tx = m[0]*(x+.5) + m[1]*(y+.5) + m[2];
      	  double ty = m[3]*(x+.5) + m[4]*(y+.5) + m[5];
      	  int ftx = floor(tx);
      	  int fty = floor(ty);
      	  pa = peekpixel(ftx, fty, z, width, height, a, bg);
      	}
      	b[i++] = pa;
      }
    }
  }

  return _b;
} 
