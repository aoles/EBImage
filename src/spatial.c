#include <R.h>
#include <Rdefines.h>
#include "spatial.h"
#include "tools.h"

inline double peekpixel(int tx, int ty, int z, int width, int height, double *a) {
  double v;
  if (tx<0) v=0;
  else if (ty<0) v=0;
  else if (tx>=width) v=0;
  else if (ty>=height) v=0;
  else v = a[tx+ty*width+z*width*height];
  return(v);
}

SEXP affine(SEXP _a, SEXP _b, SEXP _m, SEXP _filter) {
  int width, height, nz;
  int owidth, oheight;
  int filter;
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

  // get output image b data
  owidth = INTEGER(GET_DIM(_b))[0];
  oheight = INTEGER(GET_DIM(_b))[1];
  b = REAL(_b);

  // apply transform
  for (int z=0; z<nz; z++) {
    for (int y=0; y<oheight; y++) { 
      for (int x=0; x<owidth; x++) {
	double tx = m[0]*x + m[1]*y + m[2];
	double ty = m[3]*x + m[4]*y + m[5];
	int ftx = floor(tx);
	int fty = floor(ty);
	double dx = tx-ftx;
	double dy = ty-fty;
	double pa = peekpixel(ftx, fty, z, width, height, a);
	// bilinear filter?
	if (filter==1) {
	  double pb = peekpixel(ftx+1, fty, z, width, height, a);
	  double pc = peekpixel(ftx, fty+1, z, width, height, a);
	  double pd = peekpixel(ftx+1, fty+1, z, width, height, a);
	  pa = (1-dy)*(pa*(1-dx) + pb*dx) + dy*(pc*(1-dx) + pd*dx);
	}
	b[x + y*owidth + z*owidth*oheight] = pa;
      }
    }
  }

  return _b;
} 
