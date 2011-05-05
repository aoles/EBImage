#include <R.h>
#include <Rdefines.h>

#include "translate.h"
#include "tools.h"

SEXP translate(SEXP _a,SEXP _v) {
  SEXP _b;
  int width,height,nz;
  int x,y,z,nprotect=0;
  int tx,ty;
  double *a,*v,*b;

  // check image validity
  validImage(_a,0); 

  // initialize width, height, nz
  width=INTEGER(GET_DIM(_a))[0];
  height=INTEGER(GET_DIM(_a))[1];
  nz=getNumberOfFrames(_a,0);

  // initialize a
  a=REAL(_a);
  v=REAL(_v);

  // initialize d, the output distance matrix
  PROTECT(_b=Rf_duplicate(_a));
  b=REAL(_b);
   
  nprotect++;
 
  // Do the job
  for (z=0;z<nz;z++) {
    for (y=0;y<height;y++) { 
      for (x=0;x<width;x++) {
	tx=x+v[z];
	ty=y+v[z+nz];
	if (tx<0) tx=0;
	if (ty<0) ty=0;
	if (tx>width-1) tx=width-1;
	if (ty>height-1) ty=height-1;
	b[x+y*width+z*width*height]=a[tx+ty*width+z*width*height];
      }
    }
  }

  UNPROTECT (nprotect);
  return _b;
}

SEXP affine(SEXP _a, SEXP _m) {
  SEXP _b;
  int width, height, nz;
  int x, y, z, nprotect=0;
  int tx, ty;
  double *a, *m, *b, v;

  // check image validity
  validImage(_a, 0); 

  // initialize width, height, nz
  width = INTEGER(GET_DIM(_a))[0];
  height = INTEGER(GET_DIM(_a))[1];
  nz = getNumberOfFrames(_a, 0);

  // initialize a
  a = REAL(_a);
  m = REAL(_m);

  // initialize d, the output distance matrix
  PROTECT(_b=Rf_duplicate(_a));
  b = REAL(_b);
  
  nprotect++;
 
  // Do the job
  for (z=0; z<nz; z++) {
    for (y=0; y<height; y++) { 
      for (x=0; x<width; x++) {
	tx = round(m[0]*x + m[1]*y + m[2]);
	ty = round(m[3]*x + m[4]*y + m[5]);
	if (tx<0) v = 0;
	else if (ty<0) v = 0;
	else if (tx>=width) v = 0;
	else if (ty>=height) v = 0;
	else v = a[tx+ty*width+z*width*height];
	b[x + y*width + z*width*height] = v;
      }
    }
  }

  UNPROTECT (nprotect);
  return _b;
} 
