#include <stdio.h>
#include <math.h>
#include <R.h>
#include <Rdefines.h>
#include "ocontour.h"

#define MAX_NB_POINTS 65535

static int rotr[8]={-1,-1,-1,0,1,1,1,0};
static int rotc[8]={-1,0,1,1,1,0,-1,-1};
static int dir [9]={5,4,3,6,-1,2,7,0,1};

SEXP ocontour(SEXP _image) {
  int *image, width, height;
  int i, j, k, direction, nbCells;
  int r, c, ocr, occ, ndirection, nr, nc;
  int nprotect=0, nboc,  *octemp;
  SEXP _res, _oc;
  
  // Transfer variables
  height=INTEGER(GET_DIM(_image))[0];
  width=INTEGER(GET_DIM(_image))[1];
  image=INTEGER(_image);
  
  // Compute number of objects
  nbCells=0;
  for (i=0; i<width*height; i++) {
    if (image[i]>nbCells) nbCells=image[i];
  }
  nbCells++;
 
  // Output result
  _res = PROTECT(allocVector(VECSXP, nbCells));
  nprotect++;

  // Temporary vector to store the current oriented contour
  octemp=(int *)R_Calloc(MAX_NB_POINTS*2+1, int);

  // For each object, except the 0-th one (background)
  for (k=1; k<nbCells; k++) {
    nboc=0;

    // Find min (r,c) starting point for object k
    i=0;
    while (image[i]!=k && i<width*height) i++;
    if (i!=width*height) {
      r=i%height;
      c=i/height;

      // Starting points of the oriented coutour
      ocr=r;
      occ=c;

      // Turn around the edges of the object
      direction=0;
      do {
      	// Stores (r,c) in the oriented contour matrix
      	octemp[2*nboc]=r;
      	octemp[2*nboc+1]=c;
      	if (nboc<MAX_NB_POINTS) nboc++;
        
      	// Change direction
      	for (j=0; j<8; j++) {
      	  ndirection=(j+direction)%8;
      	  nr=r+rotr[ndirection];
      	  nc=c+rotc[ndirection];
      	  if (nr>=0 && nc>=0 && nr<height && nc<width) {
      	    if (image[nr+nc*height]==k) break;
      	  }
      	}
      	if (j!=8) {
      	  direction=dir[(nr-r+1)+3*(nc-c+1)];
      	  r=nr;
      	  c=nc;
      	}
      } while (r!=ocr || c!=occ);
    }
    // Copy octemp in an element of _res
    _oc = PROTECT(allocVector(INTSXP, nboc*2));
    nprotect++;
    SET_VECTOR_ELT(_res, k, _oc);
    memcpy(INTEGER(_oc), octemp, nboc*2*sizeof(int));
  } // k

  // Free oct
  R_Free(octemp);

  UNPROTECT (nprotect);
  return(_res);
}
