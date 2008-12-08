// Fast exact L2/L1 distance map algorithm 
// Complexity: O(N^2*log(N)), where N is the edge of a square image
// Reference: Kolountzakis, Kutulakos: Fast Computation of the Euclidean Distance Map for Binary Images, Infor. Proc. Letters (1992)

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <limits.h>

#include <R.h>
#include <Rdefines.h>

#include "distmap.h"
#include "tools.h"

// Globals
int width,height,metric,*vj;
numeric *a,*d;

// Find all minimal distances of points (*,j) given their closest neighbours given in vj
// Complexity: O(N*log(N))
void find_ndist(int x1, int x2, int z1 ,int z2, int j) {
  static int k;
  static double dk,dmin;
  int x0,z0;

  x0=(x1+x2)/2;

  // find (z0,vj[z0]), coordinates of the nearest neighbour of (x0,j), in the range z1<=k<=z2 from the pixels (k,vj[k])
  dmin=R_PosInf;
  z0=0;
  for (k=z1;k<z2+1;k++) {
    if (vj[k]==INT_MAX) dk=R_PosInf;
    else {
      if (metric) dk=fabs(x0-k+0.0)+fabs(j-vj[k]+0.0);
      else dk=(x0-k+0.0)*(x0-k+0.0)+(j-vj[k]+0.0)*(j-vj[k]+0.0);
    }
    if (dk<dmin) {
      z0=k;
      dmin=dk;
    }
  }
  if (dmin==R_PosInf) z0=(z1+z2)/2;
  if (dmin<d[j+x0*width]) d[j+x0*width]=dmin;
    
  // child calls, with constrained ranges
  if (x1<=(x0-1)) find_ndist(x1,x0-1,z1,z0,j);
  if ((x0+1)<=x2) find_ndist(x0+1,x2,z0,z2,j);
}

// Compute minimal distances in one direction
void distmap_onesided(int right2left) {
  int i,j,k;

  // initialize vj
  for (i=0;i<height;i++) vj[i]=-1;

  for (j=0;j<width;j++) {
    // compute vj, knowing v(j-1)
    for (i=0;i<height;i++) {
      if (vj[i]<j) {
	k=j;
	if (right2left)	while (k<width) if (a[k+i*width]!=0) k++; else break;
	else while (k<width) if (a[width-1-k+i*width]!=0) k++; else break;
	if (k==width) vj[i]=INT_MAX;
	else vj[i]=k;
      }
    }

    if (right2left) find_ndist(0,height-1,0,height-1,j);
    else {
      for (i=0;i<height;i++) if (vj[i]!=INT_MAX) vj[i]=width-1-vj[i];
      find_ndist(0,height-1,0,height-1,width-1-j);
      for (i=0;i<height;i++) if (vj[i]!=INT_MAX) vj[i]=width-1-vj[i];
    }

    // check for user interruption
    R_CheckUserInterrupt();
  }
}

// Compute Euclidean (L2)/Manhattan (L1) distance map of matrix _a 
// Input: numeric matrix _a, of size width*height, where 0 is background and everything else is foreground. _a shouldn't contain any NAs
// Input: integer _metric. If 0, will compute Euclidean distance and Manhattan distance otherwise
// Output: distance matrix of same size as _a
SEXP distmap(SEXP _a, SEXP _metric) {
  SEXP res;
  int i,nprotect=0,nz;

  // check validity
  validImage(_a,0);

  // initialize width, height, dim
  width=INTEGER(GET_DIM(_a))[0];
  height=INTEGER(GET_DIM(_a))[1];
  nz=getNumberOfFrames(_a,0);

 // initialize vj, where (i,vj[i]) are the coordinates of the closest background pixel to a(i,j) with vj[i]>=j
  vj=(int *)R_Calloc(height,int);

  // initialize a
  a=REAL(_a);

  // initialize d, the output distance matrix
  PROTECT(res=Rf_duplicate(_a));
  nprotect++;
  d=REAL(res);
  for (i=0;i<height*width*nz;i++) d[i]=R_PosInf;
  
  // initialize dist, the distance type
  metric=INTEGER(_metric)[0];
   
  // do the job
  for (i=0;i<nz;i++) {
    distmap_onesided(1);
    distmap_onesided(0);
    a=a+height*width;
    d=d+height*width;
  }

  // final square root for Euclidean distance
  d=REAL(res);
  if (metric==0) for (i=0;i<height*width*nz;i++) d[i]=sqrt(d[i]);

  // free vj
  R_Free(vj);

  UNPROTECT (nprotect);
  return res;
}

