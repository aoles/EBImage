/* -------------------------------------------------------------------------
Implementation of the Voronoi-based segmentation on image manifolds [1]

The code below is based on the 'IdentifySecPropagateSubfunction.cpp'
module (revision 4730) of CellProfiler [2]. CellProfiler is released
under the terms of GPL, however the LGPL license was granted by T. Jones
on Feb 7, 07 to use the code in the above file for this project.

[1] T. Jones, A. Carpenter and P. Golland,
    "Voronoi-Based Segmentation of Cells on Image Manifolds"
    CVBIA05 (535-543), 2005

[2] CellProfiler: http://www.cellprofiler.org

Copyright (C) of the original CellProfiler code:
 - Anne Carpenter <carpenter@wi.mit.edu>
 - Thouis Jones <thouis@csail.mit.edu>
 - In Han Kang <inthek@mit.edu>

Copyright (C) of the implementation below:
 - Oleg Sklyar <osklyar@ebi.ac.uk>
 - Wolfgang Huber <huber@ebi.ac.uk>

See: ../LICENSE for license, LGPL.

------------------------------------------------------------------------- */

#include <math.h>
#include <queue>
#include <vector>
#include <iostream>
#include "propagate.h"
#include <R_ext/Error.h>
#include "tools.h"
using namespace std;

#define IJ(i,j) ((j)*m+(i))

class Pixel { 
public:
  double distance;
  unsigned int i, j;
  int label;
  Pixel (double ds, unsigned int ini, unsigned int inj, int l) : 
    distance(ds), i(ini), j(inj), label(l) {}
};

struct Pixel_compare { 
 bool operator() (const Pixel& a, const Pixel& b) const 
 { return a.distance > b.distance; }
};

typedef priority_queue<Pixel, vector<Pixel>, Pixel_compare> PixelQueue;

static double
clamped_fetch(double *image, 
              int i, int j,
              int m, int n)
{
  if (i < 0) i = 0;
  if (i >= m) i = m-1;
  if (j < 0) j = 0;
  if (j >= n) j = n-1;

  return (image[IJ(i,j)]);
}

static double
Difference(double *image,
           int i1,  int j1,
           int i2,  int j2,
           unsigned int m, unsigned int n,
           double lambda)
{
  int delta_i, delta_j;
  double pixel_diff = 0.0;

  /* At some point, the width over which differences are calculated should probably be user controlled. */
  for (delta_j = -1; delta_j <= 1; delta_j++) {
    for (delta_i = -1; delta_i <= 1; delta_i++) {
      pixel_diff += fabs(clamped_fetch(image, i1 + delta_i, j1 + delta_j, m, n) - 
                         clamped_fetch(image, i2 + delta_i, j2 + delta_j, m, n));
    }
  } 
  double dEucl = (double(i1)-i2)*(double(i1)-i2) + (double(j1)-j2)*(double(j1)-j2);
  return (sqrt((pixel_diff*pixel_diff + lambda*dEucl)/(1.0 + lambda)));
  //return (sqrt(pixel_diff*pixel_diff + (fabs((double) i1 - i2) + fabs((double) j1 - j2)) * lambda * lambda));
}

static void
push_neighbors_on_queue(PixelQueue &pq, double dist,
                        double *image,
                        unsigned int i, unsigned int j,
                        unsigned int m, unsigned int n,
                        double lambda, int label,
                        int *labels_out)
{
  /* TODO: Check if the neighbor is already labelled. If so, skip pushing. 
   */    
    
  /* 4-connected */
  if (i > 0) {
    if ( 0 == labels_out[IJ(i-1,j)] ) // if the neighbor was not labeled, do pushing
      pq.push(Pixel(dist + Difference(image, i, j, i-1, j, m, n, lambda), i-1, j, label));
  }                                                                   
  if (j > 0) {                                                        
    if ( 0 == labels_out[IJ(i,j-1)] )   
      pq.push(Pixel(dist + Difference(image, i, j, i, j-1, m, n, lambda), i, j-1, label));
  }                                                                   
  if (i < (m-1)) {
    if ( 0 == labels_out[IJ(i+1,j)] ) 
      pq.push(Pixel(dist + Difference(image, i, j, i+1, j, m, n, lambda), i+1, j, label));
  }                                                                   
  if (j < (n-1)) {              
    if ( 0 == labels_out[IJ(i,j+1)] )   
      pq.push(Pixel(dist + Difference(image, i, j, i, j+1, m, n, lambda), i, j+1, label));
  } 

  /* 8-connected */
  if ((i > 0) && (j > 0)) {
    if ( 0 == labels_out[IJ(i-1,j-1)] )   
      pq.push(Pixel(dist + Difference(image, i, j, i-1, j-1, m, n, lambda), i-1, j-1, label));
  }                                                                       
  if ((i < (m-1)) && (j > 0)) {                                           
    if ( 0 == labels_out[IJ(i+1,j-1)] )   
      pq.push(Pixel(dist + Difference(image, i, j, i+1, j-1, m, n, lambda), i+1, j-1, label));
  }                                                                       
  if ((i > 0) && (j < (n-1))) {                                           
    if ( 0 == labels_out[IJ(i-1,j+1)] )   
      pq.push(Pixel(dist + Difference(image, i, j, i-1, j+1, m, n, lambda), i-1, j+1, label));
  }                                                                       
  if ((i < (m-1)) && (j < (n-1))) {
    if ( 0 == labels_out[IJ(i+1,j+1)] )   
      pq.push(Pixel(dist + Difference(image, i, j, i+1, j+1, m, n, lambda), i+1, j+1, label));
  }
  
}

static void _propagate(int *labels_in, double *im_in,
                      int *mask_in, int *labels_out,
                      double *dists,
                      unsigned int m, unsigned int n,
                      double lambda)
{
  /* TODO: Initialization of nuclei labels can be simplified by labeling
   *       the nuclei region first, then make the queue prepared for 
   *       propagation
   */
  unsigned int i, j;
  PixelQueue pixel_queue;

  /* initialize dist to Inf, read labels_in and wrtite out to labels_out */
  for (j = 0; j < n; j++) {
    for (i = 0; i < m; i++) {
      dists[IJ(i,j)] = R_PosInf;            
      labels_out[IJ(i,j)] = labels_in[IJ(i,j)];
    }
  }
  /* if the pixel is already labeled (i.e, labeled in labels_in) and within a mask, 
   * then set dist to 0 and push its neighbors for propagation */
  for (j = 0; j < n; j++) {
    for (i = 0; i < m; i++) {        
      int label = labels_in[IJ(i,j)];
      if ((label > 0) && (mask_in[IJ(i,j)])) {
        dists[IJ(i,j)] = 0.0;
        push_neighbors_on_queue(pixel_queue, 0.0, im_in, i, j, m, n, lambda, label, labels_out);
      }
    }
  }

  while (! pixel_queue.empty()) {
    Pixel p = pixel_queue.top();
    pixel_queue.pop();
    if (! mask_in[IJ(p.i, p.j)]) continue;
    if ((dists[IJ(p.i, p.j)] > p.distance) && (mask_in[IJ(p.i,p.j)])) {
      dists[IJ(p.i, p.j)] = p.distance;
      labels_out[IJ(p.i, p.j)] = p.label;
      push_neighbors_on_queue(pixel_queue, p.distance, im_in, p.i, p.j, m, n, lambda, p.label, labels_out);
    }
  }
}

// R entry point
SEXP propagate(SEXP _x, SEXP _seeds, SEXP _mask, SEXP _lambda) {
  SEXP res;
  int nx = INTEGER(GET_DIM(_x))[0];
  int ny = INTEGER(GET_DIM(_x))[1];
  int nz = getNumberOfFrames(_x, 0);
  double lambda = REAL(_lambda)[0];
  int nprotect = 0;

  PROTECT( res = allocVector(INTSXP, XLENGTH(_x)) );
  nprotect++;
  DUPLICATE_ATTRIB(res, _x);

  double *dists = (double *)R_Calloc(nx*ny, double);
  int *mask;
  if (_mask == R_NilValue) {
    mask = (int *) R_Calloc(nx*ny, int);
    for (int i=0; i<nx*ny; mask[i++]=1);
  }
  
  for (int im=0; im<nz; im++) {
    double *x = &( REAL(_x)[im*nx*ny]);
    int *tgt = &( INTEGER(res)[im*nx*ny]);
    int *seeds = &( INTEGER(_seeds)[im*nx*ny]);
    if (_mask != R_NilValue) mask = &( INTEGER(_mask)[im*nx*ny]);

    _propagate(seeds, x, mask, tgt, dists, nx, ny, lambda); 
  }
  
  if (_mask == R_NilValue) R_Free(mask);
  R_Free(dists);
  UNPROTECT( nprotect );
  return res;
}
