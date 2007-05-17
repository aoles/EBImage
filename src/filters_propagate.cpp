#include "filters_propagate.h"

/* -------------------------------------------------------------------------
Implementation of the Voronoi-based segmentation on image manifolds [2]

The code below is based on the 'IdentifySecPropagateSubfunction.cpp'
module (revision 3667) of CellProfiler [1,3]. CellProfiler is released
under the terms of GPL, however the LGPL license was granted by T. Jones
on Feb 7, 07 to use the code in the above file for this project.

If you reuse this code under the LGPL terms, you cannot
apply the terms of LGPL to CellProfiler itself or any code derived from
it, not even to the module for which LGPL was granted in this project.
You are only allowed to reuse under LGPL the code which you find here.

[1] A. Carpenter, T.R. Jones, M.R. Lamprecht, C. Clarke, I.H. Kang,
    O. Friman, D. Guertin, J.H. Chang, R.A. Lindquist, J. Moffat,
    P. Golland and D.M. Sabatini
    "CellProfiler: image analysis software for identifying and
    quantifying cell phenotypes", Genome Biology 2006, 7:R100

[2] T. Jones, A. Carpenter and P. Golland,
    "Voronoi-Based Segmentation of Cells on Image Manifolds"
    CVBIA05 (535-543), 2005

[3] CellProfiler: http://www.cellprofiler.org


Copyright (C) of the original CellProfiler code:
 - Anne Carpenter <carpenter@wi.mit.edu>
 - Thouis Jones <thouis@csail.mit.edu>
 - In Han Kang <inthek@mit.edu>

Copyright (C) of the implementation below:
 - Oleg Sklyar <osklyar@ebi.ac.uk>
 - Wolfgang Huber <huber@ebi.ac.uk>

See: ../LICENSE for license, LGPL.

When reusing the code please cite all of the above to clearly state
the authors of the algorithm and the code and provide the corresponding
references!
------------------------------------------------------------------------- */

#include <R_ext/Error.h>
#include <queue>

using namespace std;

#define INDEX(i,j) ((i) + (j) * nx)

class Pixel {
    public:
        double distance;
        int i, j;
        double seed;
        Pixel (double ds, int ini, int inj, double s) :
            distance(ds), i(ini), j(inj), seed(s) {}
};

struct Pixel_compare {
    bool operator() (const Pixel& a, const Pixel& b) const {
        return a.distance > b.distance;
    }
};

typedef priority_queue<Pixel, vector<Pixel>, Pixel_compare> PixelQueue;

/* forward declaration: distance calculation function */
inline double
deltaG (double *, int, int, int, int, int, int, double, int);

/* these will form indexes for a neighbourhood of surrounding pixels, going
 * firsth through the ones left-right and top-bottom and then diagonals */
static int ix[8] = {-1, 0, +1,  0, -1, +1, -1, +1};
static int jy[8] = { 0, 1,  0, -1, -1, +1, +1, -1};

/* ----  R Interface entry point  --------------------------------------- */
SEXP
lib_propagate (SEXP x, SEXP seeds_, SEXP mask_, SEXP dx_, SEXP lambda_) {
    SEXP res;
    int i, ii, j, jj, cntr, nprotect = 0;

    int nx = INTEGER ( GET_DIM(x) )[0];
    int ny = INTEGER ( GET_DIM(x) )[1];
    int nz = INTEGER ( GET_DIM(x) )[2];

    int dx = INTEGER( dx_ )[0];
    double lambda = REAL( lambda_ )[0];

    PROTECT ( res = Rf_duplicate(x) );
    nprotect++;
    SET_CLASS (res, mkString("IndexedImage") );

    /* we will keep distances here, reset for every new image */
    double * dists = new double[ nx * ny ];

    for ( int im = 0; im < nz; im++ ) {
        double * src =   &( REAL( x )[im * nx * ny] );
        double * tgt =   &( REAL( res )[im * nx * ny] );
        double * seeds = &( REAL( seeds_ )[im * nx * ny] );

        double * mask;
        if ( mask_ != R_NilValue )
            mask = &( REAL( mask_ )[im * nx * ny] );
        else
            mask = NULL;

        PixelQueue pixel_queue;
        double seed, d;
        int index;
        bool masked;
        /* main algorithm */

        /* res initialization */
        for ( index = 0; index < nx * ny; index++ )
            tgt[ index ] = 0.0;

        /* initialization */
        for ( j = 0; j < ny; j++ )
            for ( i = 0; i < nx; i++ ) {
                index = i + nx * j;
                masked = false;
                if ( mask )
                    if ( mask[ index] <= 0 )
                        masked = true;
                /* initialize distances */
                dists[ index ] = R_PosInf;

                /* mark seed in returns, indexing should start at 1; */
                /* 0.5 is reserved to mark contact regions (EBImage) */
                if ( (seed = seeds[index]) <= 0.9 || masked ) continue;
                tgt[ index ] = seed;
                dists[ index ] = 0.0;
                /* push neighbours onto the queue */
                for ( cntr = 0; cntr < 8; cntr++ ) {
                    ii = i + ix[cntr];
                    jj = j + jy[cntr];
                    if ( ii < 0 || ii >= nx || jj < 0 || jj >= ny ) continue;
                    /* at this initialization step we do not push pixels on the queue which are
                     * already assigned, i.e. which are seeds because their distance is 0 and cannot be smaller */
                    if ( tgt[ INDEX(ii, jj) ] > 0.9 ) continue;
                    pixel_queue.push( Pixel(deltaG(src, i, j, ii, jj, nx, ny, lambda, dx), ii, jj, seed) );
                }
            }

        /* propagation */
        /* the queue only has pixels around the seeds originally, but it
         * gets new pixels dynamically as the assigned regions extend */
        while ( !pixel_queue.empty() ) {
            /* get the topmost pixel as its distance is largerst */
            Pixel px = pixel_queue.top();
            index = INDEX(px.i, px.j);
            /* and remove it from the queue */
            pixel_queue.pop();

            masked = false;
            if ( mask )
                if ( mask[ index] <= 0 )
                    masked = true;

            /* if currently assigned distance for this pixel (PosInf)
             * is larger than the one stored in the pixel when pushing
             * onto the queue, then reassign distance and seed */
            if ( dists[ index ] <= px.distance || masked ) continue;
            dists[ index ] = px.distance;
            tgt[ index ] = px.seed;
            /* push neighbours onto the queue */
            for ( cntr = 0; cntr < 8; cntr++ ) {
                ii = px.i + ix[cntr];
                jj = px.j + jy[cntr];
                if ( ii < 0 || ii >= nx || jj < 0 || jj >= ny ) continue;
                index = INDEX(ii, jj) ;
                /* now we do not want to push onto the queue pixels that are already
                 * assigned to the same seed, we only update their distance. pixels assigned
                 * to other seeds are pushed up as their distance to this seed can be
                 * smaller later on (in L.163 above). anyway this should be much faster than
                 * CellProfile'r original algorithm as we do not resize the queue on
                 * pixels that we are not going to reassign */
                d = px.distance + deltaG(src, px.i, px.j, ii, jj, nx, ny, lambda, dx);
                if ( tgt[ index ] == px.seed ) {
                    if ( dists[ index ] > d )
                        dists[ index ] = d;
                    continue;
                }
                pixel_queue.push( Pixel(d, ii, jj, px.seed) );
            }
        }
    }

    delete[] dists;
    UNPROTECT( nprotect );
    return res;
}

inline double
clamped_fetch (double * data, int i, int j, int nx, int ny) {
  if (i < 0) i = 0;
  if (i >= nx) i = nx - 1;
  if (j < 0) j = 0;
  if (j >= ny) j = ny - 1;

  return data[ INDEX(i, j) ];
}

/* this is the distance evaluation in the modified metric */
inline double
deltaG ( double * src, int i1,  int j1, int i2,  int j2,
         int nx, int ny, double lambda, int dx) {
    int i, j;
    double dI = 0.0;

    for ( i = -dx; i <= dx; i++)
        for ( j = -dx; j <= dx; j++)
            dI += fabs( clamped_fetch(src, i1 + i, j1 + j, nx, ny) -
                        clamped_fetch(src, i2 + i, j2 + j, nx, ny));
    /* this is not because we calculate dI/dx as it is dx*(dI/dx)=dI, this is
    * because we want the mean value of the gradient in the region */
    dI /= dx * dx;

    double dEucl = (i2 - i1) * (i2 - i1) + (j2 - j1) * (j2 - j1);
    return sqrt( (dI * dI + lambda * dEucl) / (1.0 + lambda) );
    /* CellProfiler uses this formula
        double dManh = abs(i2 - i1) + abs(j2 - j1);
        return sqrt( dI*dI + dManh * lambda * lambda);
    */
}

