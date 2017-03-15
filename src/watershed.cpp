#include "watershed.h"

/* -------------------------------------------------------------------------
Watershed transform for Image
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

#include "tools.h"
#include <R_ext/Error.h>

/* list of STL, C++ */
#include <list>

#define BG 0.0

struct TheSeed {
    int index, seed;
};

typedef std::list<int>     IntList;
typedef std::list<TheSeed> SeedList;

double check_multiple( double *, double *, int &, IntList &, SeedList &, double &, int &, int & );

/*----------------------------------------------------------------------- */
SEXP
watershed (SEXP x, SEXP _tolerance, SEXP _ext) {
    SEXP res;
    int im, i, j, nx, ny, nz, ext;
    double tolerance;

    nx = INTEGER ( GET_DIM(x) )[0];
    ny = INTEGER ( GET_DIM(x) )[1];
    nz = getNumberOfFrames(x,0);
    tolerance = REAL( _tolerance )[0];
    ext = INTEGER( _ext )[0];

    PROTECT( res = allocVector(INTSXP, XLENGTH(x)) );
    DUPLICATE_ATTRIB(res, x);
    
    int * index = new int[ nx * ny ];
    double * frame = new double[ nx * ny ];

    for ( im = 0; im < nz; im++ ) {

        double * src = &( REAL(x)[ im * nx * ny ] );
        int * tgt = &( INTEGER(res)[ im * nx * ny ] );

        /* generate pixel index and negate the image -- filling wells */
        for ( i = 0; i < nx * ny; i++ ) {
        	  frame[ i ] = -src[ i ];
        	  index[ i ] = i;
        }
        /* from R includes R_ext/Utils.h */
        /* will resort frame as well */
        rsort_with_index( frame, index, nx * ny );
        /* reassign frame as it was reset above but keep new index */
        for ( i = 0; i < nx * ny; i++ )
            frame[ i ] = ( src[i]==0 ? 0 : -src[i] ); // avoid turning +0 into -0

        SeedList seeds;  /* indexes of all seed starting points, i.e. lowest values */

        IntList  equals; /* queue of all pixels on the same gray level */
        IntList  nb;     /* seed values of assigned neighbours */
        int ind, indxy, nbseed, x, y, topseed = 0;
        IntList::iterator it;
        TheSeed newseed;
        PointXY pt;
        bool isin;
        /* loop through the sorted index */
        for ( i = 0; i < nx * ny && src[ index[i] ] > BG; ) {
            /* pool a queue of equally lowest values */
            ind = index[ i ];
            equals.push_back( ind );
            for ( i = i + 1; i < nx * ny; ) {
                if ( src[ index[i] ] != src[ ind ] ) break;
                equals.push_back( index[i] );
                i++;
            }
            while ( !equals.empty() ) {
                /* first check through all the pixels if we can assign them to
                 * existing objects, count checked and reset counter on each assigned
                 * -- exit when counter equals queue length */
                for ( j = 0; j < (int) equals.size(); ) {
		  if ((j%1000)==0) R_CheckUserInterrupt();
                    ind = equals.front();
                    equals.pop_front();
                    /* check neighbours:
                     * - if exists one, assign
                     * - if two or more check what should be combined and assign to the steepest
                     * - if none, push back */
                    /* reset j to 0 every time we assign another pixel to restart the loop */
                    nb.clear();
                    POINT_FROM_INDEX(pt, ind, nx)
                    /* determine which neighbour we have, push them to nb */
                    for ( x = pt.x - ext; x <= pt.x + ext; x++ )
                        for ( y = pt.y - ext; y <= pt.y + ext; y++ ) {
                            if ( x < 0 || y < 0 || x >= nx || y >= ny || (x == pt.x && y == pt.y) ) continue;
                            indxy = x + y * nx;
                            nbseed = (int) frame[ indxy ];
                            if ( nbseed < 1 ) continue;
                            isin = false;
                            for ( it = nb.begin(); it != nb.end() && !isin; it++ )
                                if ( nbseed == *it ) isin = true;
                            if ( !isin ) nb.push_back( nbseed );
                        }
                    if ( nb.size() == 0 ) {
                        /* push the pixel back and continue with the next one */
                        equals.push_back( ind );
                        j++;
                        continue;
                    }
                    frame[ ind ] = check_multiple(frame, src, ind, nb, seeds, tolerance, nx, ny );
                    /* we assigned the pixel, reset j to restart neighbours detection */
                    j = 0;
                }
                /* now we have assigned all that we could */
                if ( !equals.empty() ) {
                    /* create a new seed for one pixel only and go back to assigning neighbours */
                    topseed++;
                    newseed.index = equals.front();
                    newseed.seed = topseed;
                    equals.pop_front();
                    frame[ newseed.index ] = topseed;
                    seeds.push_back( newseed );
                }
            } // assigning equals
        } // sorted index

        /* now we need to reassign indexes while some seeds could be removed */
        double * finseed = new double[ topseed ];
        for ( i = 0; i < topseed; i++ )
            finseed[ i ] = 0;
        i = 0;
        while ( !seeds.empty() ) {
            newseed = seeds.front();
            seeds.pop_front();
            finseed[ newseed.seed - 1 ] = i + 1;
            i++;
        }
        for ( i = 0; i < nx * ny; i++ ) {
            j = (int) frame[ i ];
            tgt[ i ] = ( 0 < j && j <= topseed ) ? finseed[ j - 1 ] : 0;
        }
        delete[] finseed;

    } // loop through images

    delete[] index;
    delete[] frame;

    UNPROTECT (1);
    return res;
}

bool
get_seed( SeedList & seeds, int & seed, SeedList::iterator & sit ) {
    for ( sit = seeds.begin(); sit != seeds.end(); sit++ )
        if ( (*sit).seed == seed ) return true;
    return false;
}

double
check_multiple( double * tgt, double * src, int & ind, IntList & nb, SeedList & seeds, double & tolerance, int & nx, int & ny ) {
    if ( nb.size() == 1 ) return nb.front();
    if ( nb.size() <  1 ) return 0.0; // dumb protection

    double diff, maxdiff = 0.0, res = 0.0;
    int i;
    IntList::iterator  it;
    SeedList::iterator sit;
    PointXY ptsit, pt;
    POINT_FROM_INDEX(pt, ind, nx)
    double distx, dist = DBL_MAX;

    /* maxdiff */
    for ( it = nb.begin(); it != nb.end(); it++ ) {
        if ( !get_seed( seeds, *it, sit ) ) continue;
        diff = fabs( src[ ind ] - src[ (*sit).index ] );
        if ( diff > maxdiff ) {
            maxdiff = diff;
            /* assign result to the steepest until and if it not assigned to closest over the tolerance */
            if ( dist == DBL_MAX )
                res = *it;
        }
        /* we assign to the closest centre which is above tolerance, if none than to maxdiff */
        if ( diff >= tolerance ) {
            POINT_FROM_INDEX(ptsit, (*sit).index, nx)
            distx = DISTANCE_XY(pt, ptsit);
            if ( distx < dist ) {
                dist =  distx;
                res = * it;
            }
        }

    }
    /* assign all that need assignment to res, which has maxdiff */
    for ( it = nb.begin(); it != nb.end(); it++ ) {
        if ( *it == res ) continue;
        if ( !get_seed( seeds, *it, sit ) ) continue;
        if ( fabs( src[ ind ] - src[ (*sit).index ] ) >= tolerance ) continue;
        for ( i = 0; i < nx * ny; i++ )
            if ( tgt[ i ] == *it )
                tgt[ i ] = res;
        seeds.erase( sit );
    }
    return res;
}
