#include "distmaps.h"
#include "conversions.h"

#include <R_ext/Error.h>
#include <iostream>

using namespace std;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
inline void calc_dist_map(int * data, int& ncol, int& nrow, int& alg);
inline void lz_dist_map(int * data, int& ncol, int& nrow);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP distMap(SEXP rimage, SEXP alg) {
    try {
        int * dim = INTEGER(GET_DIM(rimage));
        int ndim = LENGTH(GET_DIM(rimage));
        int algorithm = INTEGER(alg)[0];
        /* see if R is that clever: we will modify the supplied image and then return it
        let's see if we then have both - new and old */
        int ncol = dim[0];
        int nrow = dim[1];
        int nimages = 1;
        if (ndim > 2)
            nimages = dim[2];
        int * data;
        for (int i = 0; i < nimages; i++) {
            data = &(INTEGER(rimage)[i * ncol * nrow]);
            calc_dist_map(data, ncol, nrow, algorithm);
        }
    }
    catch(...) {
        error("exception within distMap c++ routine");
    }
    return(rimage);
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
inline void calc_dist_map(int * data, int& ncol, int& nrow, int& alg) {
    switch(alg) {
        default:
            lz_dist_map(data, ncol, nrow);
    };
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* DEBUG FUNCTION - REMOVE WHEN CODE IS CHECKED */
void print_data(int* data, int& ncol, int& nrow) {
  int i,j;
  Rprintf("\n");
  for(i=0; i<ncol; i++) {
    for(j=0; j<nrow; j++)
      Rprintf("%12d", MAT_ELT(data, i, j, ncol));
    Rprintf("\n");
  }
  Rprintf("\n");
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  adapted from Animal Package by Rocardo Fabbri:: distmap-lz.c
  algorithm: R. Lotufo, F. Zampirolli, SIBGRAPI 2001, 100-105, 2001
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
inline void lz_dist_map(int * data, int& ncol, int& nrow) {
    /* WH: a very large number - but not so large as that adding more to could lead to
       integer overflow */
    int verylarge = INT_MAX - ncol * ncol - nrow * nrow;

    try {
        /* consider 0 as background and set the rest to large value for binary type */
        /* OS: basically we can accept any image type as input: binary, gray, RGB */
        int i, j;
        for (i = 0; i < ncol * nrow; i++)
           if (data[i] != 0)
               data[i] = verylarge;
        /* DEBUG */
        /* print_data(data, ncol, nrow); */

        /* STEP 1 vertical */
        /* i are the columns, j are the rows */
        int inc;
        for (i = 0; i < ncol; i++) {
            inc = 1;
            for (j = 1; j < nrow; j++)
                if (MAT_ELT(data, i, j, ncol) > MAT_ELT(data, i, j-1, ncol) + inc) {
                    MAT_ELT(data, i, j, ncol) = MAT_ELT(data, i, j-1, ncol) + inc;
                    /* DEBUG */
                    /* Rprintf("Step 1, for-loop 1: inc=%d -> data[%d, %d]=%d\n", inc, i, j, MAT_ELT(data, i, j, ncol)); */
                    inc += 2;
                }
                else
                   inc = 1;
            inc = 1;
            for (j = nrow - 2; j >= 0; j--)
                if (MAT_ELT(data, i, j, ncol) > MAT_ELT(data, i, j+1, ncol) + inc) {
                    MAT_ELT(data, i, j, ncol) = MAT_ELT(data, i, j+1, ncol) + inc;
                    /* DEBUG */
                    /* Rprintf("Step 1, for-loop 2: inc=%d -> data[%d, %d]=%d\n", inc, i, j, MAT_ELT(data, i, j, ncol)); */
                    inc += 2;
                }
                else
                    inc = 1;
        }
        /* DEBUG */
        /* print_data(data, ncol, nrow); */
        /* STEP 2 horizontal */
        int * wq  = (int *) R_alloc(ncol, sizeof(int));
        int * wq2 = (int *) R_alloc(ncol, sizeof(int));
        int * eq  = (int *) R_alloc(ncol, sizeof(int));
        int * eq2 = (int *) R_alloc(ncol, sizeof(int));
        int * point;
        int * ptr = data;
        int wqEnd, wqIni, eqEnd, eqIni, wq2End, eq2End;
        int * tmpq;
        for (j = 0; j < nrow; j++) {
            for (i = 0; i < ncol - 1; ++i) {
                wq[i] = i + 1;
                eq[i] = ncol - i - 2;
            }
            wqEnd = eqEnd = ncol - 1;
            wqIni = eqIni = 0;
            inc = 1;
            /* while queue is not empty */
            while (wqEnd > wqIni || eqEnd > eqIni) {
                wq2End = eq2End = 0;
                while (eqEnd > eqIni) {
                    i = eq[eqIni++];   /* remove from queue */
                    point = ptr + i;
                    if (point[1] > *point + inc) {
                        point[1] = *point + inc;
                        if (i + 1 < ncol - 1)
                            eq2[eq2End++] = i + 1; /* insert into queue */
                    }
                }
                while (wqEnd > wqIni) {
                    i = wq[wqIni++];
                    point = ptr + i - 1;
                    if (*point > point[1] + inc) {
                        *point = point[1] + inc;
                        if (i - 1 > 0)
                            wq2[wq2End++] = i - 1;
                    }
                }
                tmpq = wq; wq = wq2; wq2 = tmpq;
                tmpq = eq; eq = eq2; eq2 = tmpq;
                wqEnd = wq2End;
                eqEnd = eq2End;
                wqIni = 0;
                eqIni = 0;
                inc += 2;
            }
            ptr += ncol;
        }
    }
    catch(...) {
      error("memory problems in 'lz_dist_map' c++ routine");
    }
}
