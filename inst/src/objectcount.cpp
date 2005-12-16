/*
idea: how to count objects based on original distance maps
--------------------------------------------------
while (maxValue of the array > 0)
    - get index of the first occurance of the max value. one of our objects
    - values at points about this one are not larger - add them to the selection
      until we hit background or the neighbouring value will be larger than at the current
      point or we reach the maximum radius (= max object diameter)
    - the size of the selection - somehow proportional to the object size (think of it)
      as well as the value at the max point is proportional to the object size
    - reset the selection to the background to exclude from consequent runs!
end
*/

#include "objectcount.h"
/* this one is not needed as input is considered to be the result of distmap
   but if this changes uncomment it
#include "distmaps.h"
*/
#include "conversions.h"
#include <R_ext/Error.h>
#include <vector>
#include <iostream>
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
struct ColRow {
    ColRow(int col, int row) {
        this->col = col;
        this->row = row;
    }
    ColRow(const ColRow& obj) {
        col = obj.col;
        row = obj.row;
    }
    int col;
    int row;
};
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
typedef vector<int> VectorInt;
const int BACKGROUND = 0;
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
void object_count(int* data, ColRow& size, double* param, VectorInt& indexes, VectorInt& areas);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP objectCount(SEXP rimage, SEXP params) {
    try {
        int * dim = INTEGER(GET_DIM(rimage));
        int ndim = LENGTH(GET_DIM(rimage));
        ColRow size(dim[0], dim[1]);
        int nimages = 1;
        if (ndim > 2)
            nimages = dim[2];
        int * data;
        SEXP res = R_NilValue;
        /* for a stack we return a list, otherwise a single matrix */
        if (nimages > 1)
            PROTECT(res = allocVector(VECSXP, nimages));
        SEXP * items = new SEXP[nimages];
        SEXP * dims  = new SEXP[nimages];
        for (int i = 0; i < nimages; i++)
            items[i] = R_NilValue;
        VectorInt indexes, areas;
        double par[4];
        for (int i = 0; i < 4; i++)
            par[i] = REAL(params)[i]; // 0 -min size, 1 - max size, 2 - tolerance, 3 - max objects
//        cout << maxsize << endl;
        for (int i = 0; i < nimages; i++) {
            indexes.clear();
            areas.clear();
            /* it is assumed that rimage is alread a distMap!!!
               otherwise run the next line
               calc_dist_map(data, ncol, nrow, algorithm);
            */
            data = &(INTEGER(rimage)[i * size.col * size.row]);
            /* indexes and sizes of objects within one image are returned in vectors */
            object_count(data, size, par, indexes, areas);
            /* copy all returned values to the list element */
            int nobjects = indexes.size();
            PROTECT(items[i] = allocVector(INTSXP, nobjects * 2));
            PROTECT(dims[i] = allocVector(INTSXP, 2));
            if (nobjects > 0) {
                INTEGER(dims[i])[0] = 2;
                INTEGER(dims[i])[1] = nobjects;
                SET_DIM(items[i], dims[i]);
            }
            for (int j = 0; j < nobjects; j++) {
                INTEGER(items[i])[j * 2] = indexes[j];
                INTEGER(items[i])[j * 2 + 1] = areas[j];
            }
        }
        /* add all list elements to the list */
        if (nimages > 1)
            for (int i = nimages - 1; i >= 0; i--) {
                SET_VECTOR_ELT(res, i, items[i]);
            }
        else
            res = items[0];
        delete[] items;
        delete[] dims;
        UNPROTECT(2 * nimages); // items[] and dims[]
        if (nimages > 1)
            UNPROTECT(1); // res
        return res;
    }
    catch(...) {
        error("exception within distMap c++ routine");
    }
    /* if it comes to this point I will be surprised */
    return R_NilValue;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
inline ColRow getMaxIndex(int * data, ColRow& size) {
    ColRow res(0, 0), counter(0, 0);
    for (counter.col = 0; counter.col < size.col; counter.col++)
        for (counter.row = 0; counter.row < size.row; counter.row++)
            if (data[INDEX(counter.col, counter.row, size.col)] > data[INDEX(res.col, res.row, size.col)])
                res = counter;
    return res;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
inline double dist(ColRow& pt1, ColRow& pt2) {
    return sqrt((pt1.col - pt2.col) * (pt1.col - pt2.col) + (pt1.row - pt2.row) * (pt1.row - pt2.row));
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* this function is recursive!!! */
void ffill(int * data, int col, int row, ColRow& start, ColRow& size, int value, double* param, int& area, bool& error) {
    /* check if there were any errors in previous recursive calls */
    if (error) return;
    /* check if point outside of the image */
    if (col < 0 || row < 0) return;
    /* if additionally it is bottom or right border, do not count object!!! */
    if (col >= size.col || row >= size.row) {
        error = true;
        return;
    }
    int newvalue = data[INDEX(col, row, size.col)];
    /* check if current point already on background */
    if (newvalue == BACKGROUND) return;
    /* check if we do not enter another maximum area with a given tolerance - another object */
    if (newvalue > value + param[2]) return;
    ColRow pt(col, row);
    /* check if we are still within the 1.5x max object radius (factor because start can be non-cetral) */
    if (dist(pt, start) > 2.0 * param[1]) return;
    /* add this point and reset this to BG first, otherwise this recursive thing will be infinite!!! */
    area++;
    data[INDEX(col, row, size.col)] = 0;
    ffill(data, col + 1, row    , start, size, newvalue, param, area, error);
    ffill(data, col - 1, row    , start, size, newvalue, param, area, error);
    ffill(data, col    , row + 1, start, size, newvalue, param, area, error);
    ffill(data, col    , row - 1, start, size, newvalue, param, area, error);
    ffill(data, col + 1, row + 1, start, size, newvalue, param, area, error);
    ffill(data, col - 1, row - 1, start, size, newvalue, param, area, error);
    ffill(data, col - 1, row + 1, start, size, newvalue, param, area, error);
    ffill(data, col + 1, row - 1, start, size, newvalue, param, area, error);
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
void object_count(int * data, ColRow& size, double* param, VectorInt & indexes, VectorInt & areas) {
    /* get the first maximum-value point */
    ColRow max(getMaxIndex(data, size));
    /* convert col and row into index - this is what we store */
    int index = INDEX(max.col, max.row, size.col);
    int area, counter = 0;
    bool error;
    /* repeat the thing until our next max value is background or we reached the maximum number of objects */
    while(data[index] > BACKGROUND && counter < param[3]) {
        counter++;
        area = 0;
        error = false;
        /* fill the surrounding area and get the size of the filling */
        ffill(data, max.col, max.row, max, size, data[index], param, area, error);
        /* add this object to the list if it is large enough */
        if (area >= param[0] && !error) {
            indexes.push_back(index);
            areas.push_back(area);
        }
        /* before exit try to find the next object - the next maximum */
        max = getMaxIndex(data, size);
        index = INDEX(max.col, max.row, size.col);
    }
}
