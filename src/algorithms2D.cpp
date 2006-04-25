#include "algorithms2D.h"
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
typedef vector<double> VectorDouble;
const double BACKGROUND = 0;
/*
Object count idea: how to count objects based on original distance maps
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
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
void object_count(double* data, double* origData, ColRow& imsize, double* param, VectorDouble& x, VectorDouble& y, VectorDouble& size, VectorDouble& intensity);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP objectCount(SEXP rimage, SEXP rOrigImage, SEXP params) {
    if (!assertImage(rimage))
        error("Wrong argument class, Image expected");
    if (LOGICAL(GET_SLOT(rimage, mkString("rgb")))[0])
        error("Algorithm works for grayscale images only");
    try {
        int * dim = INTEGER(GET_DIM(rimage));
        ColRow imsize(dim[0], dim[1]);
        int nimages = dim[2];
        double * data;
        double * origData;
        SEXP res = R_NilValue;
        /* for a stack we return a list, otherwise a single matrix */
        if (nimages > 1)
            PROTECT(res = allocVector(VECSXP, nimages));
        SEXP * items = new SEXP[nimages];
        SEXP * dims  = new SEXP[nimages];
        for (int i = 0; i < nimages; i++)
            items[i] = R_NilValue;
        VectorDouble id, x, y, size, intensity;
        double par[4];
        for (int i = 0; i < 4; i++)
            par[i] = REAL(params)[i]; /* 0 -min size, 1 - max size, 2 - tolerance, 3 - max objects */
        for (int i = 0; i < nimages; i++) {
            id.clear();
            x.clear();
            y.clear();
            size.clear();
            intensity.clear();
            /* it is assumed that rimage is alread a distMap!!!
               otherwise run the next line
               calc_dist_map(data, ncol, nrow, algorithm); */
            data = &(REAL(rimage)[i * imsize.col * imsize.row]);
            if (assertImage(rOrigImage))
                origData = &(REAL(rOrigImage)[i * imsize.col * imsize.row]);
            else
                origData = NULL;
            /* indexes and sizes of objects within one image are returned in vectors */
            object_count(data, origData, imsize, par, x, y, size, intensity);
            /*  copy all returned values to the list element*/
            int nobjects = x.size();
            for (int j = 0; j < nobjects; j++)
                id.push_back(i + 1);
            if (origData)
                PROTECT(items[i] = allocVector(REALSXP, nobjects * 5));
            else
                PROTECT(items[i] = allocVector(REALSXP, nobjects * 4));
            PROTECT(dims[i] = allocVector(INTSXP, 2));
            if (nobjects > 0) {
                if (origData)
                    INTEGER(dims[i])[1] = 5;
                else
                    INTEGER(dims[i])[1] = 4;
                INTEGER(dims[i])[0] = nobjects;
                SET_DIM(items[i], dims[i]);
            }
            for (int j = 0; j < nobjects; j++) {
                REAL(items[i])[j]     = id[j];
                REAL(items[i])[j + nobjects] = x[j];
                REAL(items[i])[j + 2 * nobjects] = y[j];
                REAL(items[i])[j + 3 * nobjects] = size[j];
                if (origData)
                    REAL(items[i])[j + 4 * nobjects] = intensity[j];
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
        UNPROTECT(2 * nimages); /* items[] and dims[] */
        if (nimages > 1)
            UNPROTECT(1); /* res */
        return res;
    }
    catch(exception &error_) {
        error(error_.what());
    }
    /* if it comes to this point I will be surprised */
    return R_NilValue;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
inline ColRow getMaxIndex(double * data, ColRow& imsize) {
    ColRow res(0, 0), counter(0, 0);
    for (counter.col = 0; counter.col < imsize.col; counter.col++)
        for (counter.row = 0; counter.row < imsize.row; counter.row++)
            if (data[INDEX(counter.col, counter.row, imsize.col)] > data[INDEX(res.col, res.row, imsize.col)])
                res = counter;
    return res;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
inline double dist(ColRow& pt1, ColRow& pt2) {
    return sqrt((long double)((pt1.col - pt2.col) * (pt1.col - pt2.col) + (pt1.row - pt2.row) * (pt1.row - pt2.row)));
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/* this function is recursive!!! */
/* counter also defines the number of the object filled in the image with negative sign - 1
   (-1) reserved for borders */
void ffill(double* data, double* origData, int col, int row, ColRow& start, ColRow& imsize, double value, double* param, double& area, double& intens, bool& error, int& counter) {
    /* check if there were any errors in previous recursive calls */
    if (error) return;
    /* check if point outside of the image */
    if (col < 0 || row < 0) return;
    /* if additionally it is bottom or right border, do not count object!!! */
    if (col >= imsize.col || row >= imsize.row) {
        error = true;
        return;
    }
    double newvalue = data[INDEX(col, row, imsize.col)];
    /* check if current point already on background */
    if (newvalue <= BACKGROUND)  return;
    /* check if we do not enter another maximum area with a given tolerance - another object */
    if (newvalue >= value + param[2]) return;
    ColRow pt(col, row);
    /* check if we are still within the 1.5x max object radius (factor because start can be non-cetral) */
    if (dist(pt, start) > 2.0 * param[1]) return;
    /* reset this to BG first */
    data[INDEX(col, row, imsize.col)] = -(counter + 1.0);
    /* update intensity if original data present */
    if (origData != NULL)
        intens += origData[INDEX(col, row, imsize.col)];
    /* add this point, otherwise this recursive thing will be infinite!!! */
    area++;
    ffill(data, origData, col + 1, row    , start, imsize, newvalue, param, area, intens, error, counter);
    ffill(data, origData, col - 1, row    , start, imsize, newvalue, param, area, intens, error, counter);
    ffill(data, origData, col    , row + 1, start, imsize, newvalue, param, area, intens, error, counter);
    ffill(data, origData, col    , row - 1, start, imsize, newvalue, param, area, intens, error, counter);
    ffill(data, origData, col + 1, row + 1, start, imsize, newvalue, param, area, intens, error, counter);
    ffill(data, origData, col - 1, row - 1, start, imsize, newvalue, param, area, intens, error, counter);
    ffill(data, origData, col - 1, row + 1, start, imsize, newvalue, param, area, intens, error, counter);
    ffill(data, origData, col + 1, row - 1, start, imsize, newvalue, param, area, intens, error, counter);
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
void object_count(double* data, double* origData, ColRow& imsize, double* param, VectorDouble& x, VectorDouble& y, VectorDouble& size, VectorDouble& intensity) {
    /* get the first maximum-value point */
    ColRow max(getMaxIndex(data, imsize));
    /* convert col and row into index - this is what we store */
    int index = INDEX(max.col, max.row, imsize.col);
    int indexold = -1;
    int counter = 0;
    double area, intens;
    bool error;
    /* repeat the thing until our next max value is background or we reached the maximum number of objects */
    while(data[index] > BACKGROUND + param[2] && counter < param[3] && index != indexold) {
        counter++;
        area = 0;
        intens = 0;
        error = false;
        /* fill the surrounding area and get the size of the filling */
        ffill(data, origData, max.col, max.row, max, imsize, data[index], param, area, intens, error, counter);
        /* add this object to the list if it is large enough */
        if (area >= param[0] && !error) {
            x.push_back(max.col);
            y.push_back(max.row);
            size.push_back(area);
            intensity.push_back(intens);
        }
        /* before exit try to find the next object - the next maximum */
        indexold = index;
        max = getMaxIndex(data, imsize);
        index = INDEX(max.col, max.row, imsize.col);
    }
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
