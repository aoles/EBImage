#include "watershed.h"
#include "conversions.h"
#include <R_ext/Error.h>
#include <vector>
#include <algorithm>
#include <iostream>
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
const double BG = 0;
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
struct TheObject {
    TheObject(int index, double intensity, int size = 1, int edge = 0, int perimeter = 0) {
        this->index = index;
        this->intensity = intensity;
        this->size = size;
        this->edge = edge;
        this->perimeter = perimeter;
        dx = 0;
        dy = 0;
    }
    int index;          /* index of the object origin on the image */
    double intensity;   /* overall intensity */
    int size;           /* size of the object */
    int perimeter;      /* number of perimeter points */
    int edge;           /* number of edge points */
    double dx;          /* elongation in x direction */
    double dy;          /* elongation in y direction */
};
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
struct Point {
    Point() {
        x = 0;
        y = 0;
    }
    Point(int x, int y) {
        this->x = x;
        this->y = y;
    }
    Point(const Point& obj) {
        x = obj.x;
        y = obj.y;
    }
    int x;
    int y;
};
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
Point & coordFromIndex(int index, Point & size, Point & res) {
    res.y = (int)(index / size.x);
    res.x = index - res.y * size.x;
    return res;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
inline double dist(Point & p1, Point & p2) {
    return sqrt((long double)((p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y)));
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
void doWatershed(double * data, double * srcdata, Point & size, double mindist, int minradius, vector<TheObject> & objects) {
    int npts = size.x * size.y;
    /* I need to create a vector of indexes, which is sorted by the intensity of the corresponding distmap value */
    int mindata = 0;
    int nonzeros = -1;
    for (int i = 0; i < npts; i++) {
        data[i] = -ceil(data[i]);
        if (data[i] < BG) {
            nonzeros++;
            if (data[i] < mindata)
                mindata = (int)data[i];
        }
    }
    int * index = new int[nonzeros];
    int lastpt = -1;
    for (int i = 0; i < npts; i++) {
        if (data[i] < BG) {
            lastpt++;
            index[lastpt] = i;
        }
    }
/* DEBUG */
//cout << "Max " << mindata << " lastpt " << lastpt << endl;
    /* if value of the maximum point is smaller than the minradius - return */
    if (fabs(mindata) < minradius) return;
    Point pt, objcentre;
    bool seeded, edgypt; int objind, objind0, perimeterpt; double objdist, objdist0, val;
    for (int d = mindata; d < 0; d++) {
/* DEBUG */
//cout << "d " << d  << endl;
//cout << objects.size() << endl;
        int i = 0;
        while (i <= lastpt) {        
            if (data[index[i]] > d) {
                i++;
                continue;
            }
/* DEBUG */
//cout << "i " << i  << " of " << lastpt << endl;
//cout << objects.size() << endl;
            /* get coordinates of the current point */
            coordFromIndex(index[i], size, pt);
            /* check neighbours: if one or more negative - add the closest, if all non-negative - 
              check to start new seed - check if any other seed is close and 
              add to closest otherwise and check if object is larger than minsize before starting the object */
            seeded = false;
            perimeterpt = 0;
            edgypt = false;
            objind = -1;
            objdist = size.x + size.y;
//cout << "pass getting coordinates" << endl;
            for (int ix = pt.x - 1; ix <= pt.x + 1; ix += 2) {
                for (int iy = pt.y - 1; iy <= pt.y + 1; iy += 2) {
                    if (ix < 0 || ix >= size.x || iy < 0 || iy >= size.y) {
                        edgypt = true;
                        continue;
                    }
                    val = data[ix + iy * size.x];
                    if (val < 0) continue;
                    /* this can be a perimeter point and it can edge more than one obj/BG */
                    if (val == 0) {
                        perimeterpt++;
                        continue;
                    }
//cout << "hit " << val << endl;
                    objind0 = (int)val - 1;
                    if (objind0 == objind || objind0 >= objects.size()) continue;
                    objcentre = coordFromIndex(objects[objind0].index, size, objcentre);
                    objdist0 = dist(objcentre, pt);
                    /* must only be smaller to prevent returning to the same object */
                    if (objdist0 < objdist) {
                        objdist = objdist0;
                        /* if it is not the first seed - it is also a perimeter point */
                        if (objind >= 0) {
                            perimeterpt++;
                            /* add a perimeter point to the seed we do not consider any more */
                            objects[objind].perimeter++;
                        }
                        objind = objind0;
                        seeded = true;
                    }
                } /* iy */
            } /* ix */
//cout << "pass neighbours" << endl;
            if (!seeded) {
                /* try to check adding to the closest seed if dist < mindist */
                objind = -1;
                objdist  = mindist;
                for (int io = 0; io < objects.size(); io++) {
                    objcentre = coordFromIndex(objects[io].index, size, objcentre);
                    objdist0 = dist(objcentre, pt);
                    if (objdist0 < objdist) {
                        objdist = objdist0;
                        objind = io;
                        seeded = true;
                    }
                }        
            }
//cout << "pass !seeded vicinity check" << endl;
            if (seeded) {
//cout << "seeded" << endl;
//cout << "adding to seed " << objind << endl;
                /* add point to the seed */
                data[index[i]] = (double)(objind + 1);
                objects[objind].size++;
                if (edgypt) 
                    objects[objind].edge++;
                objects[objind].perimeter += perimeterpt;
                if (srcdata)
                    objects[objind].intensity += srcdata[index[i]];
                objects[objind].dx += pt.x - objcentre.x;
                objects[objind].dy += pt.y - objcentre.y;
            }
            else {
                if (fabs(data[index[i]]) >= minradius) {
//cout << "starting new" << endl;
                    /* start seed */
                    if (srcdata)
                        objects.push_back(TheObject(index[i], srcdata[index[i]]));
                    else
                        objects.push_back(TheObject(index[i], 0.0));
                    data[index[i]] = (double)objects.size();
                }
                else {
                    /* disregard */
//cout << "disregard" << endl;
                    data[index[i]] = BG;
                }
            }
            /* move last value and -- last point */
            if (i < lastpt) {
//cout << "move last pt" << endl;
                index[i] = index[lastpt];
                lastpt--;        
            }
        } // i 
    } // d
    delete[] index;
} 
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
void stopIfAnyWrong(SEXP rimage, SEXP srcimage) {
    if (!assertImage(rimage))
        error("Wrong argument class, Image expected");
    if (LOGICAL(GET_SLOT(rimage, mkString("rgb")))[0])
        error("Algorithm works for grayscale images only");
    if (srcimage != R_NilValue) {
        if (!assertImage(srcimage))
            error("Wrong class of srcimage, Image or NULL expected");
        if (LOGICAL(GET_SLOT(srcimage, mkString("rgb")))[0])
            error("Algorithm works for grayscale source images only");
    }
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    int index;         
    double intensity;  
    int size;          
    int edge;          
    int perimeter;     
    double dx;         
    double dy;
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP watershedDetection(SEXP rimage, SEXP srcimage, SEXP seeds, SEXP params) {
    stopIfAnyWrong(rimage, srcimage);
    SEXP res = R_NilValue;
    try {
        double * data = &(REAL(rimage)[0]);
        double * srcdata = NULL;
        if (srcimage != R_NilValue)
            srcdata = &(REAL(srcimage)[0]);;
        int * dim = INTEGER(GET_DIM(rimage));
        Point size(dim[0], dim[1]);
        double mindist = REAL(params)[0];
        double minradius = REAL(params)[1];
        vector<TheObject> objects;
        doWatershed(data, srcdata, size, mindist, (int)minradius, objects);
        int nobj = objects.size();
        if (nobj > 0) {
            Point pt;
            SEXP dims;
            PROTECT(dims = allocVector(INTSXP, 2));
            INTEGER(dims)[0] = nobj;
            INTEGER(dims)[1] = 9;
            PROTECT(res = allocVector(REALSXP, nobj * 9));
            SET_DIM(res, dims);
            for (int i = 0; i < nobj; i++) {
                REAL(res)[i]            = 0; /* index */
                coordFromIndex(objects[i].index, size, pt);
                REAL(res)[i +     nobj] = pt.x;
                REAL(res)[i + 2 * nobj] = pt.y;
                REAL(res)[i + 3 * nobj] = objects[i].intensity;
                REAL(res)[i + 4 * nobj] = objects[i].size;
                REAL(res)[i + 5 * nobj] = objects[i].perimeter;
                REAL(res)[i + 6 * nobj] = objects[i].edge;
                REAL(res)[i + 7 * nobj] = objects[i].dx;
                REAL(res)[i + 8 * nobj] = objects[i].dy;
            }
            UNPROTECT(2);
        }
    }
    catch(exception &error_) {
        error(error_.what());
    }
    return res;
}
