#include "watershed.h"
#include "conversions.h"
#include <R_ext/Error.h>
#include <vector>
/* #include <algorithm> */
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
        x = 0;
        y = 0;
        rm = false;
    }
    int index;          /* index of the object origin on the image */
    double intensity;   /* overall intensity */
    int size;           /* size of the object */
    int perimeter;      /* number of perimeter points */
    int edge;           /* number of edge points */
    double x;          /* summ of all x coordinates */
    double y;          /* summ of all y coordinates */
    bool  rm;          /* if the object has to be removed due to any reason: edgy, combined with smth etc */
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
void doWatershed(double *, double *, Point &, double, int, vector<TheObject> &, double);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP watershedDetection(SEXP rimage, SEXP srcimage, SEXP seeds, SEXP params) {
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
        double edgeFactor = REAL(params)[2];
        vector<TheObject> objects;
        if (seeds != R_NilValue) {
            /* TODO fill in objects */        
        
        }
        doWatershed(data, srcdata, size, mindist, (int)minradius, objects, edgeFactor);
        int nobj = objects.size();
        if (nobj > 0) {
            Point pt;
            SEXP dims;
            PROTECT(dims = allocVector(INTSXP, 2));
            INTEGER(dims)[0] = nobj;
            INTEGER(dims)[1] = 8; //9;
            PROTECT(res = allocVector(REALSXP, nobj * 8)); //9));
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
                REAL(res)[i + 7 * nobj] = (double)objects[i].rm;
            /* we do not need these two at the moment - do not work 
                REAL(res)[i + 7 * nobj] = objects[i].x;
                REAL(res)[i + 8 * nobj] = objects[i].y;
            */
            }
            UNPROTECT(2);
        }
    }
    catch(exception &error_) {
        error(error_.what());
    }
    return res;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
void doWatershed(double * data, double * srcdata, Point & size, double mindist, int minradius, vector<TheObject> & objects, double edgeFactor) {
    int npts = size.x * size.y;
    /* if we supply objects with seeds, we do not add any new and just use those */
    bool noNewObjects = false;
    if (objects.size() > 0) noNewObjects = true;
    /* DistMap will be negated (-1*) and this is its minimum value then */
    int mindata = 0;
    /* how many nonBG pixels we have */
    int nonBG = 0;
    /* negate data: BG will be 0, positive will be index of objects */
    /* this conversion to int is needed to ensure that when we later on go by incrememting d by 1 we 
       do not miss values between 0 and 1, i,e, we set 0 = 0, 0.x = 1 */
    for (int i = 0; i < npts; i++) {
        data[i] = -ceil(data[i]);
        if (data[i] < BG) {
            nonBG++;
            if (data[i] < mindata)
                mindata = (int)data[i];
        }
    }
    /* create an array of indexes of all nonBG pixels (could combine with the previous
       loop, but then array size would equal npts, which is too huge (reduce mem usage) */
    int * index = new int[nonBG];
    int lastpt = -1;
    for (int i = 0; i < npts; i++) 
        if (data[i] < BG) {
            lastpt++;
            index[lastpt] = i;
        }
    /* if value of the maximum object is smaller than the minradius - return */
    if (abs(mindata) < minradius) return;
    vector<int> borders;
    /* main loop through different distmap levels, i.e. discretised colours */
    /* note: d's are negative */
    for (int d = mindata; d < 0; d++) {
        /* help variables for loops t ospeed them up, c-style, but fast */
        Point pti, objcentre; 
        bool seeded, edgypt; double objdist, objdist0, val;
        int ix, iy, objind, perimeterpt; 
        unsigned int objind0, io;
        /* main sub-loop through indexes that left */
        int i = 0;
        while (i <= lastpt) {        
            /* go to next index if this color is farther in the row */
            if (data[index[i]] > d) {
                i++;
                continue;
            }
            /* get coordinates of the current point */
            coordFromIndex(index[i], size, pti);
            /* check neighbours: seeded the closest of neighbouring obejcts */
            seeded = false;
            perimeterpt = 0; /* to how many other obejcts is this point a neighbour, if any */
            edgypt = false;  /* is this point on the image edge */
            objind = -1;     /* if seeded, index of object */
            objdist = size.x + size.y; /* if seeded, distance to the object */
            /* check 8 points  */
            for (ix = pti.x - 1; ix <= pti.x + 1; ix++) {
                for (iy = pti.y - 1; iy <= pti.y + 1; iy++) {
                    /* do not do anything for the point itself */
                    if (ix == pti.x && iy == pti.y) continue;
                    /* set as edgy if any neighbour out of image */
                    if (ix < 0 || ix >= size.x || iy < 0 || iy >= size.y) {
                        edgypt = true;
                        continue;
                    }
                    /* get value for the neighbour */
                    val = data[ix + iy * size.x];
                    /* neighbour is a normal point of distmap - not BG and not object, do nothing */
                    if (val < 0) continue;
                    /* increase perimeter if neighbour is BG and do nothing else */
                    if (val == 0) {
                        perimeterpt++;
                        continue;
                    }
                    /* so neightbor is object - get its index */
                    objind0 = (int)val - 1;
                    /* check if it is existing object and not already defined from other neighbour */
                    if (objind0 == objind || objind0 >= objects.size()) continue;
                    /* check if this object is closer than other detected, or just update */
                    objdist0 = dist(coordFromIndex(objects[objind0].index, size, objcentre), pti);
                    /* so we like this object - it is closer */
                    if (objdist0 < objdist) {
                        objdist = objdist0;
                        /* if it is not the first object - it is also a perimeter point */
                        if (objind >= 0) {
                            perimeterpt++;
                            /* add a perimeter point to the object we do not consider any more */
                            objects[objind].perimeter++;
                        }
                        objind = objind0;
                        seeded = true;
                    }
                } /* iy */
            } /* ix */
            /* add this pixel to the list of all borders */
            if (perimeterpt > 0)
                borders.push_back(index[i]);
            /* it is not neighbouring any object, but maube it is close enough anyway 
               THE ABOVE IS much FASTER, therefore this is only run if the above does not
               show anything */
            if (!seeded) {
                objind = -1;
                // we only consider objects closer than mindist
                objdist  = mindist;
                for (io = 0; io < objects.size(); io++) {
                    objdist0 = dist(coordFromIndex(objects[io].index, size, objcentre), pti);
                    if (objdist0 < objdist) {
                        objdist = objdist0;
                        objind = io;
                        seeded = true;
                    }
                }        
            }
            if (seeded) {
                /* add point to the object, mark image with the index */
                data[index[i]] = (double)(objind + 1);
                objects[objind].size++;
                if (edgypt) 
                    objects[objind].edge++;
                objects[objind].perimeter += perimeterpt;
                if (srcdata)
                    objects[objind].intensity += srcdata[index[i]];
                /* dynamically update centre (its index), use x, y for temporary summation */
                objects[objind].x += pti.x;
                objects[objind].y += pti.y;
                objects[objind].index = (int)(objects[objind].x / objects[objind].size) + (int)(objects[objind].y / objects[objind].size) * size.x;
            }
            else {
                /* start a new object if its radius (determined by the highest dm value) is larger than minradius */
                /* and if we are allowed to create new seeds */
                if (fabs(data[index[i]]) >= minradius && !noNewObjects) {
                    /* start seed */
                    if (srcdata)
                        objects.push_back(TheObject(index[i], srcdata[index[i]]));
                    else
                        objects.push_back(TheObject(index[i], 0.0));
                    data[index[i]] = (double)objects.size();
                    /* dynamically update centre, use x, y for temporary summation */
                    objects[objects.size() - 1].x = pti.x;
                    objects[objects.size() - 1].y = pti.y;
                }
                else {
                    /* otherwise disregard */
                    data[index[i]] = BG;
                }
            }
            /* replace this with last point  */
            if (i < lastpt) {
                objind = index[i];
                index[i] = index[lastpt];
                index[lastpt] = objind;
            }
            lastpt--;        
        } // i 
    } // d
    unsigned int nobj = objects.size();
    if (nobj > 1) {
        /* try to combine objects that are close */
        int * combineWith = new int[nobj];
        /* mark edgy and small objects for deletion, they can still be combined with other */
        for (int i = 0; i < nobj; i++) {
            combineWith[i] = -1;
            if (objects[i].perimeter == 0) {
                objects[i].rm = true;
                continue;
            }
            if (objects[i].edge / (double)objects[i].perimeter > edgeFactor) {
                objects[i].rm = true;
                continue;
            }
            if (objects[i].size < M_PI * minradius * minradius)
                objects[i].rm = true;
        }
        Point pti, ptj;
        for (int i = 0; i < nobj - 1; i++) {
            /* we do not want other objects to be combined with this */
            if (objects[i].rm) continue;
            coordFromIndex(objects[i].index, size, pti);
            for (int j = i + 1; j < nobj; j++) {
                /* skip objects that will already be combined with other */
                if (combineWith[j] < 0) 
                    if (dist(coordFromIndex(objects[j].index, size, ptj), pti) <= mindist) {
                        objects[j].rm = true; // it will be combined or removed anyway
                        /* new value for combineWith */
                        int k = combineWith[i];
                        if (k < 0) k = i;
                        /* but only if this new object is not gonna be removed */
                        double fullperimeter = objects[j].perimeter + objects[k].perimeter;
                        int fulledge = objects[j].edge + objects[k].edge;
                        if (fullperimeter != 0)
                            if (fulledge / fullperimeter > edgeFactor) 
                                continue;
                        /* do combine objects */
                        combineWith[j] = k;
                        objects[k].x += objects[j].x;
                        objects[k].y += objects[j].y;
                        objects[k].size += objects[j].size;
                        objects[k].index = (int)(objects[k].x / objects[k].size) + (int)(objects[k].y / objects[k].size) * size.x;
                        objects[k].intensity += objects[j].intensity;
                        objects[k].perimeter = (int)fullperimeter;
                        objects[k].edge = fulledge;
                    } // if (dist
            } // j
        } // i
        /* mark image */
        double val;
        for (int i = 0; i < nonBG; i++) {
            val = data[index[i]];
            if (val <= BG || val > nobj) {
                if (val != BG) data[index[i]] = BG;
                continue;
            }
            if (combineWith[(int)val - 1] >= 0) {
                data[index[i]] = (double)combineWith[(int)val - 1];
            }
            else {
                if (objects[(int)val - 1].rm)
                    data[index[i]] = BG;
            }
        }
        delete[] combineWith;
        /* mark all borders with highest index + 1 */
        int nborders = borders.size();
        for (int i = 0; i < nborders; i++) {
            int ind = borders[i];
            /* do not mark if object was selected for removal */
            if (data[ind] > BG)
                data[ind] = nobj + 1;
        }
    }
    delete[] index;
    // result is returned in objects 
} 
