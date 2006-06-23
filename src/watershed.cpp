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
void doWatershed(double *, double *, Point &, double, int, vector<TheObject> &);
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
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
void doWatershed(double * data, double * srcdata, Point & size, double mindist, int minradius, vector<TheObject> & objects) {
    int npts = size.x * size.y;
    /* DistMap will be negated (-1*) and this is its minimum value then */
    int mindata = 0;
    /* how many nonBG pixels we have */
    int nonBG = -1;
    /* negate data converting to integer, determine nonBG and midata */
    /* integer conversion basically means segmenting image to discrete colors, the number of these
       colors is determined by the maximum value on the distmap, i.e. by the size of the largest object */
    for (int i = 0; i < npts; i++) {
        data[i] = -ceil(data[i]);
        if (data[i] < BG) {
            nonBG++;
            if (data[i] < mindata)
                mindata = (int)data[i];
        }
    }
    /* create an array of indeces of all nonBG pixels (could combine with the previous
       loop, but then array size equals npts, which is too huge (reduce mem usage) */
    int * index = new int[nonBG];
    int lastpt = -1;
    for (int i = 0; i < npts; i++) 
        if (data[i] < BG) {
            lastpt++;
            index[lastpt] = i;
        }
/* DEBUG */
//cout << "Max " << mindata << " lastpt " << lastpt << endl;
    /* if value of the maximum object is smaller than the minradius - return */
    if (fabs(mindata) < minradius) return;
    /* help variables for loops t ospeed them up, c-style, but fast */
    Point pt, objcentre; 
    bool seeded, edgypt; double objdist, objdist0, val;
    int i, d, ix, iy, io, objind, objind0, perimeterpt; 
    /* main loop through different distmap levels, i.e. discretised colours */
    /* note: d's are negative */
    for (d = mindata; d < 0; d++) {
/* DEBUG */
//cout << "d " << d  << endl;
//cout << objects.size() << endl;
        /* main sub-loop through indexes that left */
        i = 0;
        while (i <= lastpt) {        
            /* go to next index if this color is farther in the row */
            if (data[index[i]] > d) {
                i++;
                continue;
            }
/* DEBUG */
//cout << "i " << i  << " of " << lastpt << endl;
//cout << objects.size() << endl;
            /* get coordinates of the current point */
            coordFromIndex(index[i], size, pt);
            /* check neighbours: seeded the closest of neighbouring obejcts */
            seeded = false;
            perimeterpt = 0; /* to how many other obejcts is this point a neighbour, if any */
            edgypt = false;  /* is this point on the image edge */
            objind = -1;     /* if seeded, index of object */
            objdist = size.x + size.y; /* if seeded, distance to the object */
//cout << "pass getting coordinates" << endl;
            /* check 8 points  */
            for (ix = pt.x - 1; ix <= pt.x + 1; ix++) {
                for (iy = pt.y - 1; iy <= pt.y + 1; iy++) {
                    /* do not do anything for the point itself */
                    if (ix == pt.x && iy == pt.y) continue;
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
//cout << "hit " << val << endl;
                    /* so neightbor is object - get its index */
                    objind0 = (int)val - 1;
                    /* check if it is existing object and not already defined from other neighbour */
                    if (objind0 == objind || objind0 >= objects.size()) continue;
                    /* check if this object is closer than other detected, or just update */
                    objdist0 = dist(coordFromIndex(objects[objind0].index, size, objcentre), pt);
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
//cout << "pass neighbours" << endl;
            /* it is not neighbouring any object, but maube it is close enough anyway 
               THE ABOVE IS much FASTER, therefore this is only run if the above does not
               show anything */
            if (!seeded) {
                objind = -1;
                /* we only consider objects closer than mindist */
                objdist  = mindist;
                for (io = 0; io < objects.size(); io++) {
                    objdist0 = dist(coordFromIndex(objects[io].index, size, objcentre), pt);
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
                /* mark image with the index */
                data[index[i]] = (double)(objind + 1);
                objects[objind].size++;
                if (edgypt) 
                    objects[objind].edge++;
                objects[objind].perimeter += perimeterpt;
                if (srcdata)
                    objects[objind].intensity += srcdata[index[i]];
                /* dynamically update centre, use dx, dy for temporary summation */
                objects[objind].dx += pt.x;
                objects[objind].dy += pt.y;
                objects[objind].index = (int)(objects[objind].dx / objects[objind].size) + (int)(objects[objind].dy / objects[objind].size) * size.x;
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
                    /* dynamically update centre, use dx, dy for temporary summation */
                    objects[objects.size() - 1].dx = pt.x;
                    objects[objects.size() - 1].dy = pt.y;
                }
                else {
                    /* disregard */
//cout << "disregard" << endl;
                    data[index[i]] = BG;
                }
            }
            /* replace this with last point - effectively index will be sorted acsending! */
            if (i < lastpt) {
//cout << "move last pt" << endl;
                objind = index[i];
                index[i] = index[lastpt];
                index[lastpt] = objind;
                lastpt--;        
            }
        } // i 
    } // d
    /* now we have all objects - let's get their shapes */
    /* we will use its sorted state to speed up calculations */
    objind = -1;
    for (i = 0; i < nonBG; i++) {
        val = data[index[i]];
        /* just o be sure that we do not do smth wrong */
        if (val <= 0) continue;
        coordFromIndex(index[i], size, pt);
        if (val != objind) {
            /* we change objind and considering data are sorted - reset the dx, dy */
            objind = (int)val;
            coordFromIndex(objects[objind].index, size, objcentre);
            objects[objind].dx = pt.x - objcentre.x;
            objects[objind].dy = pt.y - objcentre.y;
        }
        else {
            objects[objind].dx += pt.x - objcentre.x;
            objects[objind].dy += pt.y - objcentre.y;
        }
    }    
    delete[] index;
} 
