#include "watershed.h"

#include <R_ext/Error.h>

#include <vector>
#include <iostream>
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
const double BG = 0;
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
Point getpoint(const int & index, const int & xsize) {
    Point res(0, index / xsize);
    res.x = index - res.y * xsize;
    return res;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
int getindex(const Point & pt, const int xsize) {
    return pt.x + pt.y * xsize;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
int getindex(const int & x, const int & y, const int xsize) {
    return x + y * xsize;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
double dist(const Point & p1, const Point & p2) {
    return sqrt((long double)((p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y)));
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
class TheFeature {
    private:
        int _ind;
        Point _centre;
    public:
        TheFeature() {
            _ind = -1;
            ok = true;
        };
        Point centre() {
            int ps = pixels.size();
            if (ps > _ind + 1)
                for (int i = _ind + 1; i < ps; i++) {
                    _centre.x += pixels[i].x;
                    _centre.y += pixels[i].y;
                }
            _ind = ps - 1;
            if (ps > 0)
                return(Point(_centre.x / ps, _centre.y / ps));
            else
                return(_centre);
        }
        vector<Point> pixels;
        vector<Point> borders;
        vector<Point> edges;
        bool          ok;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
void doWatershed(double *, Point &, double, double, vector<TheFeature> &, double);
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP watershedDetection(SEXP rimage, SEXP ref, SEXP seeds, SEXP params) {
    /* !!! use nprotect++ everywhere you use PROTECT, cleanup automatic */
    if (!assertImage(rimage))
        error("Wrong argument class, Image expected");
    if (LOGICAL(GET_SLOT(rimage, mkString("rgb")))[0])
        error("Algorithm works for grayscale images only");
    SEXP res = R_NilValue;
    SEXP * items;
    SEXP * names;
    SEXP * objMtx;
    SEXP * objDim;
    SEXP * pxsMtx;
    SEXP * pxsDim;
    SEXP * brdMtx;
    SEXP * brdDim;
    int nprotect = 0;
    try {
        /* get sizes of images */
        Point size(INTEGER(GET_DIM(rimage))[0], INTEGER(GET_DIM(rimage))[1]);
        int nimages = INTEGER(GET_DIM(rimage))[2];
        if (nimages <= 0)
            error("no images supplied");
        /* get parameters */
        double mindist = REAL(params)[0];
        double minradius = REAL(params)[1];
        double edgeFactor = REAL(params)[2];
        /* result will be a list if more than 1 image, allocate it here */
        if (nimages > 1) {
            PROTECT(res = allocVector(VECSXP, nimages));
            nprotect++;
        }
        /* precreate arrays for all possible R variables that stay */
        items  = new SEXP[nimages];
        names  = new SEXP[nimages];
        objMtx = new SEXP[nimages];
        objDim = new SEXP[nimages];
        pxsMtx = new SEXP[nimages];
        pxsDim = new SEXP[nimages];
        brdMtx = new SEXP[nimages];
        brdDim = new SEXP[nimages];
        /* main loop through images */
        for (int i = 0; i < nimages; i++) {
            /* set result for this image to NULL in case we find no objects */
            items[i] = R_NilValue;
            /* vector of all objects */
            vector<TheFeature> objects;
            /* fill it with seeds if given */
            if (seeds != R_NilValue) {
                SEXP iseeds;
                if (nimages > 1)
                    iseeds = VECTOR_ELT(seeds, i);
                else
                    iseeds = seeds;
                double * pts = REAL(iseeds);
                int npts = LENGTH(iseeds) / 2;
                for (int j = 0; j < npts; j++) {
                    objects.push_back(TheFeature());
                    objects[j].pixels.push_back(Point((int)pts[j], (int)pts[j + npts]));
                }
            }
            /* get pointer to image data for ith image */
            double * data = &(REAL(rimage)[i * size.x * size.y]);
            double * refdata = NULL;
            if (ref != R_NilValue)
                refdata = &(REAL(ref)[i * size.x * size.y]);
            /* do watershed on 1 ith image, result in objects */
            doWatershed(data, size, mindist, minradius, objects, edgeFactor);
            int nobj = objects.size();
            /* return NULL for this image if no objects detected */
            if (nobj == 0) continue;
            /* values that will keep maximum numbers of pixels and border pixels in objects */
            unsigned int maxpxs = 0;
            unsigned int maxbrd = 0;
            /* create object matrix: INTEGER */
            PROTECT(objDim[i] = allocVector(INTSXP, 2));
            nprotect++;
            INTEGER(objDim[i])[0] = nobj;
            INTEGER(objDim[i])[1] = 6;
            PROTECT(objMtx[i] = allocVector(REALSXP, INTEGER(objDim[i])[0] * INTEGER(objDim[i])[1]));
            nprotect++;
            SET_DIM(objMtx[i], objDim[i]);
            double * val = &(REAL(objMtx[i])[0]);
            /* put values into the object matrix and update maxs */
            for (int j = 0; j < nobj; j++) {
                Point ci = objects[j].centre();                
                val[j           ] = ci.x;
                val[j +     nobj] = ci.y;
                val[j + 2 * nobj] = objects[j].pixels.size();
                if (objects[j].pixels.size() > maxpxs)
                    maxpxs = objects[j].pixels.size();
                double intens = 0;
                if (refdata)
                    for (unsigned int k = 0; k < objects[j].pixels.size(); k++)
                        intens += refdata[getindex(objects[j].pixels[k], size.x)];
                val[j + 3 * nobj] = intens;
                val[j + 4 * nobj] = objects[j].borders.size();
                if (objects[j].borders.size() > maxbrd)
                    maxbrd = objects[j].borders.size();
                val[j + 5 * nobj] = objects[j].edges.size();
            }
            /* create matrix of pixel indexes with max size */
            PROTECT(pxsDim[i] = allocVector(INTSXP, 2));
            nprotect++;
            INTEGER(pxsDim[i])[0] = nobj;
            INTEGER(pxsDim[i])[1] = maxpxs;
            PROTECT(pxsMtx[i] = allocVector(INTSXP, INTEGER(pxsDim[i])[0] * INTEGER(pxsDim[i])[1]));
            nprotect++;
            SET_DIM(pxsMtx[i], pxsDim[i]);
            int * pixels = &(INTEGER(pxsMtx[i])[0]);
            /* create matrix of border indexes with max size */
            PROTECT(brdDim[i] = allocVector(INTSXP, 2));
            nprotect++;
            INTEGER(brdDim[i])[0] = nobj;
            INTEGER(brdDim[i])[1] = maxbrd;
            PROTECT(brdMtx[i] = allocVector(INTSXP, INTEGER(brdDim[i])[0] * INTEGER(brdDim[i])[1]));
            nprotect++;
            SET_DIM(brdMtx[i], brdDim[i]);
            int * borders = &(INTEGER(brdMtx[i])[0]);
            /* put values of pixels and borders, rest fill with NA */
            for (int j = 0; j < nobj; j++) {
                unsigned int k;
                for (k = 0; k < objects[j].pixels.size(); k++)
                    pixels[j + k * nobj] = getindex(objects[j].pixels[k], size.x);
                for (k = objects[j].pixels.size(); k < maxpxs; k++)
                    pixels[j + k * nobj] = NA_INTEGER;
                for (k = 0; k < objects[j].borders.size(); k++)
                    borders[j + k * nobj] = getindex(objects[j].borders[k], size.x);
                for (k = objects[j].borders.size(); k < maxbrd; k++)
                    borders[j + k * nobj] = NA_INTEGER;
            }
            /* create a list of objects, pixels and borders */
            PROTECT(items[i] = allocVector(VECSXP, 3));
            nprotect++;
            SET_VECTOR_ELT(items[i], 0, objMtx[i]);
            SET_VECTOR_ELT(items[i], 1, pxsMtx[i]);
            SET_VECTOR_ELT(items[i], 2, brdMtx[i]);
            /* name this list */
            PROTECT(names[i] = allocVector(STRSXP, 3));
            nprotect++;
            SET_STRING_ELT(names[i], 0, mkChar("objects"));
            SET_STRING_ELT(names[i], 1, mkChar("pixels"));
            SET_STRING_ELT(names[i], 2, mkChar("borders"));
            SET_NAMES(items[i], names[i]);
        }
        /* if more than one image, add all results to the list, otherwise, output first */
        if (nimages > 1)
            for (int i = 0; i < nimages; i++)
                SET_VECTOR_ELT(res, i, items[i]);
        else
            res = items[0];
        /* clenup */
        delete[] items;
        delete[] objMtx;
        delete[] objDim;
        delete[] pxsMtx;
        delete[] pxsDim;
        delete[] brdMtx;
        delete[] brdDim;
    }
    catch(exception &error_) {
        error(error_.what());
    }
    UNPROTECT(nprotect);  
    return res;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
SEXP paintWatershed(SEXP x, SEXP img, SEXP cols, SEXP dofill, SEXP doborders, SEXP opacity) {
    try {
        int nimages = INTEGER(GET_DIM(img))[2];
        if (nimages <= 0)
            error("no images supplied");
        if (nimages > 1 && LENGTH(x) != nimages)
            error("list x must have the same number of elements as the number of images");
        Point size(INTEGER(GET_DIM(img))[0], INTEGER(GET_DIM(img))[1]);
        bool fill = LOGICAL(dofill);
        bool outline = LOGICAL(doborders);
        double ropac = REAL(opacity)[0];
        for (int i = 0; i < nimages; i++) {
            SEXP xx;
            if (nimages > 1)
                xx = VECTOR_ELT(x, i);
            else
                xx = x;
            if (xx == R_NilValue) continue;
            if (VECTOR_ELT(cols, i) == R_NilValue) continue;
            int * col = INTEGER(VECTOR_ELT(cols, i));
            int nobj = LENGTH(VECTOR_ELT(cols, i));
            int * imgdata = &(INTEGER(img)[i * size.x * size.y]);
            if (VECTOR_ELT(xx, 1) != R_NilValue && fill) {
                MagickImage dots(nobj, 1, "RGBp", CharPixel, col);
                for (int j = 0; j < nobj; j++) {
                    ColorRGB cc = dots.pixelColor(j, 0);
                    cc.red(cc.red() * ropac);
                    cc.green(cc.green() * ropac);
                    cc.blue(cc.blue() * ropac);
                    dots.pixelColor(j, 0, cc);
                }
                dots.opacity(OpaqueOpacity);
                dots.type(TrueColorType);
                int * newcol = new int[nobj];
                dots.write(0, 0, nobj, 1, "RGBp", CharPixel, newcol);
                int * pixels = INTEGER(VECTOR_ELT(xx, 1));
                int npxs = INTEGER(GET_DIM(VECTOR_ELT(xx, 1)))[1];
                int index;
                for (int j = 0; j < nobj; j++) {
                    
                    for (int k = 0; k < npxs; k++)
                        if ((index = pixels[j + k * nobj]) != NA_INTEGER) 
                            imgdata[index] += newcol[j];
                }
                delete[] newcol;
            }
            if (VECTOR_ELT(xx, 2) != R_NilValue && outline) {
                int * borders = INTEGER(VECTOR_ELT(xx, 2));
                int nbrd = INTEGER(GET_DIM(VECTOR_ELT(xx, 2)))[1];
                int index;
                for (int j = 0; j < nobj; j++) {
                    for (int k = 0; k < nbrd; k++)
                        if ((index = borders[j + k * nobj]) != NA_INTEGER) 
                            imgdata[index] = col[j];
                }
            }
        }
    }
    catch(exception &error_) {
        error(error_.what());
    }
    return img;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
 runs watershed detection algorithm for the data of a single image,
 returns results in objects
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
void doWatershed(double * data, Point & size, double mindist, double minradius, vector<TheFeature> & objects, double edgeFactor) {
    unsigned i, j, k; // used too often below, thus preallocated
    unsigned int npts = size.x * size.y;
    /* if we supply objects with seeds, we do not add any new and just use those */
    bool noNewObjects = false;
    if (objects.size() > 0) noNewObjects = true;
    /* list of all non-zero pixels */
    vector<Point> pxs;
    /* DistMap will be negated (-1*) and this is its minimum value then */
    int mindata = 0;
    /* negate data: BG will be 0, positive will be index of objects */
    /* this conversion to int is needed to ensure that when we later on go by incrememting d by 1 we 
       do not miss values between 0 and 1, i,e, we set 0 = 0, 0.x = 1 */
    for (i = 0; i < npts; i++) {
        data[i] = -ceil(data[i]);
        if (data[i] < BG) {
            pxs.push_back(getpoint(i, size.x));
            if (data[i] < mindata)
                mindata = (int)data[i];
        }
    }
    /* if value of the maximum object is smaller than the minradius - return */
    if (abs(mindata) < minradius) return;
    /* main loop through different distmap levels, i.e. discretised colours */
    /* note: d's are negative */
    for (int d = mindata; d < 0; d++) {
        /* help variables for loops t ospeed them up, c-style, but fast */
/*        Point pti, objcentre; 
        bool seeded, edgypt; double objdist, objdist0, val;
        int ix, iy, objind, perimeterpt; 
        unsigned int objind0, io;
*/
        /* main sub-loop through indexes that left */
        for (i = 0; i < pxs.size(); ) {
            Point pti = pxs[i];
            /* go to next index if this color is farther in the row */
            int iindex = getindex(pti, size.x);
            if (data[iindex] > d) {
                i++;
                continue;
            }
            /* check neighbours: seeded the closest of neighbouring obejcts */
            int    seeded     = -1;
            bool   edgy       = false;
            bool   border     = false;
            double seededdist = size.x + size.y; /* if seeded, distance to the object */
            /* check 8 points  */
            for (int ix = pti.x - 1; ix <= pti.x + 1; ix++) {
                for (int iy = pti.y - 1; iy <= pti.y + 1; iy++) {
                    /* do not do anything for the point itself */
                    if (ix == pti.x && iy == pti.y) continue;
                    /* set as edgy if any neighbour out of image */
                    if (ix < 0 || ix >= size.x || iy < 0 || iy >= size.y) {
                        edgy = true;
                        continue;
                    }
                    /* get value for the neighbour */
                    double val = data[ix + iy * size.x];
                    /* neighbour is a normal point of distmap - not BG and not object, do nothing */
                    if (val < 0) continue;
                    /* increase perimeter if neighbour is BG and do nothing else */
                    if (val == 0) {
                        border = true;
                        continue;
                    }
                    /* so neightbor is object - get its index */
                    int objind = (int)val - 1;
                    /* check if it is existing object and not already defined from other neighbour */
                    if (objind == seeded || objind >= (int)objects.size()) continue;
                    /* check if this object is closer than other detected, or just update */
                    double objdist = dist(objects[objind].centre(), pti);
                    /* so we like this object - it is closer */
                    if (objdist < seededdist) {
                        seededdist = objdist;
                        /* if it is not the first object - it is also a perimeter point */
                        /* TODO: not sure if to count touching points as perimeter
                        if (objind >= 0) {
                            border = true;
                            // add a border point to the object we do not consider any more 
                            objects[seeded].borders.push_back(pti);
                        }
                        */
                        seeded = objind;
                    }
                } /* iy */
            } /* ix */
            /* it is not neighbouring any object, but maube it is close enough anyway 
               THE ABOVE IS much FASTER, therefore this is only run if the above does not
               show anything */
            if (seeded < 0) {
                // we only consider objects closer than mindist
                seededdist  = mindist;
                for (j = 0; j < objects.size(); j++) {
                    double objdist = dist(objects[j].centre(), pti);
                    if (objdist < seededdist) {
                        seededdist = objdist;
                        seeded = j;
                    }
                }      
            }
            if (seeded < 0) {
                /* start a new object if its radius (determined by the highest dm value) is larger than minradius */
                /* and if we are allowed to create new seeds */
                if (fabs(data[iindex]) >= minradius && !noNewObjects) {
                    objects.push_back(TheFeature());
                    seeded = objects.size() - 1;
                }
                else {
                    /* otherwise disregard */
                    data[iindex] = BG;
                }
            }
            /* no else, because value can change within */
            if (seeded >= 0) {
                /* mark image with the index, add point to the object */
                data[iindex] = (double)(seeded + 1);
                objects[seeded].pixels.push_back(pti);
                if (border)
                    objects[seeded].borders.push_back(pti);
                if (edgy)
                    objects[seeded].edges.push_back(pti);
            }
            /* puts last element instead of this, removes last and keeps i  */
            pxs[i] = pxs.back();
            pxs.pop_back();
        } // i 
    } // d
    if (objects.size() < 1) return;
    /* mark small and edgy objects */ 
    for (i = 0; i < objects.size(); i++) {
        if (objects[i].borders.size() == 0) {
            objects[i].ok = false;
            continue;
        }
        if (objects[i].edges.size() / (double)objects[i].borders.size() > edgeFactor) {
            objects[i].ok = false;
            continue;
        }
        if (objects[i].pixels.size() < M_PI * minradius * minradius)
            objects[i].ok = false;
    }
    bool combiFound;
    /* iterate through the objects until no combinations can be found, combine small objects */
    do {
        combiFound = false;
        for (i = 0; i < objects.size() && !combiFound; i++) {
            /* do not combine into this object, it is bad, combine vice versa if possible */
            if (!objects[i].ok) continue;
            Point ci = objects[i].centre();
            for (j = 0; j < objects.size() && !combiFound; j++) {
                if (i == j) continue;
                Point cj = objects[j].centre();
                if (dist(ci, cj) <= mindist) {
                    /* combine j into i and delete j */
                    combiFound = true;
                    objects[j].ok = false;
                    for (k = 0; k < objects[j].pixels.size(); k++)
                        objects[i].pixels.push_back(objects[j].pixels[k]);
                    for (k = 0; k < objects[j].borders.size(); k++)
                        objects[i].borders.push_back(objects[j].borders[k]);
                    for (k = 0; k < objects[j].edges.size(); k++)
                        objects[i].edges.push_back(objects[j].edges[k]);
                    /* substitute objects[j] with last and delete last */
                    objects[j] = objects.back();
                    objects.pop_back();
                    /* all for loops must stop because objects are messed up - need for combiFound */
                    break;
                }
            }
        }
    } while (combiFound);
    /* delete bad objects */
    for (i = 0; i < objects.size();) {
        if (objects[i].ok) {
            i++;
            continue;
        }
        objects[i] = objects.back();
        objects.pop_back();
    }
    // result is returned in objects 
} 
