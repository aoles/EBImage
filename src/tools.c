#include "tools.h"

/* -------------------------------------------------------------------------
Package tools
Copyright (c) 2006 Oleg Sklyar
See: ../LICENSE for license, LGPL
------------------------------------------------------------------------- */

int GTK_OK = 0;

/*----------------------------------------------------------------------- */
double
distanceXY (const PointXY pt1, const PointXY pt2) {
    return sqrt ( (long double)( (pt1.x - pt2.x) * (pt1.x - pt2.x) + (pt1.y - pt2.y) * (pt1.y - pt2.y) ) );
}

/*----------------------------------------------------------------------- */
double
distancexy (int x1, int y1, int x2, int y2) {
    return sqrt ( (long double)( (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2) ) );
}

/*----------------------------------------------------------------------- */
PointXY
pointFromIndex (const int index, const int xsize) {
    PointXY res;
    res.y = floor (index / xsize);
    res.x = index - res.y * xsize;
    return res;
}

/*----------------------------------------------------------------------- */
int
indexFromPoint (const PointXY pt, const int xsize) {
    return (int)( fabs(pt.x + pt.y * xsize) );
}

/*----------------------------------------------------------------------- */
int
indexFromXY (const int x, const int y, const int xsize) {
    return x + y * xsize;
}

/*----------------------------------------------------------------------- */
int
isImage (SEXP x) {
    if ( strcmp( CHAR( asChar( GET_CLASS(x) ) ), "Image") != 0 && 
         strcmp( CHAR( asChar( GET_CLASS(x) ) ), "IndexedImage") != 0) return 0;
    if ( LENGTH( GET_DIM(x) ) != 3 ) return 0;
    return 1;
}
