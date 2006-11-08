/* -------------------------------------------------------------------------
Image indexing routines: index <--> point
Copyright (c) 2006 Oleg Sklyar
See: indexing.h for license
------------------------------------------------------------------------- */
#include "indexing.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
Point:: Point() {
    x = 0;
    y = 0;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
Point:: Point(int x, int y) {
    this->x = x;
    this->y = y;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
Point:: Point(const Point& obj) {
    x = obj.x;
    y = obj.y;
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
Point getpoint(const int & index, const int & xsize) {
    Point res(0, (int)floor(index / xsize));
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
