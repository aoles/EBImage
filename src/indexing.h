/* -------------------------------------------------------------------------

Image indexing routines: index <--> point
 
Copyright (c) 2006 Oleg Sklyar

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License 
as published by the Free Software Foundation; either version 2.1
of the License, or (at your option) any later version.          

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the GNU Lesser General Public License for more details.
LGPL license wording: http://www.gnu.org/licenses/lgpl.html

------------------------------------------------------------------------- */

#include "common.h"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
struct Point {
    Point();
    Point(int x, int y);
    Point(const Point& obj);
    int x;
    int y;
};
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
Point   getpoint (const int & index, const int & xsize);
int     getindex (const Point & pt, const int xsize);
int     getindex (const int & x, const int & y, const int xsize);
double  dist     (const Point & p1, const Point & p2);
