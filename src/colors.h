/* -------------------------------------------------------------------------

Color conversion routines
 
Copyright (c) 2005 Oleg Sklyar

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

extern "C" {
    SEXP any2rgb     (SEXP x);
    SEXP any2gray    (SEXP x);
    SEXP any2X11char (SEXP x);
    SEXP add2rgb     (SEXP x, SEXP y);
    SEXP sub2rgb     (SEXP x, SEXP y);
    SEXP scale2rgb   (SEXP x, SEXP factor);
    SEXP getred      (SEXP x);
    SEXP asred       (SEXP x);
    SEXP getgreen    (SEXP x);
    SEXP asgreen     (SEXP x);
    SEXP getblue     (SEXP x);
    SEXP asblue      (SEXP x);

};
