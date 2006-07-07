/* -------------------------------------------------------------------------

Distance map filter
 
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

Function 'lz_dist_map' is adapted from the ANIMAL package,
corresponding license is applicable to this piece of code!
Algorithm for this function by: R. Lotufo, F. Zampirolli, 
SIBGRAPI 2001, 100-105, 2001

------------------------------------------------------------------------- */

#include "common.h"

extern "C" {
    SEXP distMap(SEXP rimage, SEXP alg);
};

