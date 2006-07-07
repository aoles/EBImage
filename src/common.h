/* -------------------------------------------------------------------------

Common definitions for the EBImage project
 
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

#include <Magick++.h>
#include <R.h>
#include <Rdefines.h>

using namespace std;
using namespace Magick;

typedef         list<Image>    MagickStack;
typedef         Image          MagickImage;

extern bool verbose;

bool assertImage(SEXP rimage);
bool assertImage2D(SEXP rimage);

extern "C" {
    SEXP setVerbose(SEXP);
};

