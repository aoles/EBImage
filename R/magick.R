# Filter methods for class Image

# Copyright (c) 2005 Oleg Sklyar

# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation; either version 2.1
# of the License, or (at your option) any later version.

# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# See the GNU Lesser General PSublic License for more details.
# LGPL license wording: http://www.gnu.org/licenses/lgpl.html

# ImageMagick filters below

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
flt.blur      <- as.integer(0)
flt.gaussblur <- as.integer(1)
flt.resize    <- as.integer(11)
flt.rotate    <- as.integer(12)

## Normalize to [0;1] if needed and go back to the original scale since ImageMagick assumes that image values are within [0;1]
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ImageMagickCall=function(x,flt,...) {
  validImage(x)

  norm=FALSE
  r=range(x)
  min=r[1]
  scale=r[2]-r[1]
  if (scale==0.0) scale=1
  if (min<0 | r[2]>1) {
    norm=TRUE
    x=(x-min)/scale
  }
  y=.Call("lib_filterMagick",castImage(x),flt,...,PACKAGE='EBImage')
  if (norm) y=y*scale+min
  y
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
blur = function (x, r=0, s=0.5) {
  if (r < 0 || s <= 0)
    stop("values of 'r' and 's' must be positive, set r=0 for automatic radius")
  if (r <= s && r != 0)
    warning("for reasonable results, 'r' should be larger than 's'")
  return(ImageMagickCall(x, flt.blur, as.numeric(c(r, s))))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
gblur = function (x, r=0, s=0.5) {
  if (r < 0 || s <= 0)
    stop("values of 'r' and 's' must be positive, set r=0 for automatic radius")
  if (r <= s && r != 0)
    warning("for reasonable results, 'r' should be larger than 's'")
  return(ImageMagickCall(x, flt.gaussblur, as.numeric(c(r, s))))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
equalize = function (x) {
  .Defunct(msg=gettextf("'%s' is defunct.\nUse standard R functions such as 'rank' or 'cut' instead.", old))
  NULL
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
resize = function (x, w, h, blur=1, filter="Lanczos") {
  if (missing(w) && missing(h))
    stop("either 'w' or 'h' must be specified")
  dimx = dim(x)
  if (missing(w)) w = dimx[1]*h / dimx[2]
    else if (missing(h)) h = dimx[2]*w / dimx[1]
  if (w <= 0 || h <= 0)
    stop("width and height of a new image must be non zero positive")
  if (length(w)>1 || length(h)>1)
      stop("width and height must be scalar values")
  filter <- switch(tolower(substr(filter,1,3)),
                   poi=0, box=1, tri=2,  her=3,  han=4,  ham=5,  bla=6, gau=7,
                   qua=8, cub=9, cat=10, mit=11, lan=12, bes=13, sin=14, 12)
  param = as.numeric(c(w, h, blur, filter))
  return(ImageMagickCall(x, flt.resize, param))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rotate = function (x, angle=90) {
  return(ImageMagickCall(x, flt.rotate, as.numeric(angle) ))
}


