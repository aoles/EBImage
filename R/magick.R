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
flt.contrast  <- as.integer(2)
flt.denoise   <- as.integer(3)
flt.despeckle <- as.integer(4)
flt.edge      <- as.integer(5)
flt.enhance   <- as.integer(6)
flt.equalize  <- as.integer(7)
flt.gamma     <- as.integer(8)
flt.median    <- as.integer(9)
flt.noise     <- as.integer(10)
flt.resize    <- as.integer(11)
flt.rotate    <- as.integer(12)
flt.sample    <- as.integer(13)
flt.segment   <- as.integer(14)
flt.sharpen   <- as.integer(15)
flt.unsharp   <- as.integer(16)
flt.athresh   <- as.integer(17)
flt.cthresh   <- as.integer(18)
flt.affinet   <- as.integer(19)
flt.modulate  <- as.integer(20)
flt.negate    <- as.integer(21)
flt.norm      <- as.integer(22)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("blur", signature(x="ImageX"),
  function (x, r=0, s=0.5) {
    if (r < 0 || s <= 0)
      stop("values of 'r' and 's' must be positive, set r=0 for automatic radius")
    if (r <= s && r != 0)
      warning("for reasonable results, 'r' should be larger than 's'")      
    return(.ImageCall("lib_filterMagick", x, flt.blur, as.numeric(c(r, s))))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("gblur", signature(x="ImageX"),
  function (x, r=0, s=0.5) {
    if (r < 0 || s <= 0)
      stop("values of 'r' and 's' must be positive, set r=0 for automatic radius")
    if (r <= s && r != 0)
      warning("for reasonable results, 'r' should be larger than 's'")    
    return(.ImageCall("lib_filterMagick", x, flt.gaussblur, as.numeric(c(r, s))))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("contrast", signature(x="ImageX"),
  function (x, sharpen=TRUE) {     
    return(.ImageCall("lib_filterMagick", x, flt.contrast, as.numeric(sharpen)))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("denoise", signature(x="ImageX"),
  function (x, r=0) {
    if (r < 0)
      stop("'r' must be positive, set r=0 for automatic selection")    
    return(.ImageCall("lib_filterMagick", x, flt.denoise, as.numeric(r)))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("despeckle", signature(x="ImageX"),
  function (x) {     
    return(.ImageCall ("lib_filterMagick", x, flt.despeckle, as.numeric(0)))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("edge", signature(x="ImageX"),
  function (x, r=0) {  
    return(.ImageCall("lib_filterMagick", x, flt.edge, as.numeric(r)))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("enhance", signature(x="ImageX"),
  function (x) {     
    return(.ImageCall("lib_filterMagick", x, flt.enhance, as.numeric(0)))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("equalize", signature(x="ImageX"),
  function (x) {    
    return(.ImageCall ("lib_filterMagick", x, flt.equalize, as.numeric(0)))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("cgamma", signature(x="ImageX"),
  function (x, level=1) {
    if (level < 0.8 || level > 2.3)
      warning("reasonable 'level' is between 0.8 and 2.3")     
    return(.ImageCall("lib_filterMagick", x, flt.gamma, as.numeric(level)))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("mediansmooth", signature(x="ImageX"),
  function (x, r=2) {
    if (r <= 1)
      stop("value of 'r' must be larger than 1")      
    return(.ImageCall("lib_filterMagick", x, flt.median, as.numeric(r)))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("noise", signature(x="ImageX"),
  function (x, type="G") {
    type = tolower(substr(type,1,1))
    param = as.numeric(switch(type, u= 1, g= 2, m= 3, i= 4, l= 5, p= 6, 2))
    if (param == 2 && type != "g")
      warning("unsupported noise type, using 'gaussian' instead. Possible values: uniform, gaussian, multi, impulse, laplace and poisson")    
    return(.ImageCall("lib_filterMagick", x, flt.noise, param))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("resize", signature(x="ImageX"),
  function (x, w, h, blur=1, filter="Lanczos") {
    if (missing(w) && missing(h))
      stop("either 'w' or 'h' must be specified")
    dimx = dim(x)
    if (missing(w)) w = dimx[1]*h / dimx[2]
    else if (missing(h)) h = dimx[2]*w / dimx[1]
    if (w <= 0 || h <= 0)
      stop("width and height of a new image must be non zero positive")
    filter <- switch(tolower(substr(filter,1,3)), 
      poi=0, box=1, tri=2,  her=3,  han=4,  ham=5,  bla=6, gau=7, 
      qua=8, cub=9, cat=10, mit=11, lan=12, bes=13, sin=14, 12)
    param = as.numeric(c(w, h, blur, filter))
    return(.ImageCall("lib_filterMagick", x, flt.resize, param))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("resample", signature(x="ImageX"),
  function (x, w, h) {
    if (missing(w) && missing(h))
      stop("either 'w' or 'h' must be specified")
    dimx = dim(x)
    if (missing(w)) w = dimx[1]*h / dimx[2]
    else if (missing(h)) h = dimx[2]*w / dimx[1]
    if (w <= 0 || h <= 0)
      stop("width and height of a new image must be non zero positive")
    param = as.numeric(c(w, h))
    return(.ImageCall("lib_filterMagick", x, flt.sample, param))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("rotate", signature(x="ImageX"),
  function (x, angle=90, col) {
    if (!missing(col))
      warning("argument 'col' is ignored, not implemented yet, black is used as default")     
    return(.ImageCall("lib_filterMagick", x, flt.rotate, as.numeric(angle) ))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("segment", signature(x="ImageX"),
  function (x, cl=10, s=1.5) {
    if (cl < 1)
      stop("cluster size 'cl' must be larger than 1")
    if (s <= 0)
      stop("smoothness 's' must be positive")
    param = as.numeric(c(cl, s))
    return(.ImageCall("lib_filterMagick", x, flt.segment, param))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("sharpen", signature(x="ImageX"),
  function (x, r=0, s=0.5) {
    if (r <= s && r != 0)
      warning("for reasonable results, 'r' should be larger than 's'")
    if (r < 0 || s <= 0)
      stop("values of 'r' and 's' must be positive, alternatively r=0 selects radius automatically")
    param = as.numeric(c(r, s))
    return(.ImageCall("lib_filterMagick", x, flt.sharpen, param))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("umask", signature(x="ImageX"),
  function (x, r=0, s=0.5, amount=5, t=2) {
    if (r <= s && r != 0)
      warning("for reasonable results, 'r' should be larger than 's'")
    if (r < 0 || s <= 0 || amount < 0 || t < 0)
      stop("all arguments must be positive, alternatively r=0 selects radius automatically")
    param = as.numeric(c(r, s, amount, t))
    return(.ImageCall("lib_filterMagick", x, flt.unsharp, param))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("athresh", signature(x="ImageX"),
  function (x, w=10, h=10, offset=0) {
    if (w < 2 || h < 2)
      stop("width 'w' and height 'h' must be larger than 1")
    param = as.numeric(c(w, h, offset))
    return(.ImageCall("lib_filterMagick", x, flt.athresh, param))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("cthresh", signature(x="ImageX"),
  function (x, threshold=0) {
    return(.ImageCall("lib_filterMagick", x, flt.cthresh, as.numeric(threshold)))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("affinet", signature(x="ImageX"),
  function (x, sx=0, rx=0, ry=0, sy=0, tx=0, ty=0) {
    param = as.numeric(c(sx, rx, ry, sy, tx, ty))
    return(.ImageCall("lib_filterMagick", x, flt.affinet, param))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("modulate", signature(x="ImageX"),
  function (x, value=100) {
    return(.ImageCall("lib_filterMagick", x, flt.modulate, as.numeric(value)))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("negate", signature(x="ImageX"),
  function (x) {
    return(.ImageCall("lib_filterMagick", x, flt.negate, as.numeric(0)))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("normalize2", signature(x="ImageX"),
  function (x) {
    return(.ImageCall("lib_filterMagick", x, flt.norm, as.numeric(0)))
  }
)


