# Pairwise distances between frames

# Copyright (c) 2007 Oleg Sklyar

# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation; either version 2.1
# of the License, or (at your option) any later version.

# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# See the GNU Lesser General Public License for more details.
# LGPL license wording: http://www.gnu.org/licenses/lgpl.html

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
setMethod("frameDist", signature(x="Image",y="Image"),
  function(x, y, r, g, b, blur=TRUE, method="dist", verbose, ...) {
    if (missing(verbose)) verbose = options()$verbose
    if (colorMode(x)!=colorMode(y))
      stop("'x' and 'y' must be in the same color mode")
    if (colorMode(x)==Grayscale && (!missing(r)||!missing(g)||!missing(b)))
      warning("r, g, b are used only for TrueColor images")
    if (missing(r)) r = 1.0
    if (missing(g)) g = 1.0
    if (missing(b)) b = 1.0
    method = as.integer(switch(tolower(substr(method,1,3)), dis=0, dot=1, cor=2))
    weights = as.double(c(r,g,b,0.0))
    if (blur) {
      if (colorMode(x)==Grayscale) {
        m = morphKern(3); m[2,2] = 4; m = m/sum(m)
        x = filter2(x, m)
        y = filter2(y, m)
      } else {
        x = blur(x, 1.5, 1.0)
        y = blur(y, 1.5, 1.0)
      }
    }
    return(.Call("lib_frameDist", x, y, weights, method, as.integer(verbose)))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
setMethod("frameDist", signature(x="Image",y="missing"),
  function(x, y, r, g, b, blur=TRUE, method="dist", verbose, ...) {
    if (missing(verbose)) verbose = options()$verbose
    if (colorMode(x)==Grayscale && (!missing(r)||!missing(g)||!missing(b)))
      warning("r, g, b are used only for TrueColor images")
    if (missing(r)) r = 1.0
    if (missing(g)) g = 1.0
    if (missing(b)) b = 1.0
    method = as.integer(switch(tolower(substr(method,1,3)), dis=0, dot=1, cor=2))
    weights = as.double(c(r,g,b,0.0))
    if (blur) {
      if (colorMode(x)==Grayscale) {
        m = morphKern(3); m[2,2] = 4; m = m/sum(m)
        x = filter2(x, m)
      } else x = blur(x, 1.5, 1.0)
    }
    return(.Call("lib_frameDist", x, x, weights, method, as.integer(verbose)))
  }
)

