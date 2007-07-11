# Extraction of zernike features from indexed images

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

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("zernike.moments", signature(x="IndexedImage", ref="Image"),
  function(x, ref, R=30, N=5, apply.Gaussian=TRUE, ...) {
    if ( colorMode(x) != Grayscale || colorMode(ref) != Grayscale ) 
      .stop( "both 'x' and 'ref' must be Grayscale" )
    if ( any( dim(x) != dim(ref) ) )
      .stop("'x' and 'ref' must have the same dimensions")

    if ( dim(x)[3] == 1 )
      xy <- moments(x=x, ref=ref)[, c(3,4)]
    else
      xy <- lapply(moments(x=x, ref=ref), function(x) x[,c(3,4)] )
    .DoCall("lib_zernike", x, ref, xy, as.numeric(R), as.integer(N), as.integer(apply.Gaussian))
  } 
)


