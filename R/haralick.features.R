# Extraction of Haralick features from indexed images

# Copyright (c) 2007 Oleg Sklyar, Mike Smith

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
setMethod ("haralickMatrix", signature(x="IndexedImage", ref="Image"),
  function(x, ref, nc=32, ...) {
    if ( colorMode(x) != Grayscale || colorMode(ref) != Grayscale)
      .stop( "'x' and 'ref' must be Grayscale" )
    rref <- range(ref)
    if ( rref[1] < 0 || rref[2] > 1 )
      .stop( "'ref' image must be in the range [0,1]" )
    res <- .DoCall( "lib_co_occurrence", x, ref, as.integer(nc) )
    return( res )
  }
)

setMethod ("haralick.matrix", signature(x="IndexedImage", ref="Image"),
  function(x, ref, ...) {
    .Deprecated("haralickMatrix", "EBImage")
    haralickMatrix(x, ref, ...)
  }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("haralickFeatures", signature(x="IndexedImage", ref="Image"),
  function(x, ref, nc=32, ...) {
    hm <- haralickMatrix(x=x, ref=ref, nc=nc, ...)
    if ( is.null(hm) || !(is.array(hm) || is.list(hm)) ) return( NULL )
    do.features <- function(m) {
      res <- .DoCall( "lib_haralick", m )
      if ( is.matrix(res) )
        colnames(res) <- c("t.asm", "t.con", "t.cor", "t.var", "t.idm", "t.sav", "t.sva", 
                           "t.sen", "t.ent", "t.dva", "t.den", "t.f12", "t.f13")
      res
    }
    if ( !is.list(hm) ) return( do.features(hm) )
    lapply( hm, do.features )
  }   
)

setMethod ("haralick.features", signature(x="IndexedImage", ref="Image"),
  function(x, ref, ...) {
    .Deprecated("haralickFeatures", "EBImage")
    haralickFeatures(x, ref, ...)
  }
)


