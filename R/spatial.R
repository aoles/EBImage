# Filter methods for class Image

# Copyright (c) 2005 Oleg Sklyar

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
setMethod ("normalize", signature(x="Image"),
  function (x, separate=TRUE, ft=c(0,1), ...) {
    if (colorMode(x) == TrueColor)
       stop("this method doesn't support the \'TrueColor\' color mode, use \'normalize2\' instead")
 
    ft <- as.numeric (ft)
    if ( diff(ft) == 0 )
      stop("normalization range is 0")
    separate <- as.integer(separate)
    x = .DoCall("lib_normalize", x, separate, ft)
    if (is(x, "IndexedImage")) x = as.Image(x)
    return(x)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("flip", signature(x="Image"),
  function (x) {
    nd=as.list(rep(T,length(dim(x))))
    nd[[2]]=dim(x)[2]:1
    do.call('[',c(list(x),nd))
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("flop", signature(x="Image"),
  function (x) {
    nd=as.list(rep(T,length(dim(x))))
    nd[[1]]=dim(x)[2]:1
    do.call('[',c(list(x),nd))
  }
)

## Translate a set of images according to a matrix of translation
## A C function was needed for performance reasons
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("translate", signature(x="Image"),
  function (x, v) {
    cat('@GP TODO\n')
    return (.DoCall("translate", x, v))
  }
)
