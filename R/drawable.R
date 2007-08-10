# Drawables for class Image

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
setMethod ("drawtext", signature(img="Image", xy="numeric", labels="character"),
  function(img, xy, labels, ...) {
    # FIXME: needs checks
    drawtext(img, list(xy), list(labels), ...)
  }
)

setMethod ("drawtext", signature(img="Image", xy="matrix", labels="character"),
  function(img, xy, labels, ...) {
    # FIXME: needs checks
    drawtext(img, list(xy), list(labels), ...)
  }
)

setMethod ("drawtext", signature(img="Image", xy="list", labels="list"),
  function(img, xy, labels, ...) {
    .Call("lib_drawText", img, xy, labels)
  }
)
  

