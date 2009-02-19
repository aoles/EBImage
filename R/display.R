# display and animate methods for different classes

# Copyright (c) 2005-2007 Oleg Sklyar

# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation; either version 2.1
# of the License, or (at your option) any later version.

# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# See the GNU Lesser General Public License for more details.
# LGPL license wording: http://www.gnu.org/licenses/lgpl.html


## display uses GTK
display = function(x, title=paste(deparse(substitute(x))), useGTK=TRUE, ...) {
  title = as.character(title)
  useGTK = as.logical(useGTK)
  stopifnot(length(useGTK)==1L, length(title)==1L, is(x, "Image"))
  validObject(x)
  invisible(.Call("lib_display", x, title, useGTK, PACKAGE="EBImage"))
}

## animate displays images using ImageMagick
animate = function (x, ...) {
  stopifnot(is(x, "Image"))
  validObject(x)
  invisible(.Call("lib_animate", x, PACKAGE="EBImage"))
}







