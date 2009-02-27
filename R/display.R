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

## display displays static images
display = function(x, no.GTK, main, colorize,
  title=paste(deparse(substitute(x))), useGTK=TRUE) {
  validImage(x)
  
  if (!missing(main)) {
    warning("'main' is deprecated. Please use 'title' instead.")
    title=main
  }
  if (!missing(no.GTK)) {
    warning("'no.GTK' is deprecated. Please use 'useGTK' instead.")
    no.GTK=!useGTK
  }
  if (!missing(colorize)) warning("'colorize' is deprecated.")
 
  title = as.character(title)
  useGTK = as.logical(useGTK)
  stopifnot(length(useGTK)==1L, length(title)==1L)
  
  invisible(.Call("lib_display", castImage(x), title, useGTK, PACKAGE="EBImage"))
}

## animate displays animated sequences of images
animate = function (x) {
  validImage(x)
  invisible(.Call("lib_animate", castImage(x), PACKAGE="EBImage"))
}







