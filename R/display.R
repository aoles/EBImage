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
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("display", signature(x="ImageX"),
  function(x, no.GTK=FALSE, main=NULL, colorize=NULL) {
    validObject(x)
    if (is.null(main)) main=paste(deparse(substitute(x), 500), collapse="\n")

    if (!is.null(colorize)) {
      mx <- max(x)
      ## cr <- colorRamp(c("#FF0000", "#FFAA00", "#FFFF00", "#00FF00", "#00FFFF", "#0000FF", "#FF00FF"))
      ## cols <- channel(c("black", sample(rgb(cr(seq_len(mx)/mx)/255), mx)), "rgb")
      index <- match(imageData(x), c(0,seq_len(mx)))
      cols <- as.integer(c(0, runif(mx, 1, 0xFFFFFF)))
      x <- Image(cols[index], dim(x), colormode=TrueColor)
    }
  invisible ( .ImageCall("lib_display", x, as.character(main), as.logical(no.GTK) ) )
  }
)

## animate displays images using ImageMagick
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("animate", signature(x="ImageX"),
  function (x, ...) {
    validObject(x)
    invisible ( .ImageCall("lib_animate", x ) )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("animate", signature(x="array"),
  function (x, ...) {
    validObject(x)
    animate(Image(x), ...)
  }
)





