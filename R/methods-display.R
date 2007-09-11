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


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
display <- function(x, no.GTK=FALSE, ...) {
  if ( !is.Image(x) && !is.array(x) && !is.matrix(x) )
    stop(sprintf("unable to find an implementation for function 'display', for signature '%s'", class(x)))
  main <- try(list(...)$main, silent=TRUE)
  if ( is.null(main) || is(main, "try-error") ) main <- paste(deparse(substitute(x), 500), collapse="\n")
  if ( !is.Image(x) ) x <- Image(x)
  if ( !.isCorrectType(x) ) x <- .correctType (x)
  if ( is(x, "IndexedImage") ) x <- normalize(x)
  invisible ( .DoCall("lib_display", x, as.character(main), as.logical(no.GTK) ) )
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#setMethod ("display", signature(x="Image"),
#  function (x, no.GTK=FALSE, ...) {
#    main <- try(list(...)$main, silent=TRUE)
#    if ( is.null(main) || is(main, "try-error") ) main <- paste(deparse(substitute(x), 500), collapse="\n")
#    if ( !.isCorrectType(x) ) x <- .correctType (x)
#    invisible ( .DoCall("lib_display", x, as.character(main), as.logical(no.GTK) ) )
#  }
#)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("animate", signature(x="Image"),
  function (x, ...) {
    if ( !.isCorrectType(x) ) x <- .correctType (x)
    invisible ( .DoCall("lib_animate", x ) )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#setMethod ("display", signature(x="array"),
#  function (x, no.GTK=FALSE, ...) {
#    main <- try(list(...)$main, silent=TRUE)
#    if ( is.null(main) || is(main, "try-error") ) main <- paste(deparse(substitute(x), 500), collapse="\n")
#    display(Image(x), no.GTK=no.GTK, main=main, ...)
#  }
#)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("animate", signature(x="array"),
  function (x, ...) {
    animate(Image(x), ...)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#setMethod ("display", signature(x="IndexedImage"),
#  function (x, no.GTK=FALSE, ...) {
#    main <- try(list(...)$main, silent=TRUE)
#    if ( is.null(main) || is(main, "try-error") ) main <- paste(deparse(substitute(x), 500), collapse="\n")
#    if ( !.isCorrectType(x) )
#      x <- .correctType ( normalize(x))
#    else
#      x <- normalize (x)
#    invisible ( .DoCall("lib_display", x, as.character(main), as.logical(no.GTK) ) )
#  }
#)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("animate", signature(x="IndexedImage"),
  function (x, ...) {
    if ( !.isCorrectType(x) ) x <- .correctType (x)
    invisible ( .DoCall("lib_animate", normalize(x) ) )
  }
)


