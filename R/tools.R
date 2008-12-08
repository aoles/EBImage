# Package tools

# Copyright (c) 2006 Oleg Sklyar

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
.DoCall <- function (name, ...) {
    .Call(name, ..., PACKAGE = "EBImage")
}

## .ImageCall valids an image and ensures that image storage.mode is the correct one
## .ImageCall _must_ be called to call an EBImage image C function (since the validity of the image won't be check in the C code !)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.ImageCall <- function (name, x, ...) {
  validObject(x)
  x=ensureStorageMode(x)
  .Call(name, x, ..., PACKAGE = "EBImage")
}
ensureStorageMode=function(x) {
  validObject(x)
  if (colorMode(x)!=TrueColor & storage.mode(imageData(x))!='double') storage.mode(imageData(x))='double'
  if (colorMode(x)==TrueColor & storage.mode(imageData(x))!='integer') storage.mode(imageData(x))='integer'
  x
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
. <- function (string) {
  ##    .DoCall ("lib_", as.character(string) )
  return (string)
}

.stop <- function (string, ...) stop( .(string), ... )
.warning <- function (string, ...) warning( .(string), ... )
.cat <- function (string, ...) cat( .(string), ... )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("channel", signature(x="ANY", mode="character"),
  function (x, mode) {
    mode <- tolower (mode)
    modeNo <- as.integer( switch (EXPR=mode, rgb=0, grey=, gray=1, r=, red=2, 
            g=, green=3, b=, blue=4, asred=5, asgreen=6, asblue=7, x11=8, -1) )
    if ( modeNo < 0 )
      stop( "wrong conversion mode")
    if ( !is.numeric(x) && !is.character(x) )
      stop( "argument must be coercible to either numeric or character" )
    res <- .DoCall("lib_channel", x, modeNo )
    if ( !is.null(res) )
      res [ which( is.na(x) ) ] = NA
    if ( is.null(res) || is.character(res) ) return (res)
    if ( is.array(x) ) dim (res) <- dim (x)
    return (res)
  }
)

## check if x (indexing image) and ref (image) are compatible
checkCompatibleImages=function(x,ref) {
  if (missing(ref)) {
    if (colorMode(x) == TrueColor)
      stop( "'x' must be an Image object not in 'TrueColor' color mode" )
  } else {
    if (colorMode(x) == TrueColor || colorMode(ref) == TrueColor)
      stop( "'x' and 'ref' must be Image objects not in 'TrueColor' color mode" )
    
    if (getNumberOfFrames(x,'total')!=getNumberOfFrames(ref,'total'))
      stop( "'x' and 'ref' must have the same total number of frames" )
    
    if (any(dim(x)[1:2]!=dim(ref)[1:2])  )
      stop( "'x' and 'ref' must have the same spatial 2D dimensions" )
  }
}
