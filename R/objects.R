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

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
paintObjects = function (x, tgt, opac=c(1, 1), col=c('red', NA), thick=FALSE, closed=FALSE) {
  validImage(x)
  if (colorMode(x)!=Grayscale) stop("'", deparse(substitute(x), width.cutoff = 500L, nlines = 1L), "' must be in 'Grayscale' color mode")
  if (any(dim(x)[1:2] != dim(tgt)[1:2])) stop( "'x' and 'tgt' must have the same size" )
  if (.numberOfFrames(x,'render') != .numberOfFrames(tgt,'render')) stop( "'x' and 'tgt' must have the same number of render frames" )                           
  
  if ((l=length(col))>2L) col[1:2] else col = switch(l+1L, c(NA, NA), c(col, NA), col)
  if ((l=length(opac))>2L) opac[1:2] else opac = switch(l+1L, c(1, 1), c(opac, 1), opac)
  
  zcol = is.na(col)
  col[zcol] = 'white'
  opac[zcol] = 0
  
  opac = as.numeric(opac)
  if (any(opac < 0) || any(opac > 1)) stop("all opacity values must be in the range [0,1]" )
  
  ## behavior at image borders
  if ( isTRUE(closed) ) {
    col[3L] = col[1L] 
    opac[3L] = opac[1L]
  }
  else {
    col[3L] = col[2L] 
    opac[3L] = opac[2L]
  }
    
  .Call(C_paintObjects, castImage(x), castImage(tgt), opac, Image(col, colormode = colorMode(tgt)), as.integer(thick))
}


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stackObjects = function (x, ref, combine=TRUE, bg.col='black', ext) {
  ## check arguments
  if (colorMode(x) != Grayscale) stop("'x' must be an image in 'Grayscale' color mode or an array")
  checkCompatibleImages(x, ref, 'render')
  nz = .numberOfFrames(x, 'total')
  
  ## uses 'computeFeatures.moment' to get centers and theta
  hf = lapply(getFrames(x), computeFeatures.moment)

  if (missing(ext)) {
    ext = unlist(sapply(hf, function(h) h[, 'm.majoraxis']))/2
    ext = quantile(ext, 0.98, na.rm=TRUE)
  }
  xy = lapply(hf, function(h) h[,c('m.cx', 'm.cy'), drop=FALSE])
  if (nz==1) xy = xy[[1]]
  bg.col = Image(bg.col, colormode=colorMode(ref))
  
 res = .Call(C_stackObjects, castImage(x), castImage(ref), bg.col, xy, as.numeric(ext))
 if (!combine || !is.list(res)) res
 else combine(res)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rmObjects = function (x, index, reenumerate = TRUE) {
  validImage(x)
  if (!is.list(index)) index = list(index)
  index = lapply (index, as.integer)
  .Call(C_rmObjects, castImage(x), index, as.integer(reenumerate))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
reenumerate = function(x) {
  validImage(x)
  if (any(max(x)<0)) stop("'x' contains negative values and is incorrectly formed")
  .dim = dim(x)
  
  if(!is.integer(x)) storage.mode(x) = 'integer'
  
  # prepare 3D array for the 'apply' function
  dim(x) = c(.dim[seq_len(2)], .numberOfFrames(x, 'total'))
  
  res = apply(x, 3, function(im) {
    from = sort.int(unique.default(c(0L, as.vector(im))))
    to = seq_along(from) - 1L
    to[match(im, from)]
  })
  
  # reconstruct Image
  dim(res) = .dim
  imageData(x) = res
  
  x
}
