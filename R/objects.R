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
paintObjects = function (x, tgt, opac=c(1, 1), col=c('red', NA)) {
  validImage(x)
  if (colorMode(x)!=Grayscale)  stop("'", paste(deparse(substitute(x))), "' must be in 'Grayscale' color mode")
  if (any(dim(x)[1:2] != dim(tgt)[1:2])) stop( "'x' and 'tgt' must have the same size" )
  if (getNumberOfFrames(x,'render') != getNumberOfFrames(tgt,'render')) stop( "'x' and 'tgt' must have the same number of render frames" )                           

  col = c(col, rep(NA, 3-length(col)))
  opac = c(opac, rep(1, 3-length(opac)))
  zcol = which(is.na(col))
  col[zcol] = 'white'
  opac[zcol] = 0
  
  opac = as.numeric(opac)
  if (any(opac < 0) || any(opac > 1)) stop("all opacity values must be in the range [0,1]" )

  .Call("paintObjects", castImage(x), castImage(tgt), opac, Image(col), PACKAGE='EBImage')
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
stackObjects = function (x, ref, index, combine=TRUE, rotate, bg.col='black', ext, centerby, rotateby) {
  checkCompatibleImages(x, ref, 'render')
  nz = getNumberOfFrames(x, 0)
  
  if (colorMode(x) != Grayscale) stop("'x' must be an image in 'Grayscale' color mode or an array")
  if (!missing(centerby)) warning("'centerby' is deprecated and ignored") 
  if (!missing(rotateby)) warning("'rotateby' is deprecated and ignored")
  if (!missing(rotate)) warning("'rotate' is deprecated and ignored. Please use 'rotate' objects after 'stackObjects'.")
  if (!missing(index)) warning("'index' is deprecated and ignored")
    
  ## uses 'hullFeatures' to get centers and theta
  hf = hullFeatures(x)
  if (nz==1) hf = list(hf)

  if (missing(ext)) {
    extx = unlist(sapply(hf, function(h) h[, 'g.l1']))
    ext = 2.0*sqrt(quantile(extx, 0.98, na.rm=TRUE))
  }
  xy = lapply(hf, function(h) h[,c('g.x', 'g.y'), drop=FALSE])
  if (nz==1) xy = xy[[1]]
  bg.col = Image(bg.col, colormode=colorMode(ref))
    
  res = .Call ("stackObjects", castImage(x), castImage(ref), bg.col, xy, as.numeric(ext), PACKAGE='EBImage')
  if (!combine || !is.list(res)) return(res)
  else return(combine(res))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rmObjects = function (x, index) {
  validImage(x)
  if (is.list(index)) index = lapply (index, as.integer)
  else index = list( as.integer(index) )
  return (.Call ("rmObjects", castImage(x), index, PACKAGE='EBImage') )
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
reenumerate = function(x) {
  validImage(x)
  if (any(max(x)<0)) stop("'x' contains negative values and is incorrectly formed")
  .dim=dim(x)
  
  storage.mode(x)='integer'
  dim(x)=c(.dim[1:2],getNumberOfFrames(x,'total'))
  
  imageData(x) = apply(imageData(x), 3, function(im) {
    from = as.integer(names(table(im)))
    to = seq_along(from)-1
      to[match(im, from)]
  })
  dim(x)=.dim
  storage.mode(x)="numeric"
  
  x
}
