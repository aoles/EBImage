# Flood fill for images and matrices

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

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
floodFill = function(x, pt, col, tolerance=0) {
  validImage(x)
  nf = numberOfFrames(x, 'render')
  nc = numberOfChannels(x)
  
  ## make sure that `pt` and `col` are lists of length matching the number of frames
  if ( is.list(pt) ) {
    if ( length(pt) != nf ) stop("length of 'pt' must match the number of 'render' frames")
  } else {
    pt = rep(list(pt), nf)
  }
  if ( is.list(col) ) {
    if ( length(col) != nf ) stop("length of 'col' must match the number of 'render' frames")
  } else {
    col = rep(list(col), nf)
  }
  
  for (i in seq_len(nf) ) {
    pti = pt[[i]]
    if ( is.list(pti) )
      pti = unlist(pti, use.names=FALSE)
    storage.mode(pti) = "integer"
    if ( any(pti<1L) || any(pti>dim(x)[1:2]) )
      stop("coordinates of starting point(s) 'pt' must be inside image boundaries")
    if ( !is.matrix(pti) || dim(pti)[2L]!=2L )
      pti = matrix(pti, nrow=length(pti)/2, ncol=2L, byrow=TRUE)
    np = dim(pti)[1L]
    pt[[i]] = pti
    
    cli = col[[i]]
    cli = if ( is.character(cli) )
      unlist(lapply(cli, function(col) rep_len(col2rgb(col, alpha=TRUE)/255, nc)))
    else 
      rep(cli, each=nc)
    storage.mode(cli) = storage.mode(x) 
    cli = matrix(rep_len(cli, np * nc), nrow=np, ncol=nc, byrow=TRUE)
    col[[i]] = cli
  }
  
  return( .Call(C_floodFill, x, pt, col, as.numeric(tolerance)))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
fillHull = function(x) {
  validImage(x)
  return(.Call(C_fillHull, x))
}
