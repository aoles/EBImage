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
floodFill = function(x, pts, col, tolerance=0) {
  validImage(x)
  nf = numberOfFrames(x, 'render')
  nc = numberOfChannels(x)
  
  ## make sure that `pt` and `col` are lists of length matching the number of frames
  format_as_list = function(x, nf) {
    if ( is.list(x) )
      if ( length(x)==nf )
        return(x)
      else if ( nf > 1L)
        stop(sprintf("length of '%s' must match the number of 'render' frames"), deparse(substitute(x)))
    rep(list(x), nf) 
  }
  pts = format_as_list(pts, nf)
  col = format_as_list(col, nf)
  
  for (i in seq_len(nf) ) {
    pti = pts[[i]]
    if ( is.list(pti) )
      if ( is.data.frame(pti) ) 
        pti = as.matrix(pti)
      else 
        pti = unlist(pti, use.names=FALSE)
    storage.mode(pti) = "integer"
    if ( !is.matrix(pti) || dim(pti)[2L]!=2L )
      pti = matrix(pti, nrow=length(pti)/2, ncol=2L, byrow=TRUE)
    np = dim(pti)[1L]
    if ( any( pti<1L ) || any( pti>rep(dim(x)[1:2], each=np) ) )
      stop("coordinates of starting point(s) 'pts' must be inside image boundaries")
    pts[[i]] = pti
    
    cli = col[[i]]
    cli = if ( is.character(cli) )
      unlist(lapply(cli, function(col) rep_len(col2rgb(col, alpha=TRUE)/255, nc)))
    else 
      rep(cli, each=nc)
    storage.mode(cli) = storage.mode(x) 
    cli = matrix(rep_len(cli, np * nc), nrow=np, ncol=nc, byrow=TRUE)
    col[[i]] = cli
  }
  
  return( .Call(C_floodFill, x, pts, col, as.numeric(tolerance)))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
fillHull = function(x) {
  validImage(x)
  return(.Call(C_fillHull, x))
}
