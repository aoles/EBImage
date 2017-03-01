# 2D convolution-based linear filter for images and matrix data

# Copyright (c) 2007-2015, Andrzej Oleś, Gregoire Pau, Oleg Sklyar

# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation; either version 2.1
# of the License, or (at your option) any later version.

# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# See the GNU Lesser General Public License for more details.
# LGPL license wording: http://www.gnu.org/licenses/lgpl.html

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

filter2 = function(x, filter, boundary = c("circular", "replicate")) {
  if ( is.numeric(boundary) ) {
    val = boundary[1L]
    boundary = "linear"
  }
  else
    boundary = match.arg(boundary)
  
  validObject(x)
  validObject(filter)
  
  d = dx = dim(x)
  df = dim(filter)
  dnames = dimnames(x)
  
  if (any(df%%2==0)) stop("dimensions of 'filter' matrix must be odd")
  if (any(dx[1:2]<df)) stop("dimensions of 'x' must be bigger than 'filter'")
  
  cf = df%/%2
  
  res = x
  
  switch(boundary,
         ## default mode just wraps around edges
         circular = {
           x = imageData(x)
         },
         ## pad with a given value
         linear = {
           dx[1:2] = dx[1:2] + cf[1:2]
           xpad = array(val, dx)
           ## is there a better way of doing this?
           x = do.call("[<-", c(quote(xpad), lapply(d, function(x) enquote(1:x)), quote(x)) )
         },
         replicate = {
           x = imageData(x)
           
           dx[1:2] = dx[1:2] + df[1:2] - 1L
           
           rep.dim <- function(x, dim, index, times) {
             xs <- asub(x, idx=index, dims=dim, drop=FALSE)
             ds <- dim(xs)
             ds[dim] <- times
             if ( dim==2L ) {
               xs <- split(xs, ceiling(seq_along(xs)/ds[1L]))
               xs <- sapply(xs, FUN=rep, times=times)
             } else {
               xs <- rep(xs, each=times)
             }
             array(xs, dim=ds)
           }
           
           # Add left and right colums
           lc <- rep.dim(x, 2L, 1L, cf[2L])
           rc <- rep.dim(x, 2L, d[2L], cf[2L])
           x <- abind(x, rc, lc, along=2L)
           # Add top and bottom rows
           tr <- rep.dim(x, 1L, 1L, cf[1L])
           br <- rep.dim(x, 1L, d[1L], cf[1L])
           x <- abind(x, br, tr, along=1L)
         }
  )
  
  ## create fft filter matrix
  wf = matrix(0.0, nrow=dx[1L], ncol=dx[2L])
  
  wf[c(if (cf[1L]>0L) (dx[1L]-cf[1L]+1L):dx[1L] else NULL, 1L:(cf[1L]+1L)), 
     c(if (cf[2L]>0L) (dx[2L]-cf[2L]+1L):dx[2L] else NULL, 1L:(cf[2L]+1L))] = filter
  
  wf = fftw2d(wf)
  
  pdx = prod(dx[1:2])
  
  .filter = function(xx) Re(fftw2d(fftw2d(xx)*wf, inverse=1)/pdx)
  
  ## convert to a frame-based 3D array
  if ( length(dx) > 2L ) {    
    y = apply(x, 3:length(dx), .filter)
    dim(y) = dx
  }
  else {
    y = .filter(x)
  }
  
  if ( boundary!="circular" ) {
    y = asub(y, list(1:d[1L], 1:d[2L]), 1:2)
  }
  
  dimnames(y) = dnames
  
  imageData(res) <- y
  res
}
