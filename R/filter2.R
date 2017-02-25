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
  
  switch(boundary,
    ## default mode just wraps around edges
    circular = {},
    ## pad with a given value
    linear = {
      dx[1:2] = dx[1:2] + cf[1:2]
      xpad = array(val, dx)
      ## is there a better way of doing this?
      imageData(x) = do.call("[<-", c(quote(xpad), lapply(d, function(x) enquote(1:x)), quote(x)) )
    },
    replicate = {
      dx[1:2] = dx[1:2] + df[1:2] - 1L
      
      # Small helper function that takes a slice out of array and replicates it (note: slicing is limited to first 2 dims) 
      rep.dim <- function(x,dim,index,times) {
        x.s <- abind::asub(x,idx=index,dims = dim,drop = FALSE)
        dim.s <- dim(x.s)
        dim.s[dim] <- times
        if (dim==2) {
          x.s <- split(x.s,ceiling(seq_along(x.s)/nrow(x)))
          x.s <- sapply(x.s,FUN = rep,times=times)
        } else {
          x.s <- rep(x.s,each=times)
        }
        out <- array(x.s,dim = dim.s)
      }
      
      # With the helper function the overal replicating of border rows/columns becomes simple
      # Add left and right colums
      lc = rep.dim(x,2,1,cf[2])
      rc = rep.dim(x,2,ncol(x),cf[2])
      imageData(x) = abind(lc,x,rc,along = 2)
      # Add top and bottom rows
      tr = rep.dim(x,1,1,cf[1])
      br = rep.dim(x,1,nrow(x),cf[1])
      imageData(x) = (tr,x,br,along = 1)
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
    y = asub(y, list(cf[1]+(1:d[1L]), cf[2]+(1:d[2L])), 1:2)
  }
  
  dimnames(y) = dnames
  
  imageData(x) <- y
  x
}
