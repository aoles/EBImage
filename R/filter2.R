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

filter2 = function(x, filter) {
  validObject(x)
  validObject(filter)
  
  dx = dim(x)
  df = dim(filter)
  dnames = dimnames(x)
  
  if (any(df%%2==0)) stop("dimensions of 'filter' matrix must be odd")
  if (any(dx[1:2]<df)) stop("dimensions of 'x' must be bigger than 'filter'")
  
  ## create fft filter matrix
  wf = matrix(0.0, nrow=dx[1L], ncol=dx[2L])
  cf = df%/%2
  
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
  
  dimnames(y) = dnames
  
  imageData(x) <- y
  x
}
