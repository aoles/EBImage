# 2D convolution-based linear filter for images and matrix data

# Copyright (c) 2007 Gregoire Pau, Oleg Sklyar

# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation; either version 2.1
# of the License, or (at your option) any later version.

# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# See the GNU Lesser General Public License for more details.
# LGPL license wording: http://www.gnu.org/licenses/lgpl.html

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
setMethod("filter2", signature(x="array",filter="matrix"),
  function(x, filter, ...) {
    df = dim(filter)
    if (any(df%%2==0) || df[1]!=df[2])
      stop("dimensions of 'filter' matrix must be equal and odd")
    dx = dim(x)
    if (length(dx)<2 || length(dx)>3)
      stop("'x' must be a matrix or a 3D array")
    if (any(dx[1:2]<df[1]))
      stop("first two dimensions of 'x' are too small")
    # find centres of x and filter
    cx = dx%/%2
    cf = df%/%2
    # create fft filter matrix
    fltr = matrix(0.0,nr=dx[1],nc=dx[2])
    fltr[(cx[1]-cf[1]):(cx[1]+cf[1]),(cx[2]-cf[2]):(cx[2]+cf[2])] = filter
    fltr = fft(fltr)
    # if matrix, convert it to array with 3rd dim 1 (to allow general apply)
    if (length(dx)==2) dim(x) = c(dx,1)
    index1 = c(cx[1]:dx[1],1:(cx[1]-1))
    index2 = c(cx[2]:dx[2],1:(cx[2]-1))
    pdx = prod(dim(x)[1:2])
    x = apply(x, 3, function(xx) {
      dim(xx) = dx[1:2]
      Re(fft(fft(xx)*fltr,inverse=T)/pdx)[index1,index2]
    })
    dim(x) = dx
    x
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
setMethod("filter2", signature(x="Image",filter="matrix"),
  function(x, filter, ...) {
    if (colorMode(x)!=Grayscale)
      stop("'x' must be a Grayscale image")
    imageData(x) = filter2(imageData(x),filter,...)
    x
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
setMethod("filter2", signature(x="array",filter="missing"),
  function(x, filter, ...) {
    filter = mkball()
    filter2(x, filter, ...)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
setMethod("filter2", signature(x="Image",filter="missing"),
  function(x, filter, ...) {
    if (colorMode(x)!=Grayscale)
      stop("'x' must be a Grayscale image")
    imageData(x) = filter2(imageData(x),mkball(),...)
    x
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
setMethod("sharpen2", signature(x="Image"),
  function(x, ...) {
    if (colorMode(x)!=Grayscale)
      stop("'x' must be a Grayscale image")
    mx = mean(x)
    m = matrix(c(1,1,1,1,-1,1,1,1,1),3,3)
    x = x - filter2(x, m/sum(m)/2)
    x[x<0] = 0.0
    x*mx/mean(x)
  }
)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
clip = function(z,min=0,max=1) {
  z[z>max] = max
  z[z<min] = min
  z
}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
mkball = function(n=15,pow=1) {
  n2 = n%/%2
  ball = matrix(seq(-n2,n2),nc=n,nr=n)
  ball = ball*ball+t(ball)^2
  ball = 2.0*max(ball)/3.0 - ball
  ball[ball<0] = 0
  ball = ball^pow
  ball = ball/sum(ball)
  ball = clip(ball)
  ball
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
mkbox = function(n=15) {
  matrix(1.0/(n*n),nc=n,nr=n)
}

