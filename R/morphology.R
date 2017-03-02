thresh = function (x, w=5, h=5, offset=0.01) {
  validImage(x)
  if (w<1 || h<1) 
    stop ("filter width 'w' and height 'h' must be larger than 1")
  
  d = dim(x)[1:2]
  d = (d-1)/2
  if (w > d[1L])
    stop ("filter width exceeds picture width")
  if (h > d[2L])
    stop ("filter height exceeds picture height")
  
  return ( .Call(C_thresh, castImage(x), as.numeric( c(w, h, offset) )) )
}

distmap = function (x, metric=c('euclidean', 'manhattan')) {
  validImage(x)
  if (any(is.na(x))) stop("'x' shouldn't contain any NAs")
  metric = match.arg(metric)
  return (.Call(C_distmap, castImage(x), switch(metric, euclidean=0L, manhattan=1L)))
}

makeBrush = function(size, shape=c('box', 'disc', 'diamond', 'Gaussian', 'line'), step=TRUE, sigma=0.3, angle=45) {
  if(! (is.numeric(size) && (length(size)==1L) && (size>=1)) ) stop("'size' must be an odd integer.")
  shape = match.arg(arg = tolower(shape), choices = c('box', 'disc', 'diamond', 'gaussian', 'line'))

  if(size %% 2 == 0){
    size = size + 1
    warning(paste("'size' was rounded to the next odd number: ", size))
  }
  
  if (shape=='box') z = array(1,dim=c(size,size))
  else if (shape == 'line') {
    angle = angle %% 180
    angle.radians = angle * pi / 180;
    tg = tan(angle.radians)
    sizeh = (size-1)/2
    if ( angle < 45 || angle > 135) {
      z.x = sizeh
      z.y = round(sizeh*tg)
    }
    else {
      z.y = sizeh
      z.x = round(sizeh/tg)
    }
    z = array(0, dim=2*c(z.x, z.y)+1);
    for (i in -sizeh:sizeh) {
      if ( angle < 45 || angle > 135) {
        ## scan horizontally
        i.x = i
        i.y = round(i*tg)
      }
      else {
        ## scan vertically
        i.y = i
        i.x = round(i/tg) 
      }
      z[i.x+z.x+1, i.y+z.y+1] = 1
    }
  }
  else if (shape=='gaussian') {
    x = seq(-(size-1)/2, (size-1)/2, length=size)
    x = matrix(x, nrow=size, ncol=size)
    z = exp(- (x^2 + t(x)^2) / (2*sigma^2))
    z = z / sum(z)
  } else {
    ## pixel center coordinates
    x = 1:size -((size+1)/2)
    
    ## for each pixel, compute the distance from its center to the origin, using L1 norm ('diamond') or L2 norm ('disc')
    if (shape=='disc') {
      z = outer(x, x, FUN=function(X,Y) (X*X+Y*Y))
      mz = (size/2)^2
      z = (mz - z)/mz
      z = sqrt(ifelse(z>0, z, 0))
    } else {
      z = outer(x, x, FUN=function(X,Y) (abs(X)+abs(Y)))
      mz = (size/2)
      z = (mz - z)/mz
      z = ifelse(z>0, z, 0)
    }

    if (step) z = ifelse(z>0, 1, 0)
  }
  z
}

erode = function (x, kern=makeBrush(5, shape='diamond')) {
  validImage(x)
  return (.Call(C_erode_dilate, castImage(x), kern, 0L))
}

dilate = function (x, kern=makeBrush(5, shape='diamond')) {
  validImage(x)
  return (.Call(C_erode_dilate, castImage(x), kern, 1L))
}

opening = function (x, kern=makeBrush(5, shape='diamond')) {
  validImage(x)
  return (.Call(C_opening_closing, castImage(x), kern, 0L))
}

closing = function (x, kern=makeBrush(5, shape='diamond')) {
  validImage(x)
  return (.Call(C_opening_closing, castImage(x), kern, 1L))
}

whiteTopHat = function (x, kern=makeBrush(5, shape='diamond')) {
  validImage(x)
  return (.Call(C_tophat, castImage(x), kern, 0L))
}

blackTopHat = function (x, kern=makeBrush(5, shape='diamond')) {
  validImage(x)
  return (.Call(C_tophat, castImage(x), kern, 1L))
}

selfComplementaryTopHat = function (x, kern=makeBrush(5, shape='diamond')) {
  validImage(x)
  return (.Call(C_tophat, castImage(x), kern, 2L))
}
