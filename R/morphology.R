thresh = function (x, w=5, h=5, offset=0.01) {
  validImage(x)
  if (w<2 || h<2) stop ("width 'w' and height 'h' must be larger than 1")
  return ( .Call("thresh", castImage(x), as.numeric( c(w, h, offset) ), PACKAGE='EBImage') )
}

distmap = function (x, metric=c('euclidean', 'manhattan')) {
  validImage(x)
  if (any(is.na(x))) stop("'x' shouldn't contain any NAs")
  metric = match.arg(metric)
  imetric = switch(metric,euclidean=0,manhattan=1)
  return (.Call("distmap", castImage(x), as.integer(imetric), PACKAGE='EBImage'))
}

makeBrush = function(size, shape=c('box', 'disc', 'diamond', 'Gaussian', 'line'), step=TRUE, sigma=0.3, angle=45) {
  if(! (is.numeric(size) && (length(size)==1L) && (size>=1)) ) stop("'size' must be an odd integer.")
  shape = match.arg(tolower(shape))
  
  if(size %% 2 == 0){
    size = size + 1
    warning(paste("'size' was rounded to the next odd number: ", size))
  }
  
  if (shape=='box') z = array(1,dim=c(size,size))
  else if (shape == 'line') {
    angle = angle %% 180
    angle.radians = angle * pi / 180;
    z.y = ceiling(abs(cos(angle.radians) * size))
    z.x = ceiling(abs(sin(angle.radians) * size))
    if (z.y == 0)
      z.y = 1
    if (z.x == 0)
      z.x = 1
    z = array(0, dim=c(z.y, z.x));
    for (i in 1:size) {
      i.y = ceiling(cos(angle.radians) * i)
      i.x = ceiling(sin(angle.radians) * i)
      if (i.y < 0)
        i.y = z.y + i.y
      if (i.x < 0)
        i.x = z.x + i.x
      if (i.y == 0)
        i.y = 1
      if (i.x == 0)
        i.x = 1
      z[i.y, i.x] = 1
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
  return (.Call("lib_erode_dilate", castImage(x), kern, as.integer(0), PACKAGE='EBImage') )
}

dilate = function (x, kern=makeBrush(5, shape='diamond')) {
  validImage(x)
  return (.Call("lib_erode_dilate", castImage(x), kern, as.integer(1), PACKAGE='EBImage') )
}

opening = function (x, kern=makeBrush(5, shape='diamond')) {
  validImage(x)
  dilate(erode(x, kern), kern)
}

closing = function (x, kern=makeBrush(5, shape='diamond')) {
  validImage(x)
  erode(dilate(x, kern), kern)
}

erodeGreyScale = function (x, kern=makeBrush(5, shape='diamond')) {
    validImage(x)
    return (.Call("lib_erode_dilate_greyscale", castImage(x), kern, as.integer(0), PACKAGE='EBImage') )
}

dilateGreyScale = function (x, kern=makeBrush(5, shape='diamond')) {
    validImage(x)
    return (.Call("lib_erode_dilate_greyscale", castImage(x), kern, as.integer(1), PACKAGE='EBImage') )
}

openingGreyScale = function (x, kern=makeBrush(5, shape='diamond')) {
    validImage(x)
    return (.Call("lib_opening_closing_greyscale", castImage(x), kern, as.integer(0), PACKAGE='EBImage') )
}

closingGreyScale = function (x, kern=makeBrush(5, shape='diamond')) {
    validImage(x)
    return (.Call("lib_opening_closing_greyscale", castImage(x), kern, as.integer(1), PACKAGE='EBImage') )
}

whiteTopHatGreyScale = function (x, kern=makeBrush(5, shape='diamond')) {
    validImage(x)
    return (.Call("lib_tophat_greyscale", castImage(x), kern, as.integer(0), PACKAGE='EBImage') )
}

blackTopHatGreyScale = function (x, kern=makeBrush(5, shape='diamond')) {
    validImage(x)
    return (.Call("lib_tophat_greyscale", castImage(x), kern, as.integer(1), PACKAGE='EBImage') )
}

selfcomplementaryTopHatGreyScale = function (x, kern=makeBrush(5, shape='diamond')) {
    validImage(x)
    return (.Call("lib_tophat_greyscale", castImage(x), kern, as.integer(2), PACKAGE='EBImage') )
}
