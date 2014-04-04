# Class Image, definition and methods

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

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Grayscale = 0L
Color     = 2L

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClass ("Image",
  representation (colormode="integer"),
  prototype (colormode=Grayscale),
  contains = "array"
)

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Image = function(data = array(0, dim=c(1,1)), dim, colormode) {
  if (missing(dim)) {
    if (is.array(data)) dim = base::dim(data)
    else dim = c(1, length(data))
  }
  else if (length(dim)<2) stop("The number of dimensions dim must be at least 2")
  
  if (missing(colormode)) {
    if (is.Image(data))
      colormode = colorMode(data)
    else 
      if (!is.character(data))
        colormode=Grayscale
  }
  else colormode = parseColorMode(colormode)
  
  if (is.character(data)) {
    datac = col2rgb(data)/255
    res = rgbImage(array(datac[1,,drop=FALSE], dim), array(datac[2,,drop=FALSE], dim), array(datac[3,,drop=FALSE], dim))
    if (!missing(colormode)) if (colormode==Grayscale) res = channel(res, 'gray')
  } 
  else res = new("Image", .Data = array(data, dim=dim), colormode=colormode)
  
  return(res)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
is.Image <- function (x) is(x, "Image")

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
as.Image = function(x) {
  if(is.Image(x))
    x
  else
    Image(x)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
colorMode = function (y) {
  if (is(y, 'Image')) y@colormode
  else Grayscale
}

`colorMode<-` = function(y, value) {
  if (is(y, 'Image')) {
    y@colormode = parseColorMode(value)
    validObject(y)
    y
  } 
  else warning('Color mode of an array cannot be changed, the array should be cast into an Image using \'Image\'')
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
imageData = function (y) {
  if (is(y, 'Image')) y@.Data
  else y
}

`imageData<-` = function (y, value) {
  if (is(y, 'Image')) {
    y@.Data = value
    validObject(y)
    y
  } 
  else value
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
validImageObject = function(object) {
  ## check colormode
  if (!is.integer(colorMode(object))) return('colormode must be an integer')
  if (colorMode(object)!=0 && colorMode(object)!=2) return('invalid colormode')

  ## check array
  if (!is.array(object)) return('object must be an array')

  ## check dim
  if (length(dim(object))<2) return('object must have at least two dimensions')
  if (getNumberOfFrames(object,'total')<1) return('Image must contain at least one frame')

  TRUE
}
setValidity("Image", validImageObject)

## Overloading binary operators
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("Ops", signature(e1="Image", e2="Image"),
	function(e1, e2) {
          e1@.Data=callGeneric(imageData(e1), imageData(e2))
          validObject(e1)
          return(e1)
	}
)
setMethod("Ops", signature(e1="Image", e2="numeric"),
	function(e1, e2) {
          e1@.Data=callGeneric(imageData(e1), e2)
          validObject(e1)
          return(e1)
	}
)
setMethod("Ops", signature(e1="numeric", e2="Image"),
	function(e1, e2) {
          e2@.Data=callGeneric(e1, imageData(e2))
          validObject(e2)
          return(e2)
	}
)

## private
## determines image type
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
determineFileType = function(files, type) {
  ## helper function 
  collapseTypeSynonyms = function(x){
    x = tolower(x)
    x[x=="tif"] = "tiff"
    x[x=="jpg"] = "jpeg"
    x
  }

  if (!(is.character(files)&&(length(files)>=1)))
    stop("Please supply at least one filename.")

  if (missing(type)) {
    type = unique(collapseTypeSynonyms(sapply(strsplit(files, split=".", fixed=TRUE), 
      function(x) {
        if (length(x)>1) 
          x[length(x)] 
        else if (length(x)==1)
          stop(sprintf("Unable to determine type of %s: Filename extension missing.", x)) 
        else 
          stop("Unable to determine type: Empty filename.") 
      }
    )))
    if (length(type)>1)
      stop(sprintf("File type implied by the file name extensions must be unique, but more than one type was found: %s.", paste(type, collapse=", ")))
  } else {
    if (!(is.character(type)&&(length(type)==1)))
      stop("'type' must be a character vector of length 1.")
    else 
      type = collapseTypeSynonyms(type)
  }
  return(type)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
readImage = function(files, type, all=TRUE, ...) {
  
  readURL = function(url, buffer=2^24){
    f = try(file(url, "rb"), silent=TRUE)
    if (inherits(f,"try-error")) 
      stop(attr(f,"condition")$message)
    
    rawData = bufData = NULL;
    
    while( length(bufData <- readBin(f, 'raw', buffer))>0 )
      rawData = c(rawData, bufData)
    
    try(close(f), silent=TRUE)
    
    rawData
  }
  
  type = try (determineFileType(files, type), silent=TRUE)
  if (inherits(type,"try-error")) 
    stop(attr(type,"condition")$message)
  
  readFun = switch(type,
                   tiff = function(x, ...) {
                     y = readTIFF(x, all=all, ...)
                     # make sure all frames have the same dimensions
                     if(length(y)>1)
                       if(!all(duplicated(lapply(y, dim))[-1]))
                         stop("Frame dimensions of a TIFF file are not equal")
                     y
                   },
                   jpeg = function(x, ...) readJPEG(x, ...),
                   png  = function(x, ...) readPNG(x, ...),
                   stop(sprintf("Invalid type: %s. Currently supported formats are JPEG, PNG, and TIFF.", type))
  )
  
  loadFun = function(i) {
    ## first look for local files
    if(!file.exists(i)){
      ## might still be a remote URL  
      w = options(warn=2)
      rawData = try(readURL(i), silent = TRUE)
      options(w) 
      if (inherits(rawData,"try-error")) {
        warning( paste(unlist(strsplit(attr(rawData,"condition")$message, "(converted from warning) ", fixed=TRUE)), sep="", collapse=""))
        return (NULL)
      }
      else
        ## is url
        return(readFun(rawData))
    }
    ## ensure that the file is not a directory
    else if (file.info(i)$isdir){
      warning(sprintf("Cannot open %s: Is directory.", i))
      return (NULL)
    }
    else
      ## appears to be a legit file
      return(readFun(i))
  }
  
  # flatten nested image list and remove null elements
  flatten <- function(x) {
    while(any(vapply(x, is.list, logical(1)))) {
      x <- lapply(x, function(x) if(is.list(x)) x else list(x))
      x <- unlist(x, recursive=FALSE) 
    }
    x[!vapply(x, is.null, logical(1))]
  }
  
  # stratify processing for single and multiple files to increase performance
  
  # single file
  if(length(files) == 1){
    y = loadFun(files)
  }
  
  #  multiple files
  else {
    y = lapply(files, loadFun)
    y = flatten(y)   
  }
  
  if(is.list(y)){
    if(length(y)==0) stop("Empty image stack.")
    
    # check whether image dimensions match
    if(!all(duplicated(lapply(y, dim))[-1]))
      stop("Images have different dimensions")
    
    y1 = y[[1]]
    channels = channelLayout(y1)
    if(length(y) == 1)
      y = y1
    else {
      y <- abind(y, along = length(dim(y1))+1)
      dimnames(y) = NULL
    }
    rm(y1)
  }
  else{
    channels = channelLayout(y)
  }
  
  y = transpose(y)
  
  new("Image", .Data = y, colormode = if(isTRUE(charmatch(channels,'G') == 1)) Grayscale else Color )
}

## private
## returns channel layout of a pixel array: G, GA, RGB, or RGBA
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
channelLayout = function(x){
  y = dim(x) 
  return( switch(if(length(y)==2) 1 else if (length(y)==3 && y[3]<=4) y[3] else 5, 'G', 'GA', 'RGB', 'RGBA', 'unknown') )
}

## private
## helper function used to check whether image data can be written on X bits without accuracy loss
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
isXbitImage = function(x, bits) {
  b = 2^bits - 1
  x = as.numeric(x)
  
  ## fast termination if not
  y = b * x[1] 
  if (trunc(y)!=y)
    FALSE
  else  {
    y = b * x
    all(trunc(y)==y)
  }
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
writeImage = function (x, files, type, quality=100L, bits.per.sample, compression='none', ...) {
  ## internal copy of getFrame which can operate on Images coerced to arrays, which is faster in combination with transpose(... coerce=TRUE)
  getFrameInternal = function(y, i, colormode) {
    ## frame dimensions
    len = length( (d = dim(y)) )
    fdim = ifelse (colormode==Color && len>2, 3, 2)
    if (len==fdim) return(y)
    
    x = asub(y, as.list(ind2sub(i, d[-1:-fdim])), (fdim+1):len)
    dim(x) = d[1:fdim]
    
    return(x)
  }
  
  validImage(x)
  
  type = try (determineFileType(files, type), silent=TRUE)
  if (inherits(type,"try-error")) 
    stop(attr(type,"condition")$message)
  
  ## automatic bits.per.sample guess
  if ( (type=='tiff') && missing(bits.per.sample) ) {
    if (isXbitImage(x, 8L)) 
      bits.per.sample = 8L 
    else 
      bits.per.sample = 16L
  }
  
  writeFun = switch(type,
                    tiff = function(x, file, ...) writeTIFF(x, file, bits.per.sample=bits.per.sample, compression=compression, ...),
                    jpeg = function(x, file, ...) writeJPEG(x, file, quality=quality/100, ...),
                    png  = function(x, file, ...) writePNG(x, file, ...),
                    stop(sprintf("Invalid type: %s. Currently supported formats are JPEG, PNG, and TIFF.", type))
  )
  
  if ((quality<1L) || (quality>100L))
    stop("'quality' must be a value between 1 and 100.")
  
  nf = getNumberOfFrames(x, type='render')
  lf = length(files)
  colormode = colorMode(x)
  
  if ( (lf!=1) && (lf!=nf) )
    stop(sprintf("Image contains %g frame(s) which is different from the length of the file name list: %g. The number of files must be 1 or equal to the size of the image stack.", nf, lf))
  
  else {
    frames = seq_len(nf)
    
    x = clipImage(x) ## clip the image and change storage mode to double
    x = transpose(x, coerce=TRUE)    
    
    if ( lf==1 && nf>1 ) {
      ## store all frames into a single TIFF file
      if (type=='tiff') {
        
        ## create list of image frames
        la = lapply(frames, function(i) getFrameInternal(x, i, colormode))
        
        if (nf==writeFun(la, files, ...))
          return(invisible(files))
        else
          stop(sprintf("Error writing file sequence to TIFF."))
      }
      ## generate file names for frames
      else {
        basename = unlist(strsplit(files, split=".", fixed=TRUE))
        prefix   = basename[-length(basename)]
        suffix   = basename[length(basename)]
        
        files = vapply(frames, function(i) paste0(paste0(prefix, collapse='.'), '-', i-1, '.', suffix), character(1))
      }
    }
    
    ## store image frames into individual files
    for (i in frames)
      writeFun(getFrameInternal(x, i, colormode), files[i], ...)
    return(invisible(files))
  }
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("[", signature(x="Image", i="ANY", j="ANY", drop="ANY"),
           function(x,i,j,...,drop) {
             ## list(...) doesn't work in this S4 method dispatch framework
             ## we are using the following trick:
             ## the current call is evaluated, but using x@.Data instead of x in the previous calling frame
             sc=sys.call()
             sc[[2]]=call('slot',sc[[2]],'.Data')
             z=eval.parent(sc)
             if (!is.array(z)) z=array(z,dim=c(length(z),1))
             x@.Data=z
             validObject(x)
             x
           })

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# convert linear index to array indices
ind2sub = function(x, y) {
  if(x > prod(y)) 
    stop("Index out of bounds")
  if ( (len=length(y)) == 0 )
    return(NULL)
  
  res = div = 1
  for(i in seq_len(len-1))
    div[i+1] = div[i] * y[i] 
  
  for(i in rev(seq_len(len))){
    res[i] = ceiling(x/div[i])
    x = x - (res[i]-1) * div[i]
  }
  
  stopifnot(length(res)==len)
  res
}

getFrame = function(y, i, type = c('total', 'render')) {
  type = match.arg(type)
  
  n = getNumberOfFrames(y, type = type)
  if (i<1 || i>n) stop("'i' must belong between 1 and ", n)
  
  ## frame dimensions
  len = length( (d = dim(y)) )
  fdim = ifelse (colorMode(y)==Color && type=='render' && len>2, 3, 2)
  if (len==fdim) return(y)
  
  x = asub(y, as.list(ind2sub(i, d[-1:-fdim])), (fdim+1):len)
  dim(x) = d[1:fdim]
  
  return(x)
}

## getNumberOfFrames
## If type='total', returns the total number of frames
## If type='render', return the number of frames to be rendered after color channel merging
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
getNumberOfFrames = function(y, type = c('total', 'render')) {
  type = match.arg(type)
  d = dim(y)
  if (type=='render' && colorMode(y)==Color) {
    if (length(d)< 3) return(1)
    else return(prod(d[-1:-3]))
  }
  else return(prod(d[-1:-2]))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("show", signature(object="Image"),
  function (object) {
    nd = dim(object)
    ld = length(nd)
    
    cat('Image\n')
    
    cat('  colormode:',c('Grayscale', NA, 'Color')[1+colorMode(object)],'\n')
    cat('  storage.mode:',storage.mode(object),'\n')
    cat('  dim:',nd,'\n')
    cat('  nb.total.frames:',getNumberOfFrames(object,'total'),'\n')
    cat('  nb.render.frames:',getNumberOfFrames(object,'render'),'\n')
    
    if (nd[1]>5) nd[1] = 5
    if (nd[2]>6) nd[2] = 6
    if (ld>2) nd[3:ld] = 1
    
    ndl = lapply(nd, seq_len)
    
    nds = paste0('[1:',nd[1],',1:',nd[2],paste(rep(',1',ld-2),collapse=''),']')
    
    cat('\nimageData(object)', nds, ':\n', sep='')
    print(asub(object@.Data, ndl))
    cat('\n')
    
    invisible(NULL)}
)

print.Image <- function(x,...) show(x)

## GP: Useful ?
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("image", signature(x="Image"),
  function(x, i, xlab = "", ylab = "", axes = FALSE, col=gray ((0:255) / 255), ...) {
    dimx <- dim (x)
    if ( missing(i) ) {
      if ( dimx[3] > 1 ) warning( "missing i for an image stack, assuming i=1" )
      i <- 1
    }
    i <- as.integer ( i[1] )
    if ( i < 1 || i > dimx[3] )
      stop( "index 'i' out of range" )
    if ( any(dimx == 0) )
      stop( "image size is zero, nothing to plot" )
    X <- 1:dimx[1]
    Y <- 1:dimx[2]
    Z <- imageData(x[,,i])[, rev(Y), 1, drop=TRUE]
    asp <- dimx[2] / dimx[1]
    graphics:::image (x=X, y=Y, z=Z, asp=1, col=col, axes=axes, xlab=xlab, ylab=ylab, ...)
  }
)

## private function to select a channel from a Color image
## failsafe, will return a black image if the channel doesn't exist
selectChannel = function(x, i) {
  if (colorMode(x)==Grayscale) stop("in 'selectChannel', color mode must be 'Color'")
  n = getNumberOfFrames(x, 'render')
  dim = dim(x)
  d = dim[1:2]
  if (n>1) d = c(d,n)
  y = NULL 
  
  if (length(dim)<3) {
    if (i==1) y = x
  } 
  else {
    if (i<=dim[3]) y = asub(x, i, 3)
  }
  
  if(is.null(y)) 
    y = new("Image", .Data = array(0, dim = d), colormode = Grayscale)
  else
    colorMode(y) = Grayscale
  
  y
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
channel = function (x, mode) {
  if (!is.array(x)) x=Image(x)
  if (is.character(x)) x=Image(x)
  mode=tolower(mode)
  validObject(x)
  if (colorMode(x)==Grayscale) {
    return(switch(mode,
                  rgb=rgbImage(red=x,green=x,blue=x),
                  grey=,gray=,luminance=x,
                  r=,red=stop('invalid conversion mode, can\'t extract the red channel from a \'Grayscale\' image'),
                  g=,green=stop('invalid conversion mode, can\'t extract the green channel from a \'Grayscale\' image'),
                  b=,blue=stop('invalid conversion mode, can\'t extract the blue channel from a \'Grayscale\' image'),
                  asred=rgbImage(red=x),
                  asgreen=rgbImage(green=x),
                  asblue=rgbImage(blue=x),
                  x11=array(rgb(x,x,x),dim=dim(x)),
                  stop('invalid conversion mode')
                  ))
  } else {
    return(switch(mode,
                  rgb=x,
                  ## Color->Grayscale conversion is done using 1/3 uniform RGB weights
                  grey=,gray=(selectChannel(x,1)+selectChannel(x,2)+selectChannel(x,3))/3,
                  luminance=0.2126*selectChannel(x,1)+0.7152*selectChannel(x,2)+0.0722*selectChannel(x,3),
                  r=,red=selectChannel(x,1),
                  g=,green=selectChannel(x,2),
                  b=,blue=selectChannel(x,3),
                  asred=selectChannel(x,1),
                  asgreen=selectChannel(x,2),
                  asblue=selectChannel(x,3),
                  x11=array(rgb(selectChannel(x,1),selectChannel(x,2),selectChannel(x,3)),dim=dim(selectChannel(x,1))),
                  stop('invalid conversion mode')
                  ))
  }
}

toRGB = function(x) {
  channel(x, "rgb")
}

## GP: Useful ?
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("hist", signature(x="Image"),
  function (x, breaks=64L, main=paste("Image histogram:", length(x), "pixels"), xlab="Intensity", ...) {

    if ( colorMode(x) != Grayscale ) {
      colores = c("red", "green", "blue")
      y = lapply(colores, function(m) imageData(channel(x, m)))
      names(y) = colores
    } else {
      y = list(black=imageData(x))
    }

    rg = range(unlist(y), na.rm=TRUE)
    if(length(breaks)==1L) {
      dr = (rg[2]-rg[1])/(breaks*2L+2L)
      breaks = seq(rg[1]-dr, rg[2]+dr, length=breaks+1L)
    }

    h = lapply(y, hist, breaks=breaks, plot=FALSE)
    px = sapply(h, "[[", "breaks")[-1L,,drop=FALSE]
    matplot(x = px + dr*(col(px)-ncol(px)/2)/2,
            y = sapply(h, "[[", "counts"), type="s", lty=1L,
            main=main, xlab=xlab, col=names(y), ylab="counts", ...)
  }
)

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
combineImages = function (x, y, ...) {
  if (!all(dim(x)[1:2]==dim(y)[1:2])) stop("images must have the same 2D frame size to be combined")
  
  ## merging along position guided by colorMode
  along = ifelse (colorMode(x)==Color && colorMode(y)==Color, 4, 3)
  
  ## add extra dimension in case of single frame Color Images
  if(along == 4) {
    if(colorMode(x)==Color && length((d = dim(x)))==2) dim(x) = c(d, 1)
    if(colorMode(y)==Color && length((d = dim(y)))==2) dim(y) = c(d, 1)
  }
  z = abind(x, y, along=along)
  dimnames(z) = NULL
  imageData(x) = z
    
  validObject(x)
  return (x)
}

## general method for the superclass of 'Image' and 'matrix'
setMethod("combine", signature("array", "array"), combineImages)
## explicit methods for subclasses of 'array'
setMethod("combine", signature("matrix", "matrix"), combineImages)
setMethod("combine", signature("Image", "Image"), combineImages)

## special case of combining a list of images
setMethod("combine", signature("list", "missing"), 
  function(x, y, ...) {
    names(x) <- NULL
    do.call(combine, x)
  }
)
          
## Median & quantile redefinition
## needed to overcome an isObject() test in median() which greatly slows down median()
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
median.Image <- function(x, na.rm = FALSE) {
  median(imageData(x), na.rm=na.rm)
}

quantile.Image <- function(x, ...) {
  quantile(imageData(x), ...)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rgbImage = function(red=NULL, green=NULL, blue=NULL) {
  compareDims = function(x, y) {
    if (!identical(x, y)) stop('images must have the same 2D frame size and number of frames to be combined')
  }
 
  d = NULL
  
  if (!is.null(red)) {
    d = dim(red)
  }
  if (!is.null(green)) {
    if (is.null(d))
      d = dim(green)
    else
      compareDims(d, dim(green))
  }
  if (!is.null(blue)) {
    if (is.null(d))
      d = dim(blue)
    else
      compareDims(d, dim(blue))
  }

  if (is.null(d)) stop('at least one non-null array must be supplied')
  
  if (is.null(red)) red = array(0, dim=d)
  if (is.null(green)) green = array(0, dim=d)
  if (is.null(blue)) blue = array(0, dim=d)

  x = abind(red, green, blue, along=2.5)
  dimnames(x) = NULL
  
  new("Image", .Data = x, colormode = Color)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
parseColorMode = function(colormode) {
  icolormode=NA

  if (is.numeric(colormode)) {
    icolormode=colormode
  } else if (is.character(colormode)) {
    icolormode=pmatch(tolower(colormode),c('grayscale', NA, 'color'), duplicates.ok=TRUE,nomatch=NA)-1
  }

  as.integer(icolormode)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## returns the raster representation of an image (by default the first frame)
as.raster.Image = function(y, i = 1) {
  x = getFrame(y, i, type='render')
  x = clipImage(x)
  ## get image data with swapped XY dimensions
  x = transpose(x, coerce = TRUE)
  ## the actual raster representation
  as.raster(x)
}
