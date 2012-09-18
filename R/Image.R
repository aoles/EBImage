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
Image = function(data=array(0, dim=c(1,1)), dim, colormode=NULL) {
  if (missing(dim)) {
    if (is.array(data)) dim=base::dim(data)
    else dim=c(1,length(data))
  }
  ld = length(dim)
  if (ld<2) stop(sprintf("length(dim) must be at least 2 and is %d.", ld))

  if (is.null(colormode)) {
    if (is.Image(data)) colormode=colorMode(data)
    else if (is.character(data)) colormode=NULL
    else colormode=Grayscale
  } else colormode=EBImage:::parseColorMode(colormode)

  if (is.character(data)) {
    datac = col2rgb(data)/255
    res = rgbImage(Image(datac[1,,drop=FALSE], dim=dim[1:2]),  Image(datac[2,,drop=FALSE], dim=dim[1:2]),  Image(datac[3,,drop=FALSE], dim=dim[1:2]))
    if (!is.null(colormode)) if (colormode==Grayscale) res = channel(res, 'gray')
  } else {
    res = new("Image", .Data=array(data,dim=dim), colormode=colormode)
  }

  validObject(res)
  return(res)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
as.Image = function(x) {
  x = Image(x, colormode=Grayscale)
  validImage(x)
  return(x)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
colorMode = function (y) {
  if (is(y, 'Image')) y@colormode
  else Grayscale
}

`colorMode<-` = function(y, value) {
  if (is(y, 'Image')) {
    y@colormode = EBImage:::parseColorMode(value)
    validObject(y)
  } else warning('Color mode of an array cannot be changed, the array should be cast into an Image using \'Image\'')
  return(y)
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
  } else return(value)
  return (y)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
is.Image <- function (x) {
  if (!is(x, "Image")) return(FALSE)
  else return (TRUE)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
validImageObject = function(object) {
  ## check colormode
  if (!is.integer(colorMode(object))) return('colormode must be an integer')
  if (colorMode(object)<0 | colorMode(object)>2) return('invalid colormode')

  ## check array
  if (!is.array(object)) return('object must be an array')

  ## check dim
  d=dim(object)
  if (length(d)<2) return('object must have at least two dimensions')
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
    tiff = function(x, ...) readTIFF(x, all=all, ...),
    jpeg = function(x, ...) readJPEG(x, ...),
    png  = function(x, ...) readPNG(x, ...),
    stop(sprintf("Invalid type: %s. Currently supported formats are JPEG, PNG, and TIFF.", type))
    )

  stack = NULL

  for(i in seq_along(files)) {
    ## first look for local files
    if(!file.exists(files[i])){
      ## might still be a remote URL  
      w = options(warn=2)
      rawData = try(readURL(files[i]), silent = TRUE)
      options(w) 
      if (inherits(rawData,"try-error")) {
        warning( paste(unlist(strsplit(attr(rawData,"condition")$message, "(converted from warning) ", fixed=TRUE)), sep="", collapse=""))
        next
      }
      else
      ## is url
        img = readFun(rawData)
    }
    ## ensure that the file is not a directory
    else if (file.info(files[i])$isdir){
      warning(sprintf("Cannot open %s: Is directory.", files[i]))
      next
    }
    else
    ## appears to be a legit file
      img = readFun(files[i])

    ## readTIFF returns a list for stacked images.
    nf = if(is.list(img)) length(img) else 1

    for(j in seq_len(nf)) {
      frame = if(is.list(img)) img[[j]] else img

      ## AO: Discard alpha channel in Greyscale images to maintain compatibility with the previous version
#       if (channelLayout(frame) == 'GA')
#         frame = frame[,,1]

      ## fix image layout based on the first file (& frame)
      if (is.null(stack)) {
        refName = if (nf>1) paste(files[i], j, sep=",") else files[i]
        dim = dim(frame)
        channels = channelLayout(frame)
#         if (channels == 'unknown')
#           stop(sprintf("%s: Unsupported channel layout,", refName))

        stack = frame
      }
      else {
        if ( identical(dim, dim(frame)) )
          stack = abind(stack, frame, along=length(dim)+1)
        else if (!identical(dim[1:2], dim(frame)[1:2]))
          stop(sprintf("%s: Image size (%s) does not match reference size (%s) of %s. All images need to have the same width and height.", if (nf>1) paste(files[i], j, sep=",") else files[i], paste(dim(frame)[1:2], collapse=" x "), paste(dim[1:2], collapse=" x "), refName ))
        else
          stop(sprintf("%s: Channel layout (%s) does not match reference channel layout (%s) of %s. All images need to have the same number of channels.", if (nf>1) paste(files[i], j, sep=",") else files[i], channelLayout(frame), channels, refName ))
      }
    }
  }

  if (is.null(stack))
    stop("Empty image stack.")
  else
  ## perform image rotation by swapping the XY dimensions
  Image(swapXY(stack), colormode = if(isTRUE(charmatch(channels,'G') == 1)) 'Grayscale' else 'Color' )
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
        y = (2^bits - 1) * as.numeric(x)[1] ## fast termination if not
        if (trunc(y)==y)
          y = (2^bits - 1) * x
	all(trunc(y)==y)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
writeImage = function (x, files, quality=100, type, bits.per.sample, compression='none', ...) {
  validImage(x)

  type = try (determineFileType(files, type), silent=TRUE)
  if (inherits(type,"try-error")) 
      stop(attr(type,"condition")$message)

  ## automatic bits.per.sample guess
  if ( (type=='tiff') && missing(bits.per.sample) )
    if (isXbitImage(x, 8L)) 
      bits.per.sample = 8L 
    else 
      bits.per.sample = 16L

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

  if ( (lf!=1) && (lf!=nf) )
    stop(sprintf("Image contains %g frame(s) which is different from the length of the file name list: %g. The number of files must be 1 or equal to the size of the image stack.", nf, lf))
  
  else {
    x = castImage(x) ## if needed change storage mode to double 

    if ( lf==1 && nf>1 ) {
      ## store all frames into a single TIFF file
      if (type=='tiff') {
	x = swapXY(x, keepClass = FALSE)
        dims = dim(x)
        ndim = length(dims)

        ## create list of image frames
        if (ndim==3)
          la = lapply(seq_len(dims[ndim]), function(y) x[,,y])
        else
          la = lapply(seq_len(dims[ndim]), function(y) x[,,,y])

        if (nf==writeFun(la, files, ...))
          return(invisible(files))
        else
          stop(sprintf("Error writing file sequence to TIFF."))
      }
      ## generate frame file names
      else {
        basename = unlist(strsplit(files, split=".", fixed=TRUE))
        prefix   = basename[-length(basename)]
        suffix    = basename[length(basename)]
        for(i in seq_len(nf))
          files[i] = paste(paste(prefix, collapse='.'), '-', i-1, '.',suffix, sep='')
      }
    }

    ## store image frames into individual files
    for (i in seq_len(nf))
      writeFun(swapXY(getFrame(x, i, type='render'), keepClass = FALSE), files[i], ...)
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
getFrame = function(y, i, type='total') {
  n = getNumberOfFrames(y, type=type)
  if (i<1 || i>n) stop("'i' must belong between 1 and ", n)
  if (type=='render' && colorMode(y)==Color) {
    if (length(dim(y))==2) nchannels = 1
    else nchannels = dim(y)[3]
    dim(y) = c(dim(y)[1:2], nchannels, n)
    return(y[,,,i])
  }
  else {
    dim(y) = c(dim(y)[1:2], n)
    return(y[,,i])
  }
}

## getNumberOfFrames
## If type='total', returns the total number of frames
## If type='render', return the number of frames to be rendered after color channel merging
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
getNumberOfFrames = function(y, type='total') {
  if (missing(type)) type='total'
  if (type=='render' && colorMode(y)==Color) {
    if (length(dim(y))< 3) return(1)
    else return(prod(dim(y)[-1:-3]))
  }
  else return(prod(dim(y)[-1:-2]))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("show", signature(object="Image"),
  function (object) {
    cat('Image\n')

    valid=validObject(object,test=TRUE)
    if (!is.logical(valid)) valid=paste(FALSE,', ',valid,sep='')

    cat('  colormode:',c('Grayscale', NA, 'Color')[1+colorMode(object)],'\n')
    cat('  storage.mode:',storage.mode(object),'\n')
    cat('  dim:',dim(object),'\n')
    cat('  nb.total.frames:',getNumberOfFrames(object,'total'),'\n')
    cat('  nb.render.frames:',getNumberOfFrames(object,'render'),'\n')

    nd=dim(object)
    if (nd[1]>5) nd[1]=5
    if (nd[2]>6) nd[2]=6
    if (length(nd)>2) nd[3:length(nd)]=1
    ndl=lapply(nd,function(x) 1:x)
    nds=paste('[1:',nd[1],',1:',nd[2],paste(rep(',1',length(nd)-2),collapse=''),']',sep='')

    cat('\nimageData(object)',nds,':\n',sep='')
    print(do.call('[',c(list(object@.Data),ndl)))
    cat('\n')

    invisible(NULL)
  }
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
selectChannel=function(x,i) {
  if (colorMode(x)==Grayscale) stop("in 'selectChannel', color mode must be 'Color'")
  n=getNumberOfFrames(x,'render')
  d=dim(x)[1:2]
  if (n>1) d=c(d,n)
  black=Image(0,dim=d)
  if (length(dim(x))<3) {
    if (i==1) y=x
    else y=black
  } else {
    if (i<=dim(x)[3]) {
       nd=as.list(rep(T,length(dim(x))))
       nd[[3]]=i
       y=do.call('[',c(list(x),nd))
     }
    else y=black
  }
  colorMode(y)=Grayscale
  return(y)
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
                  grey=,gray=x,
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
                  grey=,gray=(EBImage:::selectChannel(x,1)+EBImage:::selectChannel(x,2)+EBImage:::selectChannel(x,3))/3,
                  r=,red=EBImage:::selectChannel(x,1),
                  g=,green=EBImage:::selectChannel(x,2),
                  b=,blue=EBImage:::selectChannel(x,3),
                  asred=EBImage:::selectChannel(x,1),
                  asgreen=EBImage:::selectChannel(x,2),
                  asblue=EBImage:::selectChannel(x,3),
                  x11=array(rgb(selectChannel(x,1),selectChannel(x,2),selectChannel(x,3)),dim=dim(selectChannel(x,1))),
                  stop('invalid conversion mode')
                  ))
  }
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
combine = function (x,...,along) {
  if (is.list(x)) {
    names(x) <- NULL
    do.call(combine, x)
  }
  else {
    args=c(list(x),list(...))
    if (length(args)==1) return(x)

    ## check dim[1:2]
    dm=sapply(args,function(z) dim(z)[1:2])
    dmx=dm[1,]==dm[1,1]
    dmy=dm[2,]==dm[2,1]
    if (!all(dmx) || !all(dmy)) stop("images must have the same 2D frame size to be combined")

    ## merging along position guided by colorMode
    ## if along is rationnal, a new dimension is created
    if (missing(along)) {
      y=args[[2]]
      if (colorMode(x)==Color) {
        if (colorMode(y)==Color) along=4
        else along=3
      } else along=3
    }

    z=abind(args,along=along)
    dimnames(z)=NULL
    imageData(x)=z

    validObject(x)
    return (x)
  }
}

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
  d=NULL

  if (!is.null(red)) {
    red = Image(red, colormode=Grayscale)
    d = dim(red)
  }
  if (!is.null(green)) {
    green = Image(green, colormode=Grayscale)
    d=dim(green)
  }
  if (!is.null(blue)) {
    blue = Image(blue, colormode=Grayscale)
    d=dim(blue)
  }

  if (is.null(red)) red=Image(0, dim=d)
  if (is.null(green)) green=Image(0, dim=d)
  if (is.null(blue)) blue=Image(0, dim=d)

  if (is.null(d)) stop('at least one non-null Image object must be specified')

  x=combine(red, green, blue, along=2.5)

  ## Cast to Color Image if x is an array
  if (class(x)=='array') x=Image(x)

  colorMode(x)=Color
  x
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
parseColorMode = function(colormode) {
  icolormode=NA

  if (is.numeric(colormode)) {
    ## warning('deprecated: colormode should be specified through a character string: \'grayscale\' or \'color\'')
    icolormode=colormode
  } else if (is.character(colormode)) {
    icolormode=pmatch(tolower(colormode),c('grayscale', NA, 'color'), duplicates.ok=TRUE,nomatch=NA)-1
  }

  as.integer(icolormode)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## returns the raster representation of an image (by default the first frame)
as.raster.Image = function(y, i=1) {
  f = getFrame(y, i, type='render')
  f[f<0] = 0	
  f[f>1] = 1  
  ## get image data with swapped XY dimensions
  a = swapXY(f, keepClass = FALSE)
  ## the actual raster representation
  as.raster(a)
}
