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
  if ( !missing(dim) && length(dim)<2 )
    stop("The number of dimensions dim must be at least 2")
  
  setdim = function(data) {
    if (is.array(data))
      base::dim(data)
    else 
      c(1L, length(data))
  }
  
  ## special character case
  if (is.character(data)) {
    colormode = 
      if (missing(colormode)) 
        Color
      else
        parseColorMode(colormode)
    
    if (missing(dim))
      dim = setdim(data)
      
    if ( colormode==Color )
      dim = c(dim[1:2], 3L, dim[-(1:2)])  
    
    dimnames = dimnames(data)
    data = col2rgb(data)/255
    
    if ( colormode==Color ) {
      channels = if (length(dim)<3L) 1L else seq_len(dim[3L])
      data = abind::abind(lapply(channels, function(ch) {
        array( if (ch>3L) 0 else data[ch,,drop=FALSE], dim[-3L], dimnames)
      }), along = 2.5)
      # replace a list of NULLs by the original NULL
      if(is.null(dimnames)) dimnames(data) = NULL
    }
    else
      data = array(data = (data[1,,drop=FALSE] + data[2,,drop=FALSE] + data[3,,drop=FALSE]) / 3, dim = dim, dimnames = dimnames)
  }
  ## default numeric case
  else {
    if (missing(dim))
      dim = setdim(data)
    
    colormode = 
      if (missing(colormode))
        if (is.Image(data))
          colorMode(data)
        else
          Grayscale
      else 
        parseColorMode(colormode)
  }
  
  return( new("Image", 
    .Data = 
      ## improve performance by not calling array constructor on well formed arrays
      if( is.array(data) && prod(dim)==length(data) ) {
        if( length(dim(data)) != length(dim) || any(dim(data) != dim))
          dim(data) = dim
        data
      }
      else {
        array(data, dim = dim)
      },
    colormode = colormode
  ))    
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
is.Image <- function (x) is(x, "Image")

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
as.Image <- function(x) UseMethod("as.Image")

as.Image.Image = function(x) {
  ## true Image
  if ( class(x)=="Image" )
    x
  ## coerce subclasses to Image superclass
  else
    as(x, "Image")
}

as.Image.default = function(x) Image(x)

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## define method for the S3 generic 'as.array'
as.array.Image = function(x, ...) x@.Data

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
  .Call(C_validImageObject, object) 
}
setValidity("Image", validImageObject)

## Overloading binary operators
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("Ops", signature(e1="Image", e2="Image"),
	function(e1, e2) {
          e1@.Data = callGeneric(e1@.Data, e2@.Data)
          validObject(e1)
          return(e1)
	}
)
setMethod("Ops", signature(e1="Image", e2="numeric"),
	function(e1, e2) {
          e1@.Data = callGeneric(e1@.Data, e2)
          validObject(e1)
          return(e1)
	}
)
setMethod("Ops", signature(e1="numeric", e2="Image"),
	function(e1, e2) {
          e2@.Data = callGeneric(e1, e2@.Data)
          validObject(e2)
          return(e2)
	}
)

## legacy code addressing broken 'Math2' S4GroupGenerics in R 3.1.2
setMethod("Math2", "Image",
          function(x, digits) {
            x@.Data <- callGeneric(x = x@.Data, digits = digits)
            x
          }
)

## explicit method definition for 'log' needed because of the extra formal argument, see ?Math
setMethod("log", signature(x="Image"),
          function(x, base) {
            x@.Data <- callGeneric(x = x@.Data, base = base)
            x
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
readImage = function(files, type, all=TRUE, names = sub("\\.[^.]*$", "", basename(files)), ...) {
  
  .readURL = function(url, buffer=2^24){
    f = try(file(url, "rb"), silent=TRUE)
    if (inherits(f,"try-error")) 
      stop(attr(f,"condition")$message)
    
    rawData = bufData = NULL;
    
    while( length(bufData <- readBin(f, 'raw', buffer)) > 0L )
      rawData = c(rawData, bufData)
    
    try(close(f), silent=TRUE)
    
    rawData
  }
  
  type = try (determineFileType(files, type), silent=TRUE)
  if (inherits(type,"try-error")) 
    stop(attr(type,"condition")$message)
  
  .readFun = switch(type,
                    tiff = function(x, ...) {
                      y = readTIFF(x, all = all, ...)
                      if ( (l=length(y)) > 1L) {
                        if (!is.null(names)) names(y) = seq_len(l)
                        # make sure all frames have the same dimensions
                        if(!all(duplicated.default(lapply(y, dim))[-1L]))
                          stop(sprintf("Frame dimensions of the '%s' file are not equal.", x))
                      }
                      y
                    },
                    jpeg = function(x, ...) readJPEG(x, ...),
                    png  = function(x, ...) readPNG(x, ...),
                    stop(sprintf("Invalid type: %s. Currently supported formats are JPEG, PNG, and TIFF.", type))
  )
  
  .loadFun = function(i, ...) {
    ## first look for local files
    if(!file.exists(i)){
      ## might still be a remote URL  
      w = options(warn=2)
      i = try(.readURL(i), silent = TRUE)
      options(w)
      ## is not URL
      if (inherits(i,"try-error")) {
        warning( sub("(converted from warning) ", "", attr(i,"condition")$message, fixed = TRUE) )
        return(NULL)
      }
    }
    ## ensure that the file is not a directory
    else if (file.info(i)$isdir){
      warning(sprintf("Cannot open %s: Is directory.", i))
      return(NULL)
    }
    
    return(.readFun(i, ...))
  }
  
  # flatten nested image list and remove null elements
  .flatten <- function(x) {
    while(any(vapply(x, is.list, logical(1L)))) {
      x <- lapply(x, function(x) if(is.list(x)) x else list(x))
      x <- unlist(x, recursive = FALSE) 
    }
    x[!vapply(x, is.null, logical(1L))]
  }
  
  # stratify processing for single and multiple files to increase performance
  
  # single file
  if(length(files) == 1L){
    y = .loadFun(files, ...)
  }
  
  #  multiple files
  else {
    y = lapply(files, .loadFun, ...)
    names(y) = names
    y = .flatten(y)   
  }
  
  if(is.list(y)){
    if(length(y)==0L) stop("Empty image stack.")
    
    # check whether image dimensions match
    if(!all(duplicated.default(lapply(y, dim))[-1L]))
      stop("Images have different dimensions")
    
    y1 = y[[1L]]
    channels = channelLayout(y1)
    if(length(y) == 1L)
      y = y1
    else
      y = vapply(y, identity, FUN.VALUE=y1)
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
  
  nf = numberOfFrames(x, type='render')
  lf = length(files)
  
  if ( (lf!=1) && (lf!=nf) )
    stop(sprintf("Image contains %g frame(s) which is different from the length of the file name list: %g. The number of files must be 1 or equal to the size of the image stack.", nf, lf))
  
  else {
    frames = seq_len(nf)
    
    x = clipImage(x) ## clip the image and change storage mode to double
    x = transpose(x)    
    
    if ( lf==1 && nf>1 ) {
      ## store all frames into a single TIFF file
      if (type=='tiff') {
        
        ## create list of image frames
        la = lapply(frames, function(i) getFrame(x, i, 'render'))
        
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
      writeFun(getFrame(x, i, 'render'), files[i], ...)
    return(invisible(files))
  }
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("[", "Image",
           function(x, i , j, ..., drop = TRUE) {
             ## list(...) doesn't work in this S4 method dispatch framework we
             ## are using the following trick: the current call is evaluated,
             ## but using x@.Data instead of x in the previous calling frame
             sc = sys.call()
             args = as.list(sc[-c(1L, 2L)])
             numIndices = length(args) - !is.null(args$drop)
             
             # when subsetting with single index treat as array
             if (numIndices == 1L) {
               callNextMethod()
             }
             else {
               # subset image array without dropping dimensions in order to
               # preserve spatial dimensions
               sc$drop = FALSE
               sc[[2L]] = call('slot', sc[[2L]], '.Data')
               y = eval.parent(sc)
                              
               # drop dims higher than 2 unless 'drop' explicitly set to FALSE
               if(!isTRUE(args$drop==FALSE) && length( (d = dim(y)) ) > 2L){
                 dims = which(d==1L)
                 y = adrop(y, drop = dims[dims>2L]) 
               }
               
               x@.Data = y
               validObject(x)
               x
             }
           })

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
getFrame = function(y, i, type = c('total', 'render')) {
    type = match.arg(type)
    
    n = numberOfFrames(y, type)
    if (i<1 || i>n) stop("'i' must belong between 1 and ", n)
    
    # return the argument if no subsetting neccessary
    ld = length(dim(y))
    fd = if (colorMode(y)==Color && type=='render' && ld>2L) 3L else 2L
    if (ld==fd) return(y)
    
    type = switch(type, total = 0L, render = 1L)
    
    .Call(C_getFrame, y, as.integer(i), type)
}

getFrames = function(y, i, type = c('total', 'render')) {
  type = match.arg(type)
  
  n = numberOfFrames(y, type)
  
  if ( missing(i) ) 
    i = seq_len(n)
  else {
    i = as.integer(i)
    if ( any( i<1L || i>n ) ) stop("'i' must be a vector of numbers ranging from 1 to ", n)
  }
  
  type = switch(type, total = 0L, render = 1L)
  
  .Call(C_getFrames, y, i, type)
}


## numberOfFrames
## If type='total', returns the total number of frames
## If type='render', return the number of frames to be rendered after color channel merging
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
numberOfFrames = function(y, type = c('total', 'render')) {
    type = match.arg(type)
    type = switch(type, total = 0L, render = 1L)
    .Call(C_numberOfFrames, y, type)
}

numberOfChannels = function(y, d = dim(y), cm = colorMode(y)) {
  if ( cm==Grayscale || is.na(d[3L]) ) 1L else d[3L]
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
showImage = function (object, short=FALSE) {
  nd = dim(object)
  ld = length(nd)
  dimorder = names(dimnames(object))
  
  cat(class(object)[1],'\n')
  
  cat('  colorMode    :',c('Grayscale', NA, 'Color')[1+colorMode(object)],'\n')
  cat('  storage.mode :',typeof(object),'\n')
  cat('  dim          :',nd,'\n')
  if ( !is.null(dimorder) )
  cat('  dimorder     :',dimorder,'\n')
  cat('  frames.total :',numberOfFrames(object,'total'),'\n')
  cat('  frames.render:',numberOfFrames(object,'render'),'\n')
  
  if ( !isTRUE(short) ) {
    if (nd[1]>5) nd[1] = 5
    if (nd[2]>6) nd[2] = 6
    if (ld>2) nd[3:ld] = 1
    
    ndl = lapply(nd, seq_len)
    
    nds = paste0('[1:',nd[1],',1:',nd[2],paste(rep(',1',ld-2),collapse=''),']')
    
    cat('\nimageData(object)', nds, '\n', sep='')
    print.default(asub(object@.Data, ndl))
  }
  invisible()
}

setMethod ("show", signature(object = "Image"), function(object) showImage(object))

print.Image <- function(x, ...) showImage(x, ...)

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("image", signature(x="Image"),
  function(x, i, xlab = "", ylab = "", axes = FALSE, col = gray((0:255) / 255), useRaster = TRUE, ...) {
    nf = numberOfFrames(x, type="total")
    
    if ( missing(i) ) {
      i = 1L
      if ( nf > 1L ) message( "Missing frame index for an image stack, assuming 'i = 1'")
    }
    else {
      i = as.integer(i[1L])
      if ( i<1L || i>nf ) stop( "Frame index out of range: 'i' must be between 1 and ", nf)
    }
    
    d <- dim(x)
    X <- 1:d[1L]
    Y <- 1:d[2L]
    Z <- getFrame(x, i, "total")
    image.default(x=X, y=Y, z=Z, asp=1, col=col, axes=axes, xlab=xlab, ylab=ylab, useRaster=useRaster, ...)
  }
)

## private function to select a channel from a Color image
## failsafe, will return a black image if the channel doesn't exist
selectChannel = function(x, i) {
  if (colorMode(x)==Grayscale) stop("in 'selectChannel', color mode must be 'Color'")
  
  dim = dim(x)
  y = NULL 
  
  if (length(dim) < 3) {
    if (i==1) y = x
  } 
  else {
    if (i <= dim[3]) y = asub(x, i, 3)
  }
  
  if (is.null(y)) 
    y = new("Image", .Data = array(0, dim[-3]), colormode = Grayscale)
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

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("hist", signature(x="Image"),
  function (x, breaks=64L, rg=range(x, na.rm=TRUE), main=paste("Image histogram:", length(x), "pixels"), xlab="Intensity", ...) {
    if ( !is.numeric(rg) || length(rg) != 2 ) stop("'range' must be a numeric vector of length 2.")
    if ( colorMode(x)==Color ) {
      d3 = dim(x)[3L]
      nc = if ( is.na(d3) ) 1L else min(d3, 3L)
      colores = c("red", "green", "blue")[1:nc]
      y = sapply(colores, function(m) imageData(channel(x, m)), simplify = FALSE, USE.NAMES = TRUE)
    } else {
      y = list(black=imageData(x))
    }

    if(length(breaks)==1L) {
      bins = breaks
      breaks = seq(rg[1], rg[2], length=breaks+1L)
    } else {
      bins = length(breaks) - 1L
    }
    
    h = lapply(y, hist.default, breaks=breaks, plot=FALSE)
    xx = vapply(h, "[[", vector(mode = "double", length = bins+1L), "breaks")
    yy = vapply(h, "[[", vector(mode = "integer", length = bins), "counts")
    yy = rbind(yy, yy[bins,])
    matplot(xx, yy, type="s", lty=1L, main=main, xlab=xlab, col=names(y), ylab="counts", ...)
    
    if ( length(h)==1L && names(h)=="black")
      h = h$black
    
    invisible(h)
  }
)

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
combineImages = function (x, y, ...) {
  dx = dim(x)
  dy = dim(y)
  cx = colorMode(x)
  cy = colorMode(y)
  
  if ( dx[1] != dy[1] || dx[2] != dy[2] ) stop("images must have the same 2D frame size to be combined")
  if ( cx != cy ) stop("images must have the same color mode to be combined")
  
  ## merging along position guided by colorMode
  along = if (cx == Color) 4L else 3L
  
  ## add extra dimension in case of single frame Color Images
  if (along == 4L) {
    if (length(dx)==2L) dim(x) = c(dx, 1)
    if (length(dy)==2L) dim(y) = c(dy, 1)
  }
  
  z = abind::abind(x, y, along = along)
  ## don't introduce unnecessary dimnames
  if ( is.null(dimnames(x)) && is.null(dimnames(y)) ) dimnames(z) = NULL
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
    if (!is.null(names(x))) names(x) <- NULL
    do.call(combine, x)
  }
)

## useful when combining image lists containing NULL elements
setMethod("combine", signature("ANY", "NULL"), function(x, y, ...) x)
setMethod("combine", signature("NULL", "ANY"), function(x, y, ...) y)
setMethod("combine", signature("NULL", "NULL"), function(x, y, ...) NULL)

## Median & quantile redefinition
## needed to overcome an isObject() test in median() which greatly slows down median()
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
median.Image <- function(x, ...) {
  median(x@.Data, ...)
}

quantile.Image <- function(x, ...) {
  quantile(x@.Data, ...)
}


## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rgbImage = function(red=NULL, green=NULL, blue=NULL) {
  classname = function(x) as.character(class(x))  
  
  set_check = function(prop, ch) {
    if ( !is.null(ch) ) {
      if ( is.null(prop) ) {
        prop = list(dim = dim(ch),
                    cls = classname(ch),
                    ref = substitute(ch),
                    dns = !is.null(dimnames(ch)))
      }
      else {
        if  ( !identical(prop$dim, dim(ch)) )
          stop('images must have the same 2D frame size and number of frames to be combined')
        if ( !identical(prop$cls, classname(ch)) )
          stop('images must be object of the same class in order to be combined')
        prop$dns = prop$dns || !is.null(dimnames(ch))
      }
    }
    prop
  }
  
  prop = set_check(NULL, red)
  prop = set_check(prop, green)
  prop = set_check(prop, blue)

  if (is.null(prop)) stop('at least one non-null array must be supplied')
  
  if (is.null(red)) red = array(0, dim=prop$dim)
  if (is.null(green)) green = array(0, dim=prop$dim)
  if (is.null(blue)) blue = array(0, dim=prop$dim)
  
  x = abind::abind(red, green, blue, along=2.5)
  ## don't introduce unnecessary dimnames
  if ( !prop$dns ) dimnames(x) = NULL
  
  if ( extends(prop$cls, "Image") ) {
    res = eval(prop$ref)
    imageData(res) <- x
    colorMode(res) <- Color
  } else {
    res = new("Image", .Data = x, colormode = Color)
  }
  
  res
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
as.raster.Image = function(x, max = 1, i = 1L, ...) {
  y = getFrame(x, i, type = 'render')
  y = clipImage(y, range = c(0, max))
  y = imageData(y)
  d = dim(y)
  
  ## grayscale
  r <- if (colorMode(x) == Grayscale)
    rgb(y, y, y, maxColorValue = max)
  ## color
  else {
    if (length(d) == 2L)
      rgb(y, 0, 0, maxColorValue = max)
    else
      switch(min(d[3L], 4L),
             rgb(y[,,1L], 0, 0, maxColorValue = max),
             rgb(y[,,1L], y[,,2L], 0, maxColorValue = max),
             rgb(y[,,1L], y[,,2L], y[,,3L], maxColorValue = max),
             rgb(y[,,1L], y[,,2L], y[,,3L], y[,,4L], maxColorValue = max)
      )
  }
  
  attributes(r) <- list(dim = d[2:1], class = "raster")
  r
}

