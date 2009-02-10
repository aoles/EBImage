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
TrueColor = 1L  ## deprecated mode
Color     = 2L

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClass ("Image",
  representation (colormode="integer"),
  prototype (colormode=Grayscale),
  contains = "array"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Image=function(data=array(0,dim=c(1,1)), dim, colormode=NULL) {
  if (missing(dim)) {
    if (is.array(data)) dim=base::dim(data)
    else dim=c(1,length(data))
  }
  ld = length(dim)
  if (ld<2) stop(sprintf("length(dim) must be at least 2 and is %d.", ld))

  if (is.null(colormode)) {
    if (is.Image(data)) colormode=colorMode(data)
    else colormode=Grayscale
  } else colormode=EBImage:::parseColorMode(colormode)

  ## TrueColor<->(Grayscale,Color) conversion, if requested
  if (is.Image(data)) {
    if (colorMode(data)==TrueColor) {
      if (colormode==Grayscale) {
        data=channel(data,'gray')
        dim=dim(data)
      }
      else if (colormode==Color) {
        data=rgbImage(r=channel(data,'red'),g=channel(data,'green'),b=channel(data,'blue'))
        dim=dim(data)
      }
    }
    if (colormode==TrueColor) {
      if (colorMode(data)==Grayscale) {
        dim=dim(data)
        data=as.integer(floor(256*channel(data,'gray')))
        data[data<0]=0
        data[data>255]=255
        data=256*256*data+256*data+data
      } else if (colorMode(data)==Color) {
        dim=c(dim(data)[1:2],getNumberOfFrames(data,'render'))
        red=as.integer(floor(256*channel(data,'red')))
        red[red<0]=0
        red[red>255]=255
        green=as.integer(floor(256*channel(data,'green')))
        green[green<0]=0
        green[green>255]=255
        blue=as.integer(floor(256*channel(data,'blue')))
        blue[blue<0]=0
        blue[blue>255]=255
        data=as.integer(256*256*blue+256*green+red)
      } 
    }
  }

  if (colormode==TrueColor) data=as.integer(data)
  res = new("Image", .Data=array(data,dim=dim),colormode=colormode)
  
  validObject(res)
  return(res)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("as.Image", signature(x="array"),
  function (x) {
    if (is.integer(x)) return(Image(x, colormode=TrueColor))
    else x = Image(x, colormode=Grayscale)
    validObject(x)    
    return(x)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("colorMode", signature (x="Image"),
  function (x) x@colormode
)
setMethod ("colorMode", signature (x="array"),
  function (x) Grayscale
)
setReplaceMethod ("colorMode", signature (x="Image", value="ANY"),
  function (x, value) {
    ## conversion here should not be possible ! kept for compatibility
    if ((x@colormode==TrueColor & value!=TrueColor) |
        (x@colormode!=TrueColor & value==TrueColor)) {
      x=Image(x,colormode=value)
    } else {
      x@colormode = EBImage:::parseColorMode(value)
      validObject(x)
    }
    return(x)
  }
)
setReplaceMethod ("colorMode", signature (x="array", value="ANY"),
  function (x, value) {
    warning('Color mode of an array cannot be changed, the array should be cast into an Image using \'Image\'')
    return(x)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("imageData", signature (x="Image"),
  function (x) x@.Data
)
setMethod ("imageData", signature (x="array"),
  function (x) x
)
setReplaceMethod ("imageData", signature (x="Image", value="array"),
  function (x, value) {
    x@.Data = value
    ## conversion here should not be possible ! kept for compatibility
    if (is.integer(value)) x@colormode = TrueColor
    validObject(x)   
    return (x)
  }
)
setReplaceMethod ("imageData", signature (x="array", value="array"),
  function (x, value) {
    x = value
    return (x)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
is.Image <- function (x) {
  if (!is(x, "Image")) return(FALSE)
  else return (TRUE)
}

## Assert (misnamed) checks Image dimension & color compatibility
## If strict is TRUE,  all the dimensions and colorMode are checked
## If strict is FALSE, only the two first dimensions and colorMode are checked
## GP: Useful ?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("assert", signature (x="array"),
  function (x, y, strict=FALSE) {
    n <- 2
    if (missing(y)) return(is.Image(x))
    if (strict) {
      if (length(dim(x))!=length(dim(y))) return(FALSE)
      else n = length(dim(x))
    }
    if ( any( dim(x)[1:n] != dim(y)[1:n] ) || colorMode(x) != colorMode(y) )
      return( FALSE )
    return( TRUE )
  }
)

## Shallow copy of the object: only the members (not the array) are copied
## GP: Useful ?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("header", signature(x="Image"),
  function (x) {
    dim=rep(1,length(dim(x)))
    if (colorMode(x) == Grayscale) data=array(0,dim=dim)
    else if (colorMode(x) == TrueColor) data=array(0L,dim=dim) 
    else data=array(0,dim=dim)
    
    y = new(class(x),.Data=data,colormode=colorMode(x))
    return(y)
  }
)
setMethod ("header", signature(x="array"),
  function (x) {
    dim=rep(1,length(dim(x)))
    return(array(0,dim=dim))
  }
)
      
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
validImageObject=function(object) {
  ## check colormode
  if (!is.integer(colorMode(object))) return('colormode must be an integer')
  if (colorMode(object)<0 | colorMode(object)>2) return('invalid colormode')
  if (colorMode(object)==TrueColor) warning('deprecated: the colormode \'TrueColor\' is deprecated, you should use the colormode \'Color\' instead')
  
  ## check array
  if (!is.array(object)) return('object must be an array')

  ## check dim
  d=dim(object)
  if (length(d)<2) return('object must have at least two dimensions')
  if (getNumberOfFrames(object,'total')<1) return('Image must contain at least one frame')
 
  TRUE
}
setValidity("Image",validImageObject) 

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

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("writeImage", signature(x="array"),
  function (x, files, quality=100) {
    validObject(x)
    if ( quality < 1 || quality > 100 )
      stop( "quality value is given in % between 1 and 100" )
    if ( missing(files) ) stop('\'files\' must be specified')
    invisible ( .ImageCall("lib_writeImages", x, as.character(files), as.integer(quality) ) )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
readImage <- function(files, colormode) {
  if (missing(files)) stop("argument 'files' must be present in calls to 'readImage'")
  if (missing(colormode)) colormode=-1
  .DoCall ("lib_readImages", as.character(files), as.integer(colormode) )
}

## GP: Useful ?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
chooseImage <- function(colormode=Grayscale) {
  .DoCall ("lib_chooseImages", as.integer(colormode))
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

## getNumberOfFrames
## If type='total', returns the total number of frames
## If type='render', return the number of frames to be rendered after color channel merging
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod('getNumberOfFrames',signature(x="array"),
          function(x,type='total') {
            if (type=='render' & colorMode(x)==Color) {
              if (length(dim(x))< 3) return(1)
              else return(prod(dim(x)[-1:-3]))
            }
            else return(prod(dim(x)[-1:-2]))
          })

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("show", signature(object="Image"),
  function (object) {
    cat('Image\n')

    valid=validObject(object,test=TRUE)
    if (!is.logical(valid)) valid=paste(FALSE,', ',valid,sep='')

    cat('  colormode:',c('Grayscale','TrueColor','Color')[1+colorMode(object)],'\n')
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
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
  if (colorMode(x)!=Color) stop('in \'selectChannel\', color mode must be Color')
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

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("channel", signature(x="array", mode="character"),
  function (x, mode) { 
    mode=tolower(mode)

    validObject(x)
    
    if (colorMode(x)==Grayscale) {
      return(switch(mode,
                    rgb=rgbImage(r=x,g=x,b=x),
                    grey=,gray=x,
                    r=,red=stop('invalid conversion mode, can\'t extract the red channel from a \'Grayscale\' image'),
                    g=,green=stop('invalid conversion mode, can\'t extract the green channel from a \'Grayscale\' image'),
                    b=,blue=stop('invalid conversion mode, can\'t extract the blue channel from a \'Grayscale\' image'),
                    asred=rgbImage(r=x),
                    asgreen=rgbImage(g=x),
                    asblue=rgbImage(b=x),
                    x11=array(rgb(x,x,x),dim=dim(x)),
                    stop('invalid conversion mode')
                    ))
    }
    else if (colorMode(x)==Color) {
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
                    x11=array(rgb(selectChannel(x,1),selectChannel(x,2),selectChannel(x,3)),dim=dim(x)),
                    stop('invalid conversion mode')
                    ))
    }
    else {
      ## TrueColor deprecated mode
      modeNo <- as.integer( switch (EXPR=mode, rgb=0, grey=, gray=1, r=, red=2, g=,
                                    green=3, b=, blue=4, asred=5, asgreen=6, asblue=7, x11=8, -1) )
      if (modeNo < 0) stop("wrong conversion mode")
      
      resData <- .DoCall("lib_channel", imageData(x), modeNo)
      if (is.null(resData))
        stop("error converting colors, check if all supplied values majke sense for color representation")
      resData[which(is.na(x))] = NA
      resData = array (resData, dim(x) )
      if (mode == "x11") return(resData)
      colormode=switch (EXPR=mode, rgb=, asred=, asgreen=,
        asblue=TrueColor, Grayscale)
      res=Image(data=resData,colormode=colormode)
      return(res)
    }
  }
)
setMethod ("channel", signature(x="ANY", mode="character"),
  function (x, mode, ...) {
    mode <- tolower (mode)
    modeNo <- as.integer( switch (EXPR=mode, rgb=0, grey=, gray=1, r=, red=2, 
            g=, green=3, b=, blue=4, asred=5, asgreen=6, asblue=7, x11=8, -1) )
    if ( modeNo < 0 )
      stop( "wrong conversion mode")
    if ( !is.numeric(x) && !is.character(x) )
      stop( "argument must be coercible to either numeric or character" )
    res <- .DoCall("lib_channel", x, modeNo )
    if ( !is.null(res) )
      res [ which( is.na(x) ) ] = NA
    if ( is.null(res) || is.character(res) ) return (res)
    if ( is.array(x) ) dim (res) <- dim (x)
    return (res)
  }
)

## GP: Useful ?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("combine", signature(x="array"),
  function (x,...,along) {
    args=c(list(x),list(...))
    if (length(args)==1) return(x)

    ## check colorMode
    cm=sapply(args,colorMode)==TrueColor
    if (any(cm) && !all(cm)) stop("\'TrueColor\' images cannot be combined with non \'TrueColor\' ones")

    ## check dim[1:2]
    dm=sapply(args,function(z) dim(z)[1:2])
    dmx=dm[1,]==dm[1,1]
    dmy=dm[2,]==dm[2,1]
    if (!all(dmx) || !all(dmy)) stop("images must have the same 2D frame size to be combined")

    ## check nb dim and removes final dimension 1, if any (needed by abind)
    nd=sapply(args,function(z) length(dim(z)))
    ndf=mapply(function(a,n) dim(a)[n],args,nd)
    df1=which(ndf==1)
    for (k in df1) dim(args[[k]])=dim(args[[k]])[1:(nd[k]-1)]
    nd[df1]=nd[df1]-1
    if (all(nd[1]!=nd)) stop("images must have the same 2D frame size to be combined")

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
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("combine", signature(x="list"),
  function (x) {
    names(x) <- NULL
    do.call(combine, x)
  }
)

## Median & quantile redefinition
## needed to overcome an isObject() test in median() which greatly slows down median()
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
median.Image <- function(x, na.rm = FALSE) {
  median(imageData(x), na.rm=na.rm)
}
quantile.Image <- function(x, ...) {
  quantile(imageData(x), ...)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rgbImage = function(red=NULL,green=NULL,blue=NULL) {
  d=NULL
  
  if (!is.null(red)) d=dim(red)
  if (!is.null(green)) d=dim(green)
  if (!is.null(blue)) d=dim(blue)
  if (is.null(red)) red=Image(0,dim=d)
  if (is.null(green)) green=Image(0,dim=d)
  if (is.null(blue)) blue=Image(0,dim=d)
  
  if (is.null(d)) stop('at least one non-null Image object must be specified')
  if (colorMode(red)!=Grayscale | colorMode(green)!=Grayscale |colorMode(blue)!=Grayscale) stop('all Image objects must be in \'Grayscale\' color mode')

  x=combine(red,green,blue,along=2.5)
  
  ## Cast to Color Image if x is an array
  if (class(x)=='array') x=Image(x)
    
  colorMode(x)=Color
  x
}

## sqrt(r^2+g^2+b^2)
## GP: Useful ?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
channelMix = function(r,g,b) {
  mix = list()
  if (!missing(r)) mix[["r"]] = r
  if (!missing(g)) mix[["g"]] = g
  if (!missing(b)) mix[["b"]] = b
  if (length(mix)==0)
    stop("at least one image must be provided")
  if (length(mix)==1) return(mix[[1]])
  res = mix[[1]]^2 + mix[[2]]^2
  if (length(mix)==3) res = res + mix[[3]]^2
  sqrt(res)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
parseColorMode=function(colormode) {
  icolormode=NA
  
  if (is.numeric(colormode)) {
    ## warning('deprecated: colormode should be specified through a character string: \'grayscale\' or \'color\'')
    icolormode=colormode
  } else if (is.character(colormode)) {
    icolormode=pmatch(tolower(colormode),c('grayscale','truecolor','color'),duplicates.ok=TRUE,nomatch=NA)-1
  }
  
  as.integer(icolormode)
}

