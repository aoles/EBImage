## cat tests/test.R | R --vanilla &>tests/test.Rout.tmp
library(EBImage)

## returns a hashcode given an object
hash=function(x) {
  if (is.list(x)) hash(sapply(x,hash))
  else {
    xd=as.numeric(x)
    xd=xd[!is.nan(xd)]
    sum(xd*(1:length(xd)))
  }
}

## try to evaluate fun(x,...) 
check=function(fun,x,...) {
  passed=T
  
  cat('checking \'',fun,'\' ... ',sep='')
  y=try(do.call(fun,c(list(x),list(...))),silent=T)
  if (class(y)=='try-error' || ( is.Image(y) && !validObject(y)) ) {
    y=NULL
    passed=F
  }
  
  if (passed) cat('OK (hash=',hash(y),')\n',sep='') 
  else cat('FAILED\n')
   
  y
}

## test a subset of EBImage functions on x
testEBImageFunctions=function(x) {  
  ## Image, is.image, Ops, as.Image
  ## imageData, imageData, colorMode, colorMode<-, print
  ## [, getNumberOfFrames
  cat('new test (hash=',hash(x),')\n',sep='')
  if (class(x)=='Image') check('print',x)
  y=Image(x,colormode=Color)
  a=is.Image(y)
  y=check('>',x,0.5)
  y=check('+',x,y)
  y=check('/',x,2)
  z=as.Image(imageData(y))
  imageData(z)=y
  colorMode(z)=Grayscale
  pat=as.list(rep(T,length(dim(x))))
  pat[[1]]=1:100
  pat[[2]]=1:100
  b=check('getNumberOfFrames',y,type='render')
  y=do.call('[',c(list(x),pat))

  ## display
  if (interactive()) check('display',x,main='x')
  check('writeImage',x,'test.png')

  ## resize, resample, rotate, flip, flop, translate
  y=check('resize',x,277,139)
  y=check('rotate',x,20)
  y=check('flip',x)
  y=check('flop',x)
  y=check('translate',x,rep(1,2*getNumberOfFrames(x,'total')))

  ## thresh
  y=check('thresh',x)
  
  ## median, hist
  if (mode(x)!='logical') b=check('median',x)
  if (mode(x)!='logical' & interactive()) check('hist',x) 

  ## blur, gblur, normalize
  y=check('blur',x,r=20,s=10)
  y=check('gblur',x,r=10,s=5)
  y=check('normalize',x)

  ## filter2
  z=matrix(1,nc=5,nr=5)
  z[3,3]=-24
  y=check('filter2',x,z)

  ## erode, dilate, opening, closing, distmap, watershed
  w=x>0.5
  y=check('erode',w)
  y=check('dilate',w)
  y=check('opening',w)
  y=check('closing',w)
  y=check('distmap',w)
  ws=check('watershed',y)
  y=check('floodFill', w, c(10,10), 0.5)
  y=check('fillHull', w)
  y=check('bwlabel', w)
  
  ## rgbImage, channel
  if (colorMode(x)==Grayscale) {
    y=rgbImage(x,flip(x),flop(x))
    w=check('channel',x,'asblue')
    w=check('channel',x,'rgb')
  } else y=x
  w=check('channel',y,'gray')
  w=check('channel',y,'green')
  w=check('channel',y,'x11')
  
  ## combine, tile, untile
  y=check('combine',x,x,x)
  y=check('tile',y,nx=2)
  y=check('untile',y,c(2,2))

  ## hullFeatures, rmObjects, reenumerate, getFeatures (contains hullFeatures, edgeFeatures, haralickFeatures, zernikeMoments, moments)
  ## paintObjects
  w=ws
  w[w@.Data==1]=2
  w[w@.Data==3]=2117
  y=check('reenumerate',w)
  w=ws
  hf=check('hullFeatures',w)
  if (!is.list(hf)) hf=list(hf)
  rmindex = lapply(hf, function(c) which(c[,'g.s']<6))
  w=check('rmObjects',w,rmindex)
  y=check('getFeatures',w,x)
  cat('\n')
}

## Testing procedure
lena=readImage(system.file("images","lena.gif", package="EBImage"))
lenacolor=readImage(system.file("images","lena-color.png",package="EBImage"))

## Resizing images, for speeding up testing
lena=resize(lena, 128, 128)
lenacolor=resize(lenacolor, 128, 128)

## Image - Grayscale - 1 image  - 2 dim - numeric
testEBImageFunctions(lena)

## Image - Grayscale - 1 image  - 2 dim - logical
x=lena>0.5
testEBImageFunctions(x)

## Image - Color - 1 image  - 3 dim - numeric
testEBImageFunctions(lenacolor)

## Image - Color - 2 images - 4 dim - numeric
x=combine(lenacolor, lenacolor)
testEBImageFunctions(x)

## matrix - 2 images - 3 dim - numeric
x=imageData(lena)
x=combine(x, x)
testEBImageFunctions(x)
