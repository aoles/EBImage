library(EBImage)

hash=function(x) {
 
}

check=function(fun,x,...) {
  passed=T
  
  cat('checking \'',fun,'\' ... ',sep='')
  y=try(do.call(fun,c(list(x),list(...))),silent=T)
  if (class(y)=='try-error' || ( is.Image(y) && !validObject(y)) ) {
    y=NULL
    passed=F
  }
  
  if (!passed) cat('FAILED\n')
  else cat('OK\n')
 
  y
}

test=function(x) {  
  ## Image, is.image, Ops, as.Image, assert, header
  ## imageData, imageData, colorMode, colorMode<-, print
  ## [, getNumberOfFrames
  cat('new test\n')
  y=Image(x,colormode=Color)
  a=is.Image(y)
  y=check('>',x,0.5)
  y=check('+',x,y)
  y=check('/',x,2)
  a=check('assert',x,y)
  z=check('header',x)
  z=as.Image(imageData(y))
  imageData(z)=y
  colorMode(z)=Grayscale
  ##b=check('print',z)
  pat=as.list(rep(T,length(dim(x))))
  pat[[1]]=1:150
  pat[[2]]=1:100
  b=check('getNumberOfFrames',y)
  y=do.call('[',c(list(x),pat))

  ## display
  if (interactive()) check('display',x,main='x')
  check('writeImage',x,'test.png')

  ## resize, resample, rotate, flip, flop, translate
  y=check('resize',x,277,139)
  y=check('resample',x,277,139)
  y=check('rotate',x,20)
  y=check('flip',x)
  y=check('flop',x)
  y=check('translate',x,rep(1,2*getNumberOfFrames(x,'total')))

  ## edge, segment
  y=check('edge',x)
  if (colorMode(y)!=TrueColor) y=check('segment',x) ## too slow in TrueColor
  y=check('thresh',x)
  y=check('athresh',x)
  y=check('cthresh',x)
  
  ## median, hist
  b=check('median',x)
  if (interactive()) check('hist',x) 

  ## blur, gblur, normalize, negate, normalize2
  y=check('blur',x,r=20,s=10)
  y=check('gblur',x,r=10,s=5)
  y=check('normalize',x)
  y=check('negate',x)
  y=check('normalize2',x)

  ## filter2
  z=matrix(1,nc=5,nr=5)
  z[3,3]=-24
  y=check('filter2',x,z)

  ## erode, dilate, opening, closing, distmap
  w=x>0.5
  y=check('erode',w)
  y=check('dilate',w)
  y=check('opening',w)
  y=check('closing',w)
  y=check('distmap',w)

  ## rgbImage, channel
  if (colorMode(x)==Grayscale) {
    y=rgbImage(x,flip(x),flop(x))
    w=check('channel',x,'asblue')
    w=check('channel',x,'rgb')
  } else y=x
  w=check('channel',y,'gray')
  w=check('channel',y,'green')
  w=check('channelMix',y)
  
  ## combine
  y=check('combine',x,x,x)
  y=check('tile',x)
  y=check('untile',x,c(2,2))
  cat('\n')
}

## Gray - 1 image - 2 dim
x=readImage('../lena.gif')
test(x)

## Gray - 1 image - 3 dim
dim(x)=c(512,512,1) 
test(x)

## Gray - 3 images - 3 dim
x=combine(x,flip(x),flop(x))
test(x)

## Color - 1 image - 3 dim
colorMode(x)=Color
test(x)

## Color - 1 image - 4 dim
dim(x)=c(dim(x),1)
test(x)

## Color - 3 images - 4 dim
x=combine(x,x,x)
test(x)

## TrueColor - 3 images - 3 dim
x=Image(x,colormode=TrueColor)
test(x)

##
library(EBImage)
w=readImage('../lena-color.bmp')
x=combine(w,flip(w))
y=tile(x)
z=untile(x,c(10,1))


