## cat tests/test.R | R --vanilla &>tests/test.Rout.save
library("EBImage")

## returns a hashcode given an object
hash <- function(x) {
  if (is.list(x)) hash(sapply(x,hash))
  else {
    xd <- as.numeric(x)
    xd <- xd[!is.nan(xd)]
    if (is.matrix(xd)) sum(xd*(1:length(xd))) + 0.7*hash(dim(xd))
    else sum(xd*(1:length(xd))) - 0.1
  }
}

## try to evaluate fun(x,...) 
check <- function(fun, x, ...) {
  passed <- TRUE
  
  cat("checking \'", fun, "\' ... ", sep="")
  y=try(do.call(fun,c(list(x),list(...))), silent=TRUE)
  if (class(y)=="try-error" || ( is.Image(y) && !validObject(y)) ) {
    y <- NULL
    passed <- FALSE
  }
  
  if (passed) cat("OK (hash=", hash(y), ")\n", sep="") 
  else cat("FAILED\n")
   
  y
}

testEBImageFunctions <- function(x) {
  cat("new test (hash=", hash(x), ")\n", sep="")

  ## pixel arithmetic
  z <- check(">", x, 0.5)
  z <- check("+", x, x)
  z <- check("/", x, 2)
  z <- check("transpose", x)
  if (mode(x)!="logical") z <- check("median", x)
  
  ## image methods
  z <- check("Image", x, colormode=Color)
  z <- check("as.Image", x)
  z <- check("is.Image", x)
  z <- check("imageData", x)
  z <- check("imageData<-", x, z)
  z <- check("colorMode<-", x, Grayscale)
  z <- check("getNumberOfFrames", x, type="render")
  
  ## subset
  sub <- list(x, 1:10, 1:7)
  if (length(dim(x))>2) sub <- c(sub, length(dim(x))-2)
  z <- do.call("check", c("[", sub))

  ## I/O
  check("writeImage", x, "test.png")

  ## statial transform
  z <- check("resize", x, 137, 22)
  z <- check("rotate", x, 20)
  z <- check("flip", x)
  z <- check("flop", x)
  z <- check("translate", x, c(-7, 5))
  z <- check("affine", x, matrix(c(-7, 5, 0.1, -0.2, 0.3, 1), ncol=2))

  ## segmentation
  z <- check("thresh", x)
  z <- check("ocontour", x>0.5)
  y <- check("bwlabel", x>0.5)
  ## z <- check("rmObjects", y, 3)
  z <- check("reenumerate", y)
  z <- check("paintObjects", y, x)
  
  ## filtering
  z <- check("normalize", x)
  z <- check("gblur", x, sigma=2)
  z <- check("filter2", x, array(1, dim=c(5, 5)))
  z <- check("medianFilter", x, 3)
  
  ## morphological operations
  y <- x>0.5
  z <- check("erode", y)
  z <- check("dilate", y)
  z <- check("distmap", y)
  z <- check("watershed", y)
  z <- check('floodFill', y, c(10, 10), 0.5)
  z <- check('fillHull', y)
  z <- check("erodeGreyScale", x)
  z <- check("dilateGreyScale", x)
  z <- check("whiteTopHatGreyScale", x)
  z <- check("selfcomplementaryTopHatGreyScale", x)
  
  ## colorspace
  z <- check("channel", x, "rgb")
  z <- check("rgbImage", x, x>0.5)
  
  ## image stacking, combining, tiling
  z <- check("combine", x, x)
  y <- check("tile", x, nx=2)
  z <- check("untile", y, c(2,2))
  
  ## features
  z <- check("computeFeatures", bwlabel(x>0.5), x, expandRef=NULL)
  cat("\n")
}

## test: grayscale 2D 
x <- readImage(system.file("images","lena.png", package="EBImage"))[1:32, 1:50]
testEBImageFunctions(x)

## test: color 2D
x <- readImage(system.file("images","lena-color.png", package="EBImage"))[1:32, 1:50,]
testEBImageFunctions(x)

## test: color 3D
x <- readImage(system.file("images","lena-color.png", package="EBImage"))[1:32, 1:50,]
x <- combine(x, x)
testEBImageFunctions(x)

## test: logical 2D
x <- readImage(system.file("images","lena-color.png", package="EBImage"))[1:32, 1:50,]
testEBImageFunctions(x>0.5)
