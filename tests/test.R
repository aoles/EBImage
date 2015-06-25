## cat tests/test.R | R --vanilla &>tests/test.Rout.save
library("EBImage")

set.seed(0) # make random color permutations in 'colorLabels' reproducible

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
  else cat("FAIL\n")

  y
}

testEBImageFunctions <- function(x) {
  cat("new test (hash=", hash(x), ")\n", sep="")

  ## pixel arithmetic
  z <- check(">", x, 0.5)
  z <- check("+", x, x)
  z <- check("/", x, 2)
  z <- check("*", 2, x)
  z <- check("transpose", x)
  if (mode(x)!="logical") z <- check("median", x)

  ## image methods
  z <- check("Image", x, colormode=Color)
  z <- check("as.Image", x)
  z <- check("is.Image", x)
  z <- check("imageData", x)
  z <- check("imageData<-", x, z)
  z <- check("colorMode<-", x, Grayscale)
  z <- check("numberOfFrames", x, type="render")
  z <- check("getFrames", x)
  z <- check("display", x, all=TRUE)
  
  ## drawCircle
  d <- dim(x)
  c.x <- round(d[1L]/2)
  c.y <- round(d[2L]/2)
  radius <- c.x - 1
  nf <- numberOfFrames(x, "render")
  fill <- nf > 1
  col = if ( colorMode(x)==Color ) "yellow" else 1
  z <- check("drawCircle", x, c.x, c.x, radius, col, fill, nf)
  
  ## subset
  sub <- list(x, 1:10, 1:7)
  if (length(dim(x))>2) sub <- c(sub, rep(TRUE, length(dim(x))-2))
  z <- do.call("check", c("[", sub))

  ## spatial transform
  z <- check("resize", x, 137, 22)
  z <- check("rotate", x, 20)
  z <- check("flip", x)
  z <- check("flop", x)
  z <- check("translate", x, c(-7, 5))
  z <- check("affine", x, matrix(c(-7, 5, 0.1, -0.2, 0.3, 1), ncol=2))
  z <- check("transpose", x)

  ## segmentation
  z <- check("thresh", x)
  y <- check("bwlabel", x>0.5)
  z <- check("colorLabels", y)
  z <- check("rmObjects", getFrame(y, 1), 3)
  z <- check("reenumerate", y)
  z <- paintObjects(channel(y, "gray"), x)
  y <- check("ocontour", x>0.5)
  z <- check("localCurvature", y[[1]])

  ## filtering
  z <- check("normalize", x)
  z <- check("gblur", x, sigma=2)
  z <- check("filter2", x, array(1, dim=c(5, 5)))
  z <- check("medianFilter", x, 3)
  z <- check("equalize", x)

  ## morphological operations
  y <- x>0.5
  z <- check("erode", y)
  z <- check("dilate", y)
  z <- check("opening", y, makeBrush(5, 'line'))
  z <- check("closing", y, makeBrush(5, 'disc'))
  z <- check("distmap", y)
  z <- check("watershed", y)
  z <- check('floodFill', y, c(10, 10), 0.5)
  z <- check('fillHull', y)
  z <- check("erodeGrayscale", x)
  z <- check("dilateGrayscale", x)
  z <- check("openingGrayscale", x)
  z <- check("closingGrayscale", x)
  z <- check("whiteTopHatGrayscale", x)
  z <- check("blackTopHatGrayscale", x)
  z <- check("selfcomplementaryTopHatGrayscale", x)

  ## colorspace
  z <- check("channel", x, "rgb")
  z <- check("rgbImage", x, x>0.5)

  ## image stacking, combining, tiling
  z <- check("combine", x, x)
  y <- check("tile", x, nx=2)
  z <- check("untile", y, c(2,2))

  ## features
  y <- getFrame(x, 1)
  z <- check("computeFeatures", bwlabel(y>0.5), y, expandRef=NULL)
  cat("\n")
}

sample <- readImage(system.file("images","sample.png", package="EBImage"))
sample.color <- readImage(system.file("images","sample-color.png", package="EBImage"))

## test: 2D Grayscale
x <- sample[1:32, 1:48]
testEBImageFunctions(x)

## test: 2D Color
colorMode(x) <- Color
x <- t(x)
testEBImageFunctions(x)

## test: 3D Color
x <- sample.color[1:65, 1:17,]
testEBImageFunctions(x)

## test: 3D Grayscale logical
x <- sample[32:63, 32:63]
x <- x > otsu(x)
x <- combine(x, x)
testEBImageFunctions(x)

## test: 4D Color
x <- sample.color[1:33, 1:16,]
x <- combine(x, x)
testEBImageFunctions(x)

## test: 4D Grayscale
colorMode(x) <- Grayscale
imageData(x) <- aperm(x, c(2, 1, 4, 3))
testEBImageFunctions(x)
