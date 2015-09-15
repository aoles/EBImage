## cat tests/test.R | R --vanilla &>tests/test.Rout.save
library("EBImage")

set.seed(0) # make random color permutations in 'colorLabels' reproducible

## returns a hashcode given an object
hash = function (x) .Call(digest:::digest_impl, serialize(x, connection=NULL, ascii=FALSE), 7L, -1L, 14L, 0L, 0L, PACKAGE="digest")

hashold <- function(x) {
  if ( is.list(x) && length(x)>0L ) hash(sapply(x, hash))
  else {
    xd <- suppressWarnings(as.numeric(x))
    xd <- xd[!(is.nan(xd)||is.na(xd))]
    lxd <- length(xd)
    if (lxd==0L) NA
    else {
      if (is.matrix(xd)) sum(xd*(1:lxd)) + 0.7*hash(dim(xd))
      else sum(xd*(1:lxd)) - 0.1
    }
  }
}

## try to evaluate fun(x,...) 
check <- function(fun, x, ..., capture.output=FALSE, suppressWarnings=FALSE, suppressMessages=FALSE) {
  passed <- TRUE

  cat(sprintf("checking \'%s\' %s ", fun, paste(rep(".", 35L-nchar(fun)), collapse = "")))
  
  expr = quote(do.call(fun,c(list(x),list(...))))
  if ( isTRUE(capture.output) ) expr = call("capture.output", expr)
  if ( isTRUE(suppressWarnings) ) expr = call("suppressWarnings", expr)
  if ( isTRUE(suppressMessages) ) expr = call("suppressMessages", expr)
  
  y <- try(eval(expr), silent=TRUE)
  
  if (class(y)=="try-error" || ( is.Image(y) && !validObject(y)) ) {
    y <- NULL
    passed <- FALSE
  }

  if (passed) cat("PASS (", hash(y), " ", hashold(y), ")\n", sep="") 
  else cat("FAIL\n")
  
  y
}

checkIO <- function(name) {
  cat("checking IO for \'", name, "\' ... ", sep="")
  x = get(name)
  y = FALSE
  if ( !is.null(x) ) {
    y <- try({
      xx <- readImage(writeImage(x, tempfile("", fileext = ".tif")))
      dimnames(xx) <- dimnames(x)
      identical(x, xx)
      }, silent=TRUE)
  }
  if ( isTRUE(y) ) cat("PASS\n") else cat("FAIL\n")
  invisible(y)
}

testIOFunctions <- function(...) invisible(lapply(list(...), function(y) checkIO(y)))

testEBImageFunctions <- function(x) {
  cat("new test (hash=", hash(x), ")\n", sep="")
  
  z <- check("show", x, capture.output=TRUE)
  z <- check("print", x, short=TRUE, capture.output=TRUE)
  if ( typeof(x)=="logical" )
    z <- check("hist", EBImage:::castImage(x), breaks = c(0, .5, 1))
  else
    z <- check("hist", x)
  
  ## pixel arithmetic
  z <- check(">", x, 0.5)
  z <- check("+", x, x)
  z <- check("/", x, 2)
  z <- check("*", 2, x)
  z <- check("transpose", x)
  z <- check("median", x)
  z <- check("quantile", x)

  ## image methods
  z <- check("Image", x, colormode="Color")
  z <- check("as.Image", x)
  z <- check("is.Image", x)
  z <- check("imageData", x)
  z <- check("imageData<-", x, z)
  z <- check("as.raster", x)
  z <- check("colorMode<-", x, Grayscale, suppressWarnings=TRUE)
  y <- check("numberOfFrames", x, type="render")
  z <- if ( y==1L ) check("getFrames", x, 1L, "render") else check("getFrames", x)
  z <- check("display", x, method = "browser", browser = "false")
  if ( y>2L ) {
    z <- check("display", x, method = "raster", all = TRUE)
    z <- check("image", x, i = 3L)
  }
  else {
    z <- if (y==1L) check("display", x, method = "raster") else check("display", x, method = "raster", frame = 2L, suppressMessages=TRUE)
    z <- check("image", x, suppressMessages=TRUE)
  }
  
  ## drawCircle
  d <- dim(x)
  c.x <- ceiling(d[1L]/2)
  c.y <- ceiling(d[2L]/2)
  radius <- max(c.x-1, 1)
  nf <- numberOfFrames(x, "render")
  fill <- nf > 1
  col <- if ( colorMode(x)==Color ) "yellow" else 1
  z <- check("drawCircle", x, c.x, c.x, radius, col, fill, nf)
  
  ## subset
  sub <- list(x, 1:min(10,d[1L]), 1:min(7,d[2L]))
  if (length(d)>2) sub <- c(sub, rep(TRUE, length(d)-2))
  z <- do.call("check", c("[", sub))

  ## spatial transform
  z <- check("resize", x, 137, 22)
  z <- check("rotate", x, 20)
  z <- check("flip", x)
  z <- check("flop", x)
  z <- check("translate", x, c(-7, 5), bg.col=1)
  z <- check("affine", x, matrix(c(-7, 5, 0.1, -0.2, 0.3, 1), ncol=2L))
  z <- check("transpose", x)

  ## segmentation
  z <- check("thresh", x)
  y <- check("channel", x, "luminance")
  z <- check("otsu", y)
  y <- suppressWarnings(normalize(y, separate=FALSE))
  y[is.nan(y)] <- 0
  y <- check("bwlabel", y > 0.5)
  z <- check("colorLabels", y, suppressWarnings=TRUE)
  z <- check("stackObjects", y, x)
  z <- check("stackObjects", Image(dim=dim(y)), x)
  cls <- if ( colorMode(x)==Color ) TRUE else FALSE
  z <- check("paintObjects", y, x, col=c("#ff00ff", "#ffff00"), opac=c(1.0, 0.5), thick=cls, closed=cls)  
  z <- check("rmObjects", y, as.list(seq_len(numberOfFrames(y))), cls)
  z <- check("reenumerate", z)
  
  ## features
  x1 <- getFrame(x, 1)
  x2 <- list(x=x1, y=2*x1)
  y1 <- getFrame(y, 1)
  expandRef <- if ( min(dim(x1)) > 31L ) function(ref, refnames) standardExpandRef(ref, refnames, gblob(n=31L)) else NULL
  z <- check("computeFeatures", y1, x2, expandRef = expandRef)
  z <- check("computeFeatures", y1, x2, expandRef = expandRef, properties = TRUE)
  
  ## curvature
  y <- check("ocontour", x>0.5)
  if (length(y) > 0L ) z <- check("localCurvature", y[[1L]])

  ## filtering
  z <- check("normalize", x, suppressWarnings=TRUE)
  z <- check("gblur", x, sigma=1)
  z <- check("filter2", x, array(1, dim=c(5, 5)))
  z <- check("medianFilter", x, 3)
  z <- check("equalize", x)

  ## morphological operations
  y <- x > 0.5
  z <- check("erode", y)
  z <- check("dilate", y, makeBrush(5, 'disc'))
  z <- check("opening", y, makeBrush(5, 'line'))
  z <- check("closing", y, makeBrush(4, 'line', angle=30), suppressWarnings=TRUE)
  z <- check("distmap", y)
  z <- check("watershed", z)
  z <- check('floodFill', y, c(5, 5), 0.5)
  z <- check('fillHull', y)
  z <- check("erodeGrayscale", x)
  z <- check("dilateGrayscale", x)
  z <- check("openingGrayscale", x)
  z <- check("closingGrayscale", x)
  z <- check("whiteTopHatGrayscale", x)
  z <- check("blackTopHatGrayscale", x)
  z <- check("selfcomplementaryTopHatGrayscale", x)

  ## propagate
  y <- thresh(x, offset=0.02)
  y <- fillHull(y)
  y <- bwlabel(y)
  z <- check("propagate", x, y, x>0.5)
  
  ## colorspace
  z <- check("toRGB", x)
  z <- check("rgbImage", x, x>0.5)

  ## image stacking, combining, tiling
  z <- check("combine", list(NULL, x, x, NULL, NULL))
  y <- check("tile", x, nx=2)
  z <- check("untile", y, c(2,2))

  cat("\n")
}


## check error handling
try.readImage <- function(...) tryCatch(suppressWarnings(readImage(...)), error = function(e) NULL)
mock <- try.readImage(system.file("images", package="EBImage"), type="png")
mock <- try.readImage("http://www.huber.embl.de/EBImage/missing.file ", type="png")

## single greyscale and color images
sample <- try.readImage(system.file("images","sample.png", package="EBImage"))
sample.color <- try.readImage(system.file("images","sample-color.png", package="EBImage"))
## multi-frame image stack
f = system.file("images","nuclei.tif", package="EBImage")
nuclei = try.readImage(c(f, f))
## test reading from URL
logo <- try.readImage("http://www.huber.embl.de/EBImage/logo.png")

## test: IO operations
testIOFunctions("sample", "sample.color", "nuclei", "logo")

## test: blank image
testEBImageFunctions(Image(0, c(8, 8)))

## test: 2D Grayscale
x <- nuclei[50:113,208:255,2]
testEBImageFunctions(as.array(x))

## test: 2D Color
x <- sample[1:32, 1:48]
x <- t(x)
testEBImageFunctions(Image(as.vector(x), dim(x), Color))

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
imageData(x) <- aperm(x, c(2L, 1L, 4L, 3L))
testEBImageFunctions(x)
