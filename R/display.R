## display displays static images
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
display = function(x, title=paste(deparse(substitute(x))), useGTK=TRUE) {
  validImage(x)
  title = as.character(title)
  useGTK = as.logical(useGTK)
  stopifnot(length(useGTK)==1L, length(title)==1L)
  
  invisible(.Call("lib_display", castImage(x), title, useGTK, PACKAGE="EBImage"))
}

## displays an image using R graphics
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
displayRaster = function(image, frame, all = FALSE){
  validImage(image)

  nf = getNumberOfFrames(image, type='render')

  dim <- dim(image) 
  xdim <- dim[1]
  ydim <- dim[2]

  ## save current graphical parametrs
  op <- par(no.readonly=TRUE)
  ## set new graphical parameters
  par(bty="n", mai=c(0,0,0,0), xaxs="i", yaxs="i", xaxt="n", yaxt="n")

  ## display all frames in a grid-like environment
  if (all){
    ncol = ceiling(sqrt(nf))
    nrow = ceiling(nf/ncol)

    plot(c(1, ncol*xdim), c(1, nrow*ydim), type = "n", xlab="", ylab="", asp=1)

    f = 1
    for(r in nrow:1) {
      for(c in 1:ncol) {
        ## plot the figure as a raster image
        if (f<=nf) {
          rasterImage(getFrame(image, f, type='render'), 1 + ((c-1)*xdim) , 1 + ((r-1)*ydim), c*xdim, r*ydim, interpolate=TRUE)
          f = f + 1
        }
        else break
      }
    }
  }

  ## display a single frame only (by default the first one)
  else {
    if (missing(frame)){
      frame = 1
      warning("The image contains more than one frame: only the first one is displayed. To display all frames use 'all = TRUE'.")
    }
    else
      if ( frame<1 || frame>nf ) stop("Incorrect 'frame' number: It must range between 1 and ", frame)

    plot(c(1, xdim), c(1, ydim), type = "n", xlab="", ylab="", asp=1)

    ## plot the figure as a raster image
    rasterImage(getFrame(image, frame, type='render'), 1, 1, xdim, ydim, interpolate=TRUE)
   }

  ## restore saved graphical parameters
  par(op)

  invisible(TRUE)
}
