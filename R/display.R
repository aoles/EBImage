## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
display = function(x, 
                   title = deparse(substitute(x), width.cutoff = 500L, nlines = 1), 
                   method,
                   frame, all = FALSE, ...) {
  validImage(x)
  if ( missing(method) )
    method = getOption("EBImage.display", if ( interactive() ) "browser" else "raster")
  method = match.arg(method, c("browser", "raster"))

  switch(method,
    browser = displayInBrowser(x, title, ...),
    raster  = displayRaster(x, frame, all, ...) ) 

  invisible()
}

## displays an image using R graphics
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
displayRaster = function(image, frame, all = FALSE, drawGrid = TRUE, padding = 0, ...){
  all = isTRUE(all)
  nf = .numberOfFrames(image, type="render")
  
  ## display all frames in a grid-like environment
  if ( all ) {
    ncol = ceiling(sqrt(nf))
    nrow = ceiling(nf/ncol)
  }
  ## display a single frame only
  else {
    if ( missing(frame) ) {
      frame = 1L
      if ( nf > 1L ) message("Only the first frame of the image stack is displayed.\nTo display all frames use 'all = TRUE'.")
    }
    else {
      frame = as.integer(frame[1L])
      if ( frame<1L || frame>nf ) stop( "Frame index out of range: 'frame' must be between 1 and ", nf)
    }
    ncol = nrow = 1L
  }
  
  d <- dim(image) 
  xdim <- d[1L]
  ydim <- d[2L]
  
  padding <- as.numeric(padding)
  padding[is.na(padding)] <- 0L
  padding <- padding[1:2]
  
  if ( padding[1L] > 0 )
    drawGrid = FALSE
  
  ## values greater than one are intepreted as width in pixels ...
  if ( padding[1L] >= 1 ) {
    xsep = round(padding[1L])
    ysep = if (is.na(padding[2L])) xsep else round(padding[2L])
  }
  ## .. and smaller as fractions of frame dimensions
  else if ( padding[1L] >= 0 ){
    if (is.na(padding[2L]))
      padding[2L] = padding[1L] * max(xdim, ydim)/min(xdim, ydim)
    xsep = round(xdim * padding[1L])
    ysep = round(ydim * padding[2L])
  }  
  else {
    xsep = ysep = 0
  }
  
  xran = c(0, ncol*xdim + (ncol-1)*xsep) + .5
  yran = c(0, nrow*ydim + (nrow-1)*ysep) + .5
  
  ## set graphical parameters
  user <- par(bty="n", mai=c(0,0,0,0), xaxs="i", yaxs="i", xaxt="n", yaxt="n")
  on.exit(par(user))
  plot(xran, yran, type="n", xlab="", ylab="", asp=1, ylim=rev(yran))
      
  for(r in seq_len(nrow)) {
    for(c in seq_len(ncol)) {
      f = if(all) (r-1)*ncol + c else frame
      if ( f <= nf ) rasterImage(as.nativeRaster(getFrame(image, f, type="render")), (c-1)*(xdim+xsep) + .5, r*(ydim+ysep)-ysep + .5, c*(xdim+xsep)-xsep + .5, (r-1)*(ydim+ysep) +.5, ...)
      else
        break
    }
  }    
  
  ## draw grid lines in case of multiple frames
  if( all && isTRUE(drawGrid) && nf>1 ) {
    clip(xran[1], xran[2], yran[1], yran[2])
    abline(h = seq_len(nrow-1)*ydim + .5, v = seq_len(ncol-1)*xdim + .5, col = "white")
  }
}

## displays an image using JavaScript
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
displayInBrowser = function(x, title, ...){
  ## template and script files
  templateFile = system.file("viewer","display.template", package = "EBImage")
  cssFile = system.file("viewer","display.css", package = "EBImage")
  scriptFile = system.file("viewer","viewer.js", package = "EBImage")
  tempDir = tempfile("",,"")
  htmlFile = "display.html"
  imageFile = tempfile("",tempDir,".png")

  if(!dir.create(tempDir))
    stop("Error creating temporary directory.")

  ## read the template file
  f = file(templateFile, "r")
  a = readLines(f)
  close(f)

  ## get image parameters
  d = dim(x)
  if ( length(d)==2L ) d = c(d, 1L)
  nf = .numberOfFrames(x, "render")

  ## fill-in in the template
  a = sub("HEIGHT",d[2L], sub("WIDTH",d[1L], sub("FRAMES",nf, sub("IMAGE",basename(imageFile), sub("TITLE", title, a)))))

  ## temporarily switch to tempdir and write the files
  wd = setwd(tempDir)
  
  ## fill missing channels
  if ( isTRUE(colorMode(x) == Color && d[3L] < 3L) ) {
    fd = d
    fd[3L] = 3L - d[3L] 
    imageData(x) = abind(x, Image(0, fd), along = 3L)
  }
  
  writeImage(x, imageFile)
  cat(a, file=htmlFile, sep="\n")
  file.copy(scriptFile, tempDir)
  file.copy(cssFile, tempDir)
  setwd(wd)

  ## create browser query
  query = paste0("file://", normalizePath(file.path(tempDir,"display.html")))

  browseURL(query, ...)
}

as.nativeRaster = function(x) .Call(C_nativeRaster, castImage(x))
