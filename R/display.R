## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
display = function(x, 
                   title = deparse(substitute(x), width.cutoff = 500L, nlines = 1), 
                   method = if ( interactive() ) "browser" else "raster",
                   frame, all = FALSE) {
  validImage(x)
  method = match.arg(method, c("browser", "raster"))

  switch(method,
    browser = displayInBrowser(x, title),
    raster  = displayRaster(x, frame, all) ) 

  invisible(NULL)
}

## displays an image using R graphics
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
displayRaster = function(image, frame, all = FALSE, drawGrid = TRUE, ...){
  all = isTRUE(all)
  nf = .numberOfFrames(image, type="render")
  
  ## display all frames in a grid-like environment
  if ( all ) {
    ncol = ceiling(sqrt(nf))
    nrow = ceiling(nf/ncol)
  }
  ## display a single frame only
  else {
    ## when the image contains a single frame only display it and don't care about the 'frame' argument at all
    if (nf==1)
      frame = 1
    else 
      if (missing(frame)) {
        frame = 1
        message("The image contains more than one frame: only the first one is displayed. To display all frames use 'all = TRUE'.")
      }
    else
      if ( frame<1 || frame>nf ) stop("Incorrect number of frame: it must range between 1 and ", frame)
    
    ncol = nrow = 1L
  }
  
  dim <- dim(image) 
  xdim <- dim[1]
  ydim <- dim[2]
  
  xran = c(0, ncol*xdim) + .5
  yran = c(0, nrow*ydim) + .5
  
  ## set graphical parameters
  par(bty="n", mai=c(0,0,0,0), xaxs="i", yaxs="i", xaxt="n", yaxt="n")    
  plot(xran, yran, type="n", xlab="", ylab="", asp=1, ylim=rev(yran))
  
  colormode = colorMode(image)
  img = imageData(image)
  
  for(r in seq_len(nrow)) {
    for(c in seq_len(ncol)) {
      f = if(all) (r-1)*ncol + c else frame
      if ( f <= nf )
        rasterImage(as.nativeRaster(.getFrame(img, f, type="render", colormode = colormode)), (c-1)*xdim + .5, r*ydim + .5, c*xdim + .5, (r-1)*ydim +.5, ...)
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
displayInBrowser = function(x, title){
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

  browseURL(query)
}

## take plain array as input
as.nativeRaster = function(x) {
  x = clipImage(x)
  x = round(x * 255)
  d = dim(x)
  
  y = if(length(d) == 3L)
    Reduce("+", lapply(seq_len(4), function(i) 2^((i-1)*8) * if (i > d[3L]) { if ( i == 4L) 255 else 0 } else x[,,i]) ) - 4294967296
  else
    x + x * 256 + x * 65536 + 255 * 16777216 - 4294967296
  storage.mode(y) = "integer"
  # native raster representation contains already transposed image data as in the Image class, but with swapped dimensions!
  attr(y, "dim") = dim(y)[2:1]
  attr(y, "class") = "nativeRaster"
  attr(y, "channels") = 4L
  y
}
