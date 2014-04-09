## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
display = function(x, title = deparse(substitute(x), width.cutoff = 500L, nlines = 1), method = c("browser", "raster"), frame, all = FALSE) {
  validImage(x)
  method = match.arg(method)

  switch(method,
    browser = displayInBrowser(x, title),
    raster  = displayRaster(x, frame, all) ) 

  invisible(NULL)
}

## displays an image using R graphics
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
displayRaster = function(image, frame, all = FALSE, drawGrid = TRUE, ...){
  all = isTRUE(all)
  nf = getNumberOfFrames(image, type="render")
  
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
      if ( frame<1 || frame>nf ) stop("Incorrect 'frame' number: It must range between 1 and ", frame)
    
    ncol = nrow = 1
  }
    
  dim <- dim(image) 
  xdim <- dim[1]
  ydim <- dim[2]
  
  xran = c(0, ncol*xdim) + .5
  yran = c(0, nrow*ydim) + .5
  
  ## set graphical parameters
  par(bty="n", mai=c(0,0,0,0), xaxs="i", yaxs="i", xaxt="n", yaxt="n")    
  plot(xran, yran, type="n", xlab="", ylab="", asp=1, ylim=rev(yran))
  
  for(r in seq_len(nrow)) {
    for(c in seq_len(ncol)) {
      f = if(all) (r-1)*ncol + c else frame
      if ( f <= nf )
        rasterImage(getFrame(image, f, type="render"), (c-1)*xdim + .5, r*ydim + .5, c*xdim + .5, (r-1)*ydim +.5, ...)
      else
        break
    }
  }    
    
  ## draw grid lines in case of multiple frames
  if( all && isTRUE(drawGrid) && nf>1 ) {
    clip(xran[1], xran[2], yran[1], yran[2])
    abline(h = seq_len(ncol-1)*xdim + .5, v = seq_len(ncol-1)*xdim + .5, col = "white")
  }
}

displayRaster.old = function(image, frame, all = FALSE, drawGrid = TRUE, ...){

  nf = getNumberOfFrames(image, type="render")

  dim <- dim(image) 
  xdim <- dim[1]
  ydim <- dim[2]
  
  ## set graphical parameters
  par(bty="n", mai=c(0,0,0,0), xaxs="i", yaxs="i", xaxt="n", yaxt="n")

  ## display all frames in a grid-like environment
  if (all){
    ncol = ceiling(sqrt(nf))
    nrow = ceiling(nf/ncol)

    xran = c(0, ncol*xdim) + .5
    yran = c(0, nrow*ydim) + .5
    
    plot(xran, yran, type="n", xlab="", ylab="", asp=1, ylim=rev(yran))

    f = 1
    for(r in seq_len(nrow)) {
      for(c in seq_len(ncol)) {
        ## plot the figure as a raster image
        if (f<=nf) {
          rasterImage(getFrame(image, f, type="render"), (c-1)*xdim + .5, r*ydim + .5, c*xdim + .5, (r-1)*ydim +.5, ...)
          f = f + 1
        }
        else break
      }
    }    
    
    if(isTRUE(drawGrid)){
      clip(xran[1], xran[2], yran[1], yran[2])
      abline(h = seq_len(ncol-1)*xdim + .5, v = seq_len(ncol-1)*xdim + .5, col = par()$bg)
    }
  }

  ## display a single frame only 
  else {
    ## when the image contains a single frame only display it and don't care about the 'frame' argument at all
    if (nf==1)
      frame = 1
    else 
      if (missing(frame)){
        frame = 1
        message("The image contains more than one frame: only the first one is displayed. To display all frames use 'all = TRUE'.")
      }
      else
        if ( frame<1 || frame>nf ) stop("Incorrect 'frame' number: It must range between 1 and ", frame)

    xran = c(0, xdim) + .5
    yran = c(0, ydim) + .5
    
    plot(xran, yran, type="n", xlab="", ylab="", asp=1, ylim=rev(yran))

    ## plot the figure as a raster image
    rasterImage(getFrame(image, frame, type="render"), xran[1], yran[2], xran[2], yran[1], ...)
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
  dims = dim(x)
  nf = getNumberOfFrames(x, "render")

  ## fill-in in the template
  a = sub("HEIGHT",dims[2], sub("WIDTH",dims[1], sub("FRAMES",nf, sub("IMAGE",basename(imageFile), sub("TITLE", title, a)))))

  ## temporarily switch to tempdir and write the files
  wd = setwd(tempDir) 
  writeImage(x, imageFile)
  cat(a, file=htmlFile, sep="\n")
  file.copy(scriptFile, tempDir)
  file.copy(cssFile, tempDir)
  setwd(wd)

  ## create browser query
  query = paste("file://", normalizePath(file.path(tempDir,"display.html")), sep="")

  browseURL(query)
}
