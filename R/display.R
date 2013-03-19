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
displayRaster = function(image, frame, all = FALSE){

  nf = getNumberOfFrames(image, type="render")

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
          rasterImage(getFrame(image, f, type="render"), 1 + ((c-1)*xdim) , 1 + ((r-1)*ydim), c*xdim, r*ydim, interpolate=TRUE)
          f = f + 1
        }
        else break
      }
    }
  }

  ## display a single frame only 
  else {
    ## when the image containas a single frame only display it and don't care about the 'frame' argument at all
    if (nf==1)
      frame = 1
    else 
      if (missing(frame)){
        frame = 1
        message("The image contains more than one frame: only the first one is displayed. To display all frames use 'all = TRUE'.")
      }
      else
        if ( frame<1 || frame>nf ) stop("Incorrect 'frame' number: It must range between 1 and ", frame)

    plot(c(1, xdim), c(1, ydim), type = "n", xlab="", ylab="", asp=1)

    ## plot the figure as a raster image
    rasterImage(getFrame(image, frame, type="render"), 1, 1, xdim, ydim, interpolate=TRUE)
   }

  ## restore saved graphical parameters
  par(op)
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
