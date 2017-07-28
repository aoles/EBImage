## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
display = function(x, method, ...) {
  validImage(x)
  
  if ( missing(method) )
    method = getOption("EBImage.display", if ( interactive() ) "browser" else "raster")
  method = match.arg(method, c("browser", "raster"))
  
  switch(method,
         browser = displayWidget(x, ...),
         raster  = displayRaster(x, ...)
  ) 
}

## displays an image using R graphics
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
displayRaster = function(image, frame, all = FALSE, drawGrid = isTRUE(spacing==0),
                         nx, spacing = 0, margin = 0, interpolate = TRUE,
                         title, ...) {
  all = isTRUE(all)
  nf = numberOfFrames(image, type="render")
  
  ## display all frames in a grid-like environment
  if ( all ) {
    if ( missing(nx) )
      ncol = ceiling(sqrt(nf))
    else {
      ncol = as.integer(nx[1L])
      if ( ncol==0L ) stop( "'nx' must be coercible to a non-zero integer" )
    }
    
    ## negative values are interpreted as number of rows
    if ( ncol<0 ) {
      nrow = -ncol
      ncol = ceiling(nf/nrow)
    } else {
      nrow = ceiling(nf/ncol)
    }
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
  
  d <- dim(image)[1:2]
  xdim <- d[1L]
  ydim <- d[2L]
  
  ## returns a pair of horizontal and vertical pixel dimensions
  asPixelDims <- function(x, d) {
    x <- as.numeric(x)
    x[ is.na(x) | x < 0] <- 0L
    
    ## values smaller than one are interpreted as fractions of image dimensions
    frac = which( x>0 & x<1 )
    x[frac] <- (d*x)[frac]
    
    ## single fraction is taken against the maximum dimension
    if ( identical(frac, 1L) )
      x = x * max(d)/d
    
    ## round to integer pixel values
    x = as.integer(round(x))
    x = rep(x, length.out=2L)
  }
  
  spacing <- asPixelDims(spacing, d)
  margin  <- asPixelDims(margin, d)
  
  ## draw grid unless spacing is set
  if ( missing(drawGrid) )
    drawGrid = if ( any(spacing)>0 ) FALSE else TRUE
  
  xmar <- margin[1L]
  ymar <- margin[2L]
  
  xsep <- spacing[1L]
  ysep <- spacing[2L]
  
  xran = c(0, ncol*xdim + (ncol-1)*xsep) + .5
  yran = c(0, nrow*ydim + (nrow-1)*ysep) + .5
  
  xranm = xran + c(-xmar, xmar)
  yranm = yran + c(-ymar, ymar)
  
  ## set graphical parameters
  user <- par(bty="n", mai=c(0,0,0,0), xaxs="i", yaxs="i", xaxt="n", yaxt="n", col = "white", ...)
  on.exit(par(user))
  plot(xranm, yranm, type="n", xlab="", ylab="", asp=1, ylim=rev(yranm))
      
  for(r in seq_len(nrow)) {
    for(c in seq_len(ncol)) {
      f = if(all) (r-1)*ncol + c else frame
      if ( f <= nf )
        rasterImage(as.nativeRaster(getFrame(image, f, type="render")),
                    (c-1)*(xdim+xsep) + .5,
                    r*(ydim+ysep)-ysep + .5,
                    c*(xdim+xsep)-xsep + .5,
                    (r-1)*(ydim+ysep) +.5,
                    interpolate = interpolate)
      else
        break
    }
  }    
  
  ## draw grid lines
  if ( isTRUE(drawGrid) && all && nf>1 ) {
    clip(xran[1L], xran[2L], yran[1L], yran[2L])
    abline(h = seq_len(nrow-1)*(ydim + ysep) - ysep/2 + .5,
           v = seq_len(ncol-1)*(xdim + xsep) - xsep/2 + .5)
  }
  
  invisible()
}

## displays an image using JavaScript
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
displayInBrowser = function(x, title, ...) {
  if ( missing(title) )
    title = deparse(substitute(x, parent.frame()), width.cutoff = 500L, nlines = 1L)
  
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
  nf = numberOfFrames(x, "render")

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

plot.Image = function(x, ...) displayRaster(x, ...)

as.nativeRaster = function(x) .Call(C_nativeRaster, castImage(x))

## Display Widget

displayWidget <- function(x, embed = !interactive(), tempDir = tempfile(""), ...) {
  
  ## get image parameters
  d = dim(x)
  if ( length(d)==2L ) d = c(d, 1L)
  
  ## fill missing channels
  if ( isTRUE(colorMode(x) == Color && d[3L] < 3L) ) {
    fd = d
    fd[3L] = 3L - d[3L]
    imageData(x) = abind(x, Image(0, fd), along = 3L)
  }
  
  nf = numberOfFrames(x, type='render')
  colormode = colorMode(x)
  
  x = clipImage(x) ## clip the image and change storage mode to double
  x = transpose(x)
  
  frames = seq_len(nf)
  dependencies = NULL
  
  if ( isTRUE(embed) ) {
    
    data <- sapply(frames, function(i) base64Encode(writePNG(getFrame(x, i, 'render'))))
    data <- sprintf("data:image/png;base64,%s", data)
    
  } else {
    if ( !dir.exists(tempDir) && !dir.create(tempDir, recursive=TRUE) )
        stop("Error creating temporary directory.")
    
    files = file.path(tempDir, sprintf("frame%.3d.png", frames, ".png"))
    
    ## store image frames into individual files
    for (i in frames)
      writePNG(getFrame(x, i, 'render'), files[i])
    
    dependencies = htmlDependency(
      name = basename(tempDir),
      version = "0",
      src = list(tempDir)
    )
    
    filePath = file.path(sprintf("%s-%s", dependencies$name, dependencies$version), basename(files))
    
    ## set libdir unless run in shiny

    if ( !isNamespaceLoaded("shiny") || is.null(shiny::getDefaultReactiveDomain()))
      filePath = file.path("lib", filePath)
    
    data = filePath
  }
  
  # widget options
  opts = list(
    data = data,
    width = d[1L],
    height = d[2L]
  )
  
  # create widget
  createWidget(
    name = 'displayWidget',
    package = 'EBImage',
    x = opts,
    sizingPolicy = sizingPolicy(padding = 0, browser.fill = TRUE),
    dependencies = dependencies,
    ...
  )
  
}

## Shiny bindings for displayWidget

displayOutput <- function(outputId, width = '100%', height = '500px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'displayWidget', width, height, package = 'EBImage')
}

renderDisplay <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, displayOutput, env, quoted = TRUE)
}

