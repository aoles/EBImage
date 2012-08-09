## TODO: rotation of images (notice that lena is sideways)

collapseTypeSynonyms = function(x){
    x[x=="tif"] = "tiff"
    x[x=="jpg"] = "jpeg"
    return(x)
}

readImage2 = function(files, colormode="GrayScale", type) {
  if(!(is.character(files)&&(length(files)>=1)))
    stop("'files' must be a character vector of length >=1.")

  if (missing(type)) {
    type = unique(collapseTypeSynonyms(sapply(strsplit(tolower(files), split=".", fixed=TRUE), function(v) v[length(v)])))
    if(length(type)>1)
      stop(sprintf("The file type implied by the file name extensions must be unique, but more than one type was found: %s.",
                   paste(type, collapse=", ")))
  } else {
    if(!(is.character(type)&&(length(type)==1)))
      stop("'type' must be a character vector of length 1.")
  }

  ## check colormode
  ##numDim = switch(colormode,
  ##  `GrayScale`=2,
  ##  `Color`=3,
  ##  stop(sprintf("Invalid colormode '%s'.", colormode)))

  readFun = switch(type,
      tiff = function(x) readTIFF(x, all=TRUE),
      jpeg = readJPEG,
      png  = readPNG,
      stop(sprintf("Invalid type '%s'.", type))
     )

  for(i in seq(along=files)){
    im = readFun(files[i])

    ## readTIFF returns a list for stacked tiffs. Here they are collapsed to a single array.
    if(is.list(im))
     im = combine(im)

    if(i==1) {
      theDim = dim(im)
      ##if((length(theDim))!=numDim)
      ##  stop(sprintf("colormode is '%s', and a %d-dimensional image is expected, but '%s' has %d dimensions.",
      ##       colormode, numDim, files[i], length(theDim)))
      a = array(NA_real_, dim=c(theDim, length(files)))
    } else {
      if(!identical(dim(im), theDim))
        stop(paste("The images need to have the same dimensions, but '", files[1],
                   "' has dimension [", paste(theDim,  collapse=" "), "] while '", files[i],
                   "' has dimension [", paste(dim(im), collapse=" "), "].", sep=""))
    }
    a = do.call(`[<-`, c(list(a), lapply(theDim, function(x) TRUE), list(i, im)))
  }

  Image(a, dim=dim(a), colormode=colormode)
}

