# defined outside of `setMethod` in order to subsequently call `formals` on it
.abind.Image <- function(...) {
  arglist = list(...)
  
  if (is.list(arglist[[1L]]))
    arglist <- arglist[[1L]]
  
  x <- arglist[[1L]]
  
  cm = colorMode(x)
  
  if (!all(cm==vapply(arglist, colorMode, integer(1L), USE.NAMES=FALSE)))
    stop("images have different color modes")
  
  y <- callNextMethod()
  
  ## don't introduce unnecessary dimnames
  if (all(vapply(arglist, function(x) is.null(dimnames(x)), logical(1L), USE.NAMES=FALSE)))
    dimnames(y) <- NULL
  
  ## use first object as template which might not be correct for Image subclasses
  imageData(x) <- y
  
  x
}


## neccesary for performing correct dispatch on object lists
.abind.list <- function(...) {
  cl <- match.call(expand.dots = FALSE)
  
  ## only a single list-valued argument for ... is supported
  if (length(cl$`...`)!=1L) {
    callNextMethod()
  }
  else {
    dots <- list(...)[[1L]]
    
    cl$`...` <- quote(dots)
  
    ## choose the apropriate method based on object classes
    cls <- unique(unlist(lapply(dots, class)))
    cl[[1L]] <- quote(selectMethod(abind, cls))
  
    eval(cl)
  }
}


# have the same formal arguments as the original function
formals(.abind.Image) <- formals(abind::abind)
formals(.abind.list) <- formals(abind::abind)

setMethod("abind", "Image", .abind.Image)
setMethod("abind", "list", .abind.list)
