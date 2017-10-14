# defined outside of `setMethod` in order to subsequently call `formals` on it
.abind.Image <- function(...) {
  arglist = list(...)
  
  if ( is.list(arglist[[1L]]) ) {
    if ( length(arglist)!=1L )
      stop("can only supply one list-valued arguments for ...")
    arglist <- arglist[[1L]]
  }
  
  cm = colorMode(arglist[[1L]])
  
  if ( !all(cm==vapply(arglist, colorMode, integer(1L), USE.NAMES=FALSE)) )
    stop("images have different color modes")
  
  y <- callNextMethod()
  
  ## don't introduce unnecessary dimnames
  if ( all(vapply(arglist, function(x) is.null(dimnames(x)), logical(1L), USE.NAMES=FALSE)) )
    dimnames(y) <- NULL
  
  Image(y, colormode=cm)
}

# have the same formal arguments as the original function
formals(.abind.Image) <- formals(abind::abind)

setMethod("abind", "Image", .abind.Image)
