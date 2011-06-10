## display displays static images
display = function(x, title=paste(deparse(substitute(x))), useGTK=TRUE) {
  validImage(x)
  title = as.character(title)
  useGTK = as.logical(useGTK)
  stopifnot(length(useGTK)==1L, length(title)==1L)
  
  invisible(.Call("lib_display", castImage(x), title, useGTK, PACKAGE="EBImage"))
}

## animate displays animated sequences of images
animate = function (x) {
  validImage(x)
  invisible(.Call("lib_animate", castImage(x), PACKAGE="EBImage"))
}
