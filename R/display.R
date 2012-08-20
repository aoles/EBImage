## display displays static images
display = function(x, title=paste(deparse(substitute(x))), useGTK=TRUE) {
  validImage(x)
  title = as.character(title)
  useGTK = as.logical(useGTK)
  stopifnot(length(useGTK)==1L, length(title)==1L)
  
  invisible(.Call("lib_display", castImage(x), title, useGTK, PACKAGE="EBImage"))
}
