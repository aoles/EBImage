colormap <- function(x, palette = heat.colors(256L)) {
  validImage(x)
  if (colorMode(x) != Grayscale) 
    stop("Color mapping can be applied only to grayscale images")
  
  tmp <- round(1 + x * (length(palette)-1L))
  tmp <- Image(array(palette[tmp], dim(tmp)))
  
  if ( is.Image(x) ) {
    res <- x
    imageData(res) <- tmp
    colorMode(res) <- Color
  }
  else {
    res <- tmp
  }
  
  res
}
