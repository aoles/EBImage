colormap <- function(x, palette = heat.colors(256L)) {
  validImage(x)
  if (colorMode(x) != Grayscale) 
    stop("Color mapping can be applied only to grayscale images")
  
  x <- round(1 + x * (length(palette)-1L))
  
  Image(array(palette[x], dim(x)))
}
