\name{bwlabel}

\alias{bwlabel}

\title{Binary segmentation}

\description{
  Labels connected (connected sets) objects in a binary image.
}

\usage{
  bwlabel(x)
}

\arguments{
  \item{x}{An \code{Image} object or an array. \code{x} is considered as
    a binary image, whose pixels of value 0 are considered as background
    ones and other pixels as foreground ones.}
}

\value{
  An \code{Image} object or an array, containing the labelled version
  of \code{x}.
}

\details{
  Each connected set of foreground (non-zero) pixels in \code{x} is set
  to an unique increasing integer, starting from 1. Hence, \code{max(x)}
  tells you the number of connected objects in \code{x}.
}

\author{
  Gregoire Pau, 2009
}

\examples{
  x = readImage(system.file('images', 'nuclei.tif', package='EBImage'))
  if (interactive()) display(x)

  ## computes binary mask
  y = thresh(x, 10, 10, 0.05)
  y = opening(y, makeBrush(5, shape='disc'))
  if (interactive()) display(y, title='Cell nuclei binary mask')

  ## bwlabel
  z = bwlabel(y)
  if (interactive()) display(normalize(z), title='Cell nuclei')
  nbnuclei = apply(z, 3, max)
  cat('There are', paste(nbnuclei, collapse=','),'nuclei in the
  images.\n')

  ## recolor nuclei in colors
  col = matrix(as.numeric(col2rgb(c('black', sample(rainbow(max(z)))))),  nr=3)/255
  red = Image(col[1, 1+z], dim=dim(z))
  green = Image(col[2, 1+z], dim=dim(z))
  blue = Image(col[3, 1+z], dim=dim(z))
  zrainbow = rgbImage(red, green, blue)
  if (interactive()) display(zrainbow, title='Cell nuclei (recolored)')
}
