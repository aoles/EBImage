\name{equalize}

\alias{equalize}

\concept{equalization}

\title{Histogram Equalization}

\description{
  Equalize the image histogram to a specified range and number of levels.
}

\usage{
equalize(x, range = c(0, 1), levels = 256)
}

\arguments{
  \item{x}{an \code{Image} object or an array}

  \item{range}{numeric vector of length 2, the output range of the equalized histogram}

  \item{levels}{number of grayscale levels (Grayscale images) or intensity levels for each channel (Color images)}
}

\value{
  An \code{Image} object or an array, containing the transformed version
  of \code{x}.
}

\details{
 Individual channels of \code{Color} images and frames of image stacks are equalized separately.
}

\author{
  Andrzej Oles, \email{andrzej.oles@embl.de}, 2014
}

\examples{
  x = readImage(system.file('images', 'cells.tif', package='EBImage'))
  hist(x)
  y = equalize(x)
  hist(y)
  display(y, title='Equalized Grayscale Image')

  x = readImage(system.file('images', 'lena-color.png', package='EBImage'))
  hist(x)
  y = equalize(x)
  hist(y)
  display(y, title='Equalized Grayscale Image')
}
