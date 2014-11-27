\name{floodFill}

\alias{floodFill}

\title{Region filling}

\description{
  Fill regions in images.
}

\usage{
floodFill(x, pt, col, tolerance=0)
}

\arguments{
  \item{x}{An \code{Image} object or an array.}

  \item{pt}{Coordinates of the start filling point.}

  \item{col}{Fill color. This argument should be a numeric for Grayscale images
    and an R color for Color images.}

  \item{tolerance}{Color tolerance used during the fill.}
}

\value{
  An \code{Image} object or an array, containing the transformed version
  of \code{x}.
}

\details{
  Flood fill is performed using the fast scan line algorithm. Filling
  starts at \code{pt} and grows in connected areas where the absolute
  difference of the pixels intensities (or colors) remains below
  \code{tolerance}.
}

\author{
  Gregoire Pau, Oleg Sklyar; 2007
}

\examples{
  x = readImage(system.file("images", "shapes.png", package="EBImage"))
  y = floodFill(x, c(67, 146), 0.5)
  display(y)

  y = channel(y, 'rgb')
  y = floodFill(y, c(48, 78), 'red')
  y = floodFill(y, c(156, 52), 'orange')	
  display(y)

  x = readImage(system.file("images", "sample.png", package="EBImage"))
  y = floodFill(x, c(226, 121), 1, tolerance=0.1)
  display(y)
}
