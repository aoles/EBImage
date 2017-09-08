\name{floodFill}

\alias{floodFill}

\title{Region filling}

\description{
  Fill regions in images.
}

\usage{
floodFill(x, pts, col, tolerance=0)
}

\arguments{
  \item{x}{An \code{Image} object or an array.}

  \item{pts}{Coordinates of the start filling points given as either of the following: a vector of the form \code{c(x1, y1, x2, y2, ...)}, a list of points, a matrix or data frame where rows represent points and columns are the x and y coordinates. For image stacks different points for each frame can be specified by providing them in a list of length matching the number of 'render' frames.}

  \item{col}{Fill color. This argument should be a numeric for Grayscale images and an R color for Color images. Values are recycled such that their length matches the number of points in \code{pts}. Can be a list of length matching the number of 'render' frames similarly as \code{pts}.}

  \item{tolerance}{Color tolerance used during the fill.}
}

\value{
  An \code{Image} object or an array, containing the transformed version
  of \code{x}.
}

\details{
  Flood fill is performed using the fast scan line algorithm. Filling
  starts at \code{pts} and grows in connected areas where the absolute
  difference of the pixels intensities (or colors) remains below
  \code{tolerance}.
}

\author{
  Gregoire Pau, Oleg Sklyar; 2007
}

\examples{
  x = readImage(system.file("images", "shapes.png", package="EBImage"))

  ## fill a shape with 50% shade of gray
  y = floodFill(x, c(67, 146), 0.5)
  display(y)
  
  ## fill with color
  y = toRGB(y)
  y = floodFill(y, c(48, 78), 'orange')	
  display(y)
  
  ## fill multiple shapes with different colors
  y = y[110:512,1:130,]
  points = rbind(c(50, 50), c(100, 50), c(150, 50))
  colors = c("red", "green", "blue")
  y = floodFill(y, points, colors)
  display(y)
  
  ## area fill
  x = readImage(system.file("images", "sample.png", package="EBImage"))
  y = floodFill(x, rbind(c(200, 400), c(200, 325)), 1, tolerance=0.1)
  display(y)
  
  ## application to image stacks
  f = system.file("images", "nuclei.tif", package="EBImage")
  x = readImage(f)[1:250,1:250,]
  x = opening(thresh(x, 12, 12), makeBrush(5, shape='disc'))
  xy = lapply(getFrames(bwlabel(x)), function(x) computeFeatures.moment(x)[,1:2])
  y = floodFill(toRGB(x), xy, c("red", "green", "blue"))
  display(y)
}
