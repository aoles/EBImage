\name{drawCircle}

\alias{drawCircle}

\title{Draw a circle on an image.}

\description{
  Draw a circle on an image.
}

\usage{
  drawCircle(img, x, y, radius, col, fill=FALSE, z=1)
}

\arguments{
\item{img}{An \code{Image} object or an array.}  
\item{x, y, radius}{numerics indicating the center and the radius
    of the circle.}
  
\item{col}{A numeric or a character string specifying the color
    of the circle.}
  
\item{fill}{A logical indicating whether the circle should be filled.
    Default is \code{FALSE}.}
  
\item{z}{A numeric indicating on which frame of the image the circle
    should be drawn. Default is 1.}
}

\value{
  An \code{Image} object or an array, containing the transformed version
  of \code{img}.
}

\author{
  Gregoire Pau, 2010
}

\examples{
  ## Simple white circle
  x = matrix(0, nrow=300, ncol=300)
  y = drawCircle(x, 100, 200, 47, col=1)
  if (interactive()) display(y)
  
  ## Simple filled yellow circle
  x = channel(y, 'rgb')
  y = drawCircle(x, 200, 140, 57, col='yellow', fill=TRUE)
  if (interactive()) display(y)
}


