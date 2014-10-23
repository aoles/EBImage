\name{ocontour}

\alias{ocontour}
\title{Oriented contours}

\description{
  Computes the oriented contour of objects.
}

\usage{
ocontour(x)
}

\arguments{
  \item{x}{An \code{Image} object or an array, containing objects.
    Only integer values are considered. Pixels of value 0 constitute
    the background. Each object is a set of pixels with the same unique
    integer value. Objects are assumed connected.
  }
}

\value{
  A list of matrices, containing the coordinates of object oriented contours.
}

\author{
  Gregoire Pau, \email{gpau@ebi.ac.uk}, 2008
}

\examples{
  x = readImage(system.file("images", "shapes.png", package="EBImage"))
  x = x[1:120,50:120]
  display(x)
  oc = ocontour(x)
  plot(oc[[1]], type='l')
  points(oc[[1]], col=2)
}


