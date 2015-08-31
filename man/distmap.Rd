\name{distmap}

\alias{distmap}

\title{Distance map transform}

\description{
  Computes the distance map transform of a binary image.
  The distance map is a matrix which contains for each pixel
  the distance to its nearest background pixel.
}

\usage{
distmap(x, metric=c('euclidean', 'manhattan'))
}

\arguments{
  \item{x}{An \code{Image} object or an array.
    \code{x} is considered as a binary image, whose pixels of value 0
    are considered as background ones and other pixels as
    foreground ones.}

  \item{metric}{A character indicating which metric to use, L1 distance (\code{manhattan}) 
  or L2 distance (\code{euclidean}). Default is \code{euclidean}.} 
}

\value{
  An \code{Image} object or an array, with pixels
  containing the distances to the nearest background points. 
}

\details{
  A fast algorithm of complexity O(M*N*log(max(M,N))), where (M,N) are the
  dimensions of \code{x}, is used to compute the distance map.
}

\references{M. N. Kolountzakis, K. N. Kutulakos. Fast Computation of the Euclidean
  Distance Map for Binary Images, Infor. Proc. Letters 43 (1992).}

\author{
  Gregoire Pau, \email{gpau@ebi.ac.uk}, 2008
}

\examples{
  x = readImage(system.file("images", "shapes.png", package="EBImage"))
  display(x)
  dx = distmap(x)
  display(dx/10, title='Distance map of x')
}
