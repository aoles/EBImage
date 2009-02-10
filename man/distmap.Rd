\name{distmap}

\alias{distmap}
\alias{distmap,Image-method}

\title{Distance map transform}

\description{
  Computes the distance map transform of a binary image.
  The distance map is a matrix which contains for each pixel
  the distance to its nearest background pixel.
}

\usage{
  \S4method{distmap}{Image}(x, metric=c('euclidean','manhattan'))
}

\arguments{

  \item{x}{An \code{Image} object or an array.
    \code{x} is considered as a binary image, whose pixels of value 0
    are considered as background ones and other pixels as
    foreground ones.}

  \item{metric}{A character indicating which metric to use, L2 distance
    (Euclidean) or L1 distance (Manhattan). Default is \code{euclidean}.} 
}

\value{
  An \code{Image} object or an array, with pixels
  containing the distances to the nearest background points. 
}

\details{
  A fast algorithm of complexity O(N*M*log(N)), where (M,N) are the
  dimension of \code{x}), is used to compute the distance map.
}

\references{M. N. Kolountzakis, K. N. Kutulakos. Fast Computation of the Euclidean
  Distance Map for Binary Images, Infor. Proc. Letters 43 (1992).}

\seealso{
  \code{\linkS4class{Image}}
}

\author{
  Gregoire Pau, \email{gpau@ebi.ac.uk}, 2008
}

\examples{
  \dontrun{see ?watershed for an example}
}


