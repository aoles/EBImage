\name{thresh}

\alias{thresh}

\title{Adaptive thresholding}

\description{
  Threshold an image using a moving rectangular window.
}

\usage{
  thresh(x, w=5, h=5, offset=0.01)
}

\arguments{
  \item{x}{An \code{Image} object or an array.}
  
  \item{w, h}{Width and height of the moving rectangular window.} 
  
  \item{offset}{Thresholding offset from the mean value.}
}

\value{
  An \code{Image} object or an array, containing the transformed version
  of \code{x}.
}

\details{
  
}

\seealso{
  \code{filter2}
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk},  2005-2007
}

\examples{
 
}

\keyword{manip}

