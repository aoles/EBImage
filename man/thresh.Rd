\name{thresh}

\alias{thresh}

\title{Adaptive thresholding}

\description{
  Thresholds an image using a moving rectangular window.
}

\usage{
thresh(x, w=5, h=5, offset=0.01)
}

\arguments{
  \item{x}{An \code{Image} object or an array.}
  
  \item{w, h}{Width and height of the moving rectangular window.} 
  
  \item{offset}{Thresholding offset from the averaged value.}
}

\value{
  An \code{Image} object or an array, containing the transformed version
  of \code{x}.
}

\details{
  This function returns the binary image resulting from the comparison
  between an image and its filtered version with a rectangular window.
  It is equivalent of doing \code{\{f = matrix(1, nc=2*w+1, nr=2*h+1) ; f=f/sum(f) ; x>(filter2(x, f)+offset)\}} 
  but slightly faster. The function \code{filter2} provides hence more 
  flexibility than \code{thresh}.
}

\seealso{
  \code{filter2}
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk},  2005-2007
}

\examples{
  x = readImage(system.file('images', 'nuclei.tif', package='EBImage'))
  display(x)
  y = thresh(x, 10, 10, 0.05)
  display(y)
}

\keyword{manip}
