\name{closing}

\alias{dilate}
\alias{dilate,Image-method}
\alias{closing}
\alias{closing,Image-method}
\alias{erode}
\alias{erode,Image-method}
\alias{opening}
\alias{opening,Image-method}
\alias{morphKern}

\concept{morphology}

\title{ Morphological operations on images }

\description{
  Functions to perform morphological operations on binary images.
}

\usage{
  
  \S4method{dilate}{Image}(x, kern=morphKern(5), iter=1, ...)
  \S4method{erode}{Image}(x, kern=morphKern(5), iter=1, ...)
  \S4method{opening}{Image}(x, kern=morphKern(5), iter=1, ...)
  \S4method{closing}{Image}(x, kern=morphKern(5), iter=1, ...)

  morphKern(size=5, shape="round")
}

\arguments{

  \item{x}{ An object of \code{\linkS4class{Image}}. \code{x} should
    be a binary image in the \code{\link{Grayscale}} mode. If image is
    not binary, all non-zero pixels will be considered as 1 to turn the image
    into a binary \code{\{0,1\}} image. }

  \item{kern}{ Kernel mask matrix. }

  \item{iter}{ Number of iterations. }

  \item{size, shape}{ Kernel matrix size and shape. }

  \item{...}{ Reserved. }

}

\value{

  A transformed image in an object of \code{\linkS4class{Image}}.
    
  \code{morphKern} returns a square matrix of 0 and 1 of a given size.
  
}

\details{

  \code{morphKern} can be used to generate a kernel matrix for the use with any
  of the morphological operators. The function can generate round and square 
  kernels of odd size, e.g. 5, 7, 9 etc. Even sizes are not supported because
  the location of the centre pixel is undefined. Kernels can be altered in any
  desired way, this is just a convenience function.
  
  \code{erode} applies the mask positioning its centre over every background pixel 
  (0), every pixel which is not covered by the mask is reset to foreground (1).
  In this way image features grow in size.
  
  \code{dilate} applies the mask positioning its centre over every foreground pixel
  (1), every pixel which is not covered by the mask is reset to background (0).
  In this way image features seem shrink in size.

  \code{opening} is erosion followed by dilation and \code{closing} is dilation
  followed by erosion.
  
}

\seealso{ \code{
    \linkS4class{Image}, \link{thresh}, \link{segment}
}}

\references{
  \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2006
}

\examples{
  ## see example(propagate)
}

\keyword{manip}

