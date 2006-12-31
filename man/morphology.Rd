\name{closing}

\alias{dilate}
\alias{closing}
\alias{erode}
\alias{morphKern}
\alias{opening}

\concept{morphology}

\title{ Morphological operations on images }

\description{
  Functions to perform morphological operations on binary images.
}

\section{ Usage }{
  \preformatted{
    dilate(x, kern=morphKern(5), iter=1, ...)
    erode(x, kern=morphKern(5), iter=1, ...)
    opening(x, kern=morphKern(5), iter=1, ...)
    closing(x, kern=morphKern(5), iter=1, ...)

    morphKern(size=5, shape="round")
  }
}

\section{ Arguments }{ \describe{

  \item{\code{x}}{ An object of \code{\link{Image-class}}. \code{x} should
    be a binary image in the \code{\link{Grayscale}} mode. If image is
    not binary, all non-zero pixels will be considered as 1 to turn the image
    into a binary \code{\{0,1\}} image. }

  \item{\code{kern}}{ Kernel mask matrix. }

  \item{\code{iter}}{ Number of iterations. }

  \item{\code{size, shape}}{ Kernel matrix size and shape. }

  \item{\code{...}}{ Reserved for future use. }
}}

\section{ Value }{

  A transformed image in an object of \code{\link{Image-class}}.
    
  \code{morphKern} returns a square matrix of 0 and 1 of a given size.
  
}

\section{ Details }{

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

\seealso{
  \code{
    \link{Image-class}, \link{thresh}, \link{segment}
  }
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Copyright (c) 2005-2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\examples{
  \dontrun{see ?watershed for examples}
}

\keyword{file}

