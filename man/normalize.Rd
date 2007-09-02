\name{normalize}

\alias{negate}
\alias{negate,Image-method}
\alias{normalize}
\alias{normalize,Image-method}
\alias{normalize2}
\alias{normalize2,Image-method}

\concept{normalization}

\title{ Functions to normalize images }

\description{
  Functions to normalize images.
}

\usage{

  \S4method{normalize}{Image}(x, separate=TRUE, ft=c(0,1), ...)

  ## S4 methods for signature 'x=Image'
  negate(x, ...)
  normalize2(x, ...)
  
}

\arguments{

  \item{x}{ An object of \code{\link{Image-class}}. }

  \item{separate}{ If \code{TRUE} normalizes each frame separately. }

  \item{ft}{ A numeric vector of 2 values, target minimum and maximum 
    intensity values after normalization for \code{\link{Grayscale}} images. }

  \item{...}{ Reserved. }

}

\value{

  A transformed image in an object of \code{\linkS4class{Image}}.
    
}

\details{

  \code{negate} negates the colors in the reference image. Operates on all image
    modes.
  
  \code{normalize} normalizes \code{\link{Grayscale}} images to the given range.
    
  \code{normalize2} uses \code{ImageMagick} normalization routine to normalize
    \code{\link{Grayscale}} or \code{\link{TrueColor}} images.
   
}

\seealso{ \code{
  \linkS4class{Image}, \link{channel}
}}

\references{
  \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2006-2007
}

\examples{
  ## see example(getFeatures)
}

\keyword{manip}

