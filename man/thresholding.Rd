\name{thresh}

\alias{thresh}
\alias{athresh}
\alias{cthresh}

\concept{image thresholding}

\title{ Image and color channel thresholding }

\description{
  Functions to threshold images and color channels of images.
}

\section{ Usage }{

  \code{thresh(x, w=5, h=5, offset=0.01, ...)}
  
  \code{athresh(x, w=10, h=10, offset=0, ...)}
  
  \code{cthresh(x, threshold=0, ...)}

}

\section{ Arguments}{ \describe{

  \item{\code{x}}{An object of \code{\link{Image-class}}. For \code{thresh} it must be
    in the \code{\link{Grayscale}} mode. }

  \item{\code{w, h}}{ Thresholding frame width and height in pixel. } 

  \item{\code{offset}}{ Threshold offset from the mean value. }

  \item{\code{threshold}}{ Threshold value for channel thresholding (uniform). }

  \item{...}{ Reserved for future use. }

} }

\section{ Value}{

  A new instance of \code{\link{Image-class}} in the same color mode as input.
    
}

\section{ Details}{

  If \code{thresh} and \code{athresh} are adaptive thresholind functions.
  While \code{athresh} can be used on both \code{\link{Grayscale}} and 
  \code{\link{TrueColor}} images, \code{thresh} can only be used on 
  \code{Grayscale} images, however it is significantly faster on them.
  
  The value of \code{offset} in \code{thresh} is on the same scale as data, i.e.
  the default value is selected assuming data in the range \code{[0,1]}. This
  value for \code{athresh} should be selected empirically, as this function
  uses \code{AdaptiveThreshold} of \code{ImageMagick} and the scale of this
  parameter was not documented; the reasonable values are usually in the orders
  of 500, 1000 or avobe (at least to obtain similar thresholding to that of
  0.01 with \code{thresh} on cytomicroscopic images. ).
  
}

\seealso{
  \code{
    \link{Image-class}, \link{Image}, \link{distmap}, \link{segment}, 
    \link{[,Image-method}
  }
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Copyright (c) 2005-2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

%\examples{
%}

\keyword{file}

