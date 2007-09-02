\name{watershed}

\alias{watershed}
\alias{watershed,Image-method}

\concept{watershed transformation}
\concept{watershed segmentation}
\concept{image segmentation}
\concept{object detection}

\title{ Watershed transformation and watershed based object detection }

\description{
  Watershed transformation and watershed based object detection.
}

\usage{

  \S4method{watershed}{Image}(x, tolerance=1, ext=1, ...)

}

\arguments{

  \item{x}{An object of \code{\linkS4class{Image}} in the
    \code{\link{Grayscale}} mode. }

  \item{tolerance}{ The minimum height of the object in the units of image
    intensity between its highest point (seed) and the point where it
    contacts another object (checked for every contact pixel). If the
    height is smaller than the tolerance, the object will be combined with
    one of its neighbors, which is the highest. It is assumed that the
    function is run on a distance map, therefore the default value
    is 1. If running the function on an original grayscale image with
    intensity range \code{[0,1]} one should modify this value, down to 0.1
    or other which is image specific. }

  \item{ext}{ Extension of the neighborhood for the detection
    of neighboring objects. Higher value smoothes out small objects. }

  \item{...}{ Reserved. }

}

\value{

  An object of \code{\linkS4class{Image}} in the \code{\link{Grayscale}} with
  separate objects indexed be positive integers starting from 1. To preview
  the results visually, use \code{\link{display}( \link{normalize}(result) )}
  or use it in combination with \code{\link{paintObjects}}.

}

\details{

  The algorithm identifies and separates objects that stand out of the
  background (zero), in other words to use the water fill, the source image
  is flipped upside down and the resulting valleys (values with higher
  intensities) are filled in first until another object or background is met.
  The deepest valleys (pixels with highest intensity) become indexed first.

}

\seealso{ \code{
  \linkS4class{Image}, \link{distmap}, \link{thresh}, \link{getFeatures},
  \code{matchObjects}
}}

\examples{
  
  ## FIXME: redo the example to new image files
  ddir <- paste( system.file(package="EBImage"), "images", sep="/" )
  a <- readImage( paste(ddir, "A04w1.jpg", sep="/") )

  w <- watershed( distmap( thresh(a, 10, 10) ) )
  if ( interactive() ) display( normalize(w) )

  ## often one needs to delete small objects
  ## here is the straightforward way to construct the index

  if ( interactive() ) display( w )

}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2007
}

\keyword{manip}

