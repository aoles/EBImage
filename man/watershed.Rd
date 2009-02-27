\name{watershed}

\alias{watershed}

\concept{watershed transformation}
\concept{watershed segmentation}
\concept{image segmentation}
\concept{object detection}

\title{ Watershed transformation and watershed based object detection }

\description{
  Watershed transformation and watershed based object detection.
}

\usage{

  watershed(x, tolerance=1, ext=1)

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
}}

\examples{
  
  ## load images
  f <- paste( system.file(package="EBImage"), "images/Gene1_G.tif", sep="/" )
  ii = readImage(f)
  ## normalize images
  ii = normalize(ii, separate=TRUE)
  ## segment
  mask = thresh(ii, 25, 25, 0.02)
  ## refine segmentation with morphology filters
  mk3 = makeBrush(3, shape='diamond')
  mk5 = makeBrush(5, shape='diamond')
  mask = dilate(erode(closing(mask, mk5), mk3), mk5)
  ## index objects with 'watershed'
  io = watershed( distmap(mask), 1.5, 1)
  if (interactive()) display(io)

}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2007
}

\keyword{manip}

