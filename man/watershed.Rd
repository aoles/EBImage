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

\section{ Usage }{
  \preformatted{
    watershed(x, tolerance=1, ext=1)
  }
}

\section{ Arguments }{ \describe{

  \item{\code{x}}{An object of \code{\link{Image-class}} in the
    \code{\link{Grayscale}} mode. }

  \item{\code{tolerance}}{ The minimum height of the object in the units of image
    intensity between its highest point (seed) and the point where it
    contacts another object (checked for every contact pixel). If the
    height is smaller than the tolerance, the object will be combined with
    one of its neighbors, which is the highest. It is assumed that the
    function is run on a distance map, therefore the default value
    is 1. If running the function on an original grayscale image with
    intensity range \code{[0,1]} one should modify this value, down to 0.1
    or other which is image specific. }

  \item{\code{ext}}{ Extension of the neighborhood for the detection
    of neighboring objects. Higher value smoothes out small objects. }

}}

\section{ Value }{

  An object of \code{\link{Image-class}} in the \code{\link{Grayscale}} with
  separate objects indexed be positive integers starting from 1. To preview
  the results visually, use \code{\link{display}( \link{normalize}(result) )}
  or use it in combination with \code{\link{paintObjects}}.

}

\section{ Details }{

  The algorithm identifies and separates objects that stand out of the
  background (zero), in other words to use the water fill, the source image
  is flipped upside down and the resulting valleys (values with higher
  intensities) are filled in first until another object or background is met.
  The deepest valleys (pixels with highest intensity) become indexed first.

}

\seealso{
  \code{
    \link{Image-class}, \link{distmap}, \link{thresh}, \link{getObjects},
    \code{matchObjects}
  }
}

\examples{
  if ( interactive() ) {
    ddir <- paste( system.file(package="EBImage"), "images", sep="/" )
    a <- read.image( paste(ddir, "A04w1.jpg", sep="/") )

    w <- watershed( distmap( thresh(a, 10, 10) ) )
    display( normalize(w) )

    \dontrun{often one needs to delete small objects}
    \dontrun{here is the straightforward way to construct the index}
    index <- lapply( getObjects(w), function(x) which( x[, "size"] < 40) )
    w <- rmObjects(w, index)
    display( normalize(w) )

  }
}

\author{
    Oleg Sklyar: \email{osklyar@ebi.ac.uk}
}

\keyword{file}

