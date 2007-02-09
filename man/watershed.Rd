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
    watershed(x, ext=1, alg="exclude", ..., verbose=FALSE)
  }
}

\section{ Arguments }{ \describe{

  \item{\code{x}}{An object of \code{\link{Image-class}} in the
    \code{\link{Grayscale}} mode. }

  \item{\code{ext}}{ Extension of neighboring pixels. Recommended values 1 or
    2. If \code{alg} is supplied a value \code{'exclude'} this will also
    determine the width of border exclusion. }

  \item{\code{alg}}{ Algorithm to assign pixels that have more than one
    neighboring object. Possible values are: \code{'exclude'} to exclude such
    pixels from all objects; \code{'steepest'} to assign to the object
    corresponding to the pixel with maximium intensity difference from the
    one in question; \code{'smooth'} -- to assign to the least steep. }

  \item{\code{verbose}}{ The watershed algorithm can be very lengthy on
    noisy images. This argument spcifies if a progress bar should be
    displayed during the detection. }

  \item{\code{...}}{ Reserved for future use. }
}}

\section{ Value }{

  An object of \code{\link{Image-class}} in the \code{\link{Grayscale}} with
  separate objects indexed be positive integers starting from 1. If \code{alg}
  was set to \code{'exclude'} pixels that are in contact with more than one
  object are assigned the value of \code{0.5}. To preview the results visually,
  use \code{\link{display}( \link{normalize}(result) )}.

}

\section{ Details }{

  The algorithm identifies and separates objects that stand out of the
  background (zero), in other words to use the water fill the source image
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

    \dontrun{and now we combine oversegmented}
    w <- combineObjects( w )
    display( normalize(w) )
  }
}

\author{
    Oleg Sklyar: \email{osklyar@ebi.ac.uk}
}

\keyword{file}

