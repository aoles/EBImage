\name{matchObjects}

\alias{matchObjects}

\concept{object detection}

\title{ Matches detected objects in two images }

\description{
  For objects detected in one image, this function finds indexes of matching
  obnects in the other (i.e. indexes of objects at the locations of the centres
  of objects in the original image.
}

\section{ Usage }{
  \preformatted{
    matchObjects(x, ref, ...)
  }
}

\section{ Arguments }{ \describe{

  \item{\code{x}}{An object of \code{\link{Image-class}} in the 
    \code{\link{Grayscale}} mode with integer-based object encoding, as
    returned by \code{\link{watershed}}. }

  \item{\code{ref}}{A reference grayscale image with integer-based object 
    encoding to match objects to. }

  \item{\code{...}}{ Reserved for future use. }
}}

\section{ Value }{

  A list of integer vectors, one per 2D image, with indexes of objects in the
  reference image matching objects in the original image.

}

\seealso{
  \code{
    \link{Image-class}, \link{watershed}, \link{getObjects}, 
    \link{combineObjects}
  }
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Copyright (c) 2005-2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\keyword{file}

