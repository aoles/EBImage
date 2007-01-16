\name{getObjects}

\alias{getObjects}
\alias{features}
\alias{features<-}

\concept{object detection}

\title{ Detection of objects from integer-based segmented images }

\description{
  This function will assign \code{\link{features}} matrix to an image that was
  originally segmented with \code{\link{watershed}} or similar functions (i.e.
  in which objects are encoded as positive integers).
}

\section{ Usage }{
  \preformatted{
    getObjects(x, ref, ...)
    features(x, ...)
    features(x, ...) <- value
  }
}

\section{ Arguments }{ \describe{

  \item{\code{x}}{An object of \code{\link{Image-class}} in the 
    \code{\link{Grayscale}} mode with integer-absed object encoding, as
    returned by \code{\link{watershed}}. }

  \item{\code{ref}}{A reference grayscale image to calculate object intensity. 
    Can be \code{NULL}. }

  \item{\code{...}}{ Reserved for future use. }
}}

\section{ Value }{

  \code{getObjects} returns a list of matrixes (one per image in stack) with 
  image descriptors. This list can be stored with the image using the set
  function \code{features<-} and retrieved from the object using the corresponding
  get function \code{features}. Every matrix in the list will contain 
  named columns of features starting with object position, size, perimeter etc. 
  with one row per object detected.
}

\seealso{
  \code{
    \link{Image-class}, \link{watershed}, \link{paintObjects}, 
    \link{combineObjects}
  }
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\examples{
    \dontrun{See ?watershed for example}
  }
}

\author{
    Copyright (c) 2005-2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\keyword{file}

