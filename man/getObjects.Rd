\name{getObjects}

\alias{getObjects}
\alias{features}
\alias{features<-}
\alias{stackObjects}

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
    stackObjects(x, ref, ...)
    features(x, ...)
    features(x, ...) <- value
  }
}

\section{ Arguments }{ \describe{

  \item{\code{x}}{An object of \code{\link{Image-class}} in the
    \code{\link{Grayscale}} mode with integer-absed object encoding, as
    returned by \code{\link{watershed}}. }

  \item{\code{ref}}{A reference image. This can be \code{NULL} or Grayscale
    for \code{getObjects} to determined object intensities. This must be
    present and can be in any color mode for \code{stackObjects}. }

  \item{\code{...}}{ Reserved for future use. }
}}

\section{ Value }{

  \code{getObjects} returns a list of matrixes (one per image in stack) with
  image descriptors. This list can be stored with the image using the set
  function \code{features<-} and retrieved from the object using the corresponding
  get function \code{features}. Every matrix in the list will contain
  named columns of features starting with object position, size, perimeter etc.
  with one row per object detected.

  \code{stackObjects} returns a list of Image stacks with one element of a list
  for each frame in the original Image (-stack). Each Image stack will contain
  images of single objects positioned in the middle of the image in the same
  color mode as the reference images. The result can be directly supplied to
  \code{\link{tile}} to generate tiled previews.

}

\section{See Also}{
  \code{
    \link{Image-class}, \link{watershed}, \link{paintObjects}
  }
}

\section{References}{
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

