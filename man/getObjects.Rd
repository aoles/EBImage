\name{getObjects}

\alias{getObjects}

\concept{object detection}

\title{ Detection of objects from integer-based segmented images }

\description{
  This function will assign \code{\link{features}} matrix to an image that was
  originally segmented with \code{\link{watershed}} or similar functions (i.e.
  in which objects are encoded as positive integers).
}

\section{ Usage }{
  \preformatted{
    objectImage(x, ref, ...)
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

  An unmodified image of \code{\link{Image-class}} containing a non-empty 
  list of matrixes (one per image in stack) with image
  descriptors. This list can be accessed using \code{\link{features}(result)}
  accessor method of the \code{\link{Image-class}}. Every matrix in the list
  will contain the following columns (subject ot change, will be extended):
  \code{'x', 'y', 'size', 'per', 'int', 'edge'} with one row per every single
  detected object.
    
}

\seealso{
  \code{
    \link{Image-class}, \link{watershed}, \link{paintObjects}
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

