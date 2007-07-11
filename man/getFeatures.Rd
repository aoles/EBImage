\name{getFeatures}

\alias{getFeatures}
\alias{features}

\concept{object detection}

\title{ Feature extraction for objects in indexed images }

\description{
  Functions to extract numerical descriptors for objects in indexed images.
  The functions here call a set of individual feature extraction routines for
  every feature set like hull, texture (Haralick), edge, moment features etc.
}

\section{ Usage }{
  \preformatted{
    ## S4 methods for signature x='IndexedImage'

    getFeatures(x, ref, ...)
    features(x)
}}

\section{ Arguments }{ \describe{

  \item{\code{x}}{ An object of \code{\link{IndexedImage-class}} (like those
    obtained from \code{\link{propagate}} or \code{\link{watershed}}. }

  \item{\code{ref}}{ A reference image containing images of the objects in
    Grayscale mode (intensity values)! }

  \item{\code{...}}{ Pass further arguments like \code{N}, \code{R} and \code{apply.Gaussian} 
    to \code{\link{zernike.moments}} or \code{nc} to \code{\link{haralick.features}}! }
}}

\section{ Value }{
  \code{getFeatures} calculates feature matrices and returns an object of 
  \code{\link{IndexedImage-class}} with its \code{features} slot set. The typical
  use would be to re-assign the result to the original image itself, 
  i.e. \code{x <- getFeatures(x, a)}.
  
  \code{features} extracts the content of the \code{features} slot of the 
  \code{IndexedImage}. If it is empty, the function calls \code{getFeatures}
  without a reference image and returns the generated feature set. In this
  case the \code{features} slot of the original image is not updated.
}

      
\section{ Details }{
  \code{ref} can be missing in calls to \code{getFeatures}. In this case texture
  and other intensity-dependent features will be omitted.
}
 

\section{See Also}{ \code{
  \link{IndexedImage-class}, \link{propagate}, \link{paintObjects}, \link{stackObjects},
  \link{hull.features}, \link{haralick.features}, \link{moments}, \link{edge.features},
  \link{zernike.moments}
}}

\author{
  Oleg Sklyar <\email{osklyar@ebi.ac.uk}>, 2007
}

\keyword{file}

