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

    getFeatures(x, ref, N=12, R=30, apply.Gaussian=TRUE, nc=256)
    features(x)
}}

\section{ Arguments }{ \describe{

  \item{\code{x}}{ An object of \code{\link{IndexedImage-class}} (like those
    obtained from \code{\link{propagate}} or \code{\link{watershed}}. }

  \item{\code{ref}}{ A reference image containing images of the objects in
    Grayscale mode (intensity values)! }

  \item{\code{N}}{ Passed to \code{\link{zernike.moments}}. Integer value 
    defining the degree of the Zernike polynomials, which in turn defines the 
    number of features calculated. Defaults to 12.}

  \item{\code{R}}{ Passed to \code{\link{zernike.moments}}. Defines the radius 
    of the circle around an object centre from which the features are calculated. 
    It also defines the standard deviation for the 2D Gausian applied at the 
    centre of an object. See details. Defaults to 30. }
    
  \item{\code{apply.Gaussian}}{Passed to \code{\link{zernike.moments}}. A logical 
    value that specifies if a local 2D Gaussian modification should be applied 
    to every object at its centre. Defaults to \code{TRUE}.}

  \item{\code{nc}}{ Passed to \code{\link{haralick.features}}. A numeric value. 
    Specifies the number of gray levels to separate \code{ref} into when 
    calculating the co-occurrence matrix. Here defaults to 256.}

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

