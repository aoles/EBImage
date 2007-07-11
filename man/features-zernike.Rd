\name{zernike.moments}

\alias{zernike.moments}

\concept{feature extraction}

\title{ Extraction of Zernike moments from images of indexed objects }

\description{
  
}

\section{ Usage }{
  \preformatted{

  ## S4 method for x='IndexedImage', ref='Image'

  zernike.moments(x, ref, R = 30, N = 5, apply.Gaussian=TRUE)
}}

\section{ Arguments }{ \describe{

  \item{\code{x}}{An object of \code{\link{IndexedImage-class}}.}
  \item{\code{ref}}{An object of \code{\link{Image-class}} in the Grayscale mode.}
  \item{\code{R}}{Defines the radius of the circle around an object centre from which the features are calculated.  
    Defaults to 30. Defines the standard deviation value for the Gausian applied to the objects. See details.}
  \item{\code{N}}{Integer value defining the degree of the Zernike polynomial, which in turn defines the 
    number of features calculated.  Defaults to 5}
  \item{\code{apply.Gaussian}}{A logical value that specifies if a local Gaussian 
    modification should be applied to every object. Defaults to TRUE.}
}}

\section{ Value }{
  For a single frame, a matrix of descriptors with objects in rows and features
  in columns. For image stacks, a list of such matrices.

}

\section{ Details }{
  If \code{apply.Gaussian = TRUE} then prior to calculating the Zernike features, a Gaussian is applied to the image centred at the co-ordinates of an objects centre of mass, with the standard deviation defined by \code{0.8 * R}.  The centres of mass are found by applying \code{\link{moments}}.
  For a given value of \code{N} Zernike features are calculated for all 0 < n <= N and then for all 0 <= l <= n, provided n-l is even.  Columns in the results matrix are firstly ordered by increasing value of n and secondly by increasing value of l.
}

\section{references}{
  F. Zernike.  Beugungstheorie des Schneidenverfahrens und seiner verbesserten Form, der Phasenkontrastmethode (Diffraction theory of the cut procedure and its improved form, the phase contrast method).
Physica, 1:pp. 689-704, 1934.
}

\seealso{\code{
  \link{IndexedImage-class}, \link{watershed}, \link{getFeatures}, 
  \link{edge.features}, \link{moments}
}}



\author{
  Oleg Sklyar <\email{osklyar@ebi.ac.uk}>, Mike Smith <\email{msmith@ebi.ac.uk}>, 2007
}

\keyword{file}

