\name{getObjects}

\alias{getObjects}
\alias{features}
\alias{features<-}
\alias{stackObjects}

\concept{object detection}

\title{ Object properties in indexed images }

\description{
  Functions to retreive properties of objects corresponding to differently indexed
  regions in indexed images such as those obtained in calls to 
  \code{\link{watershed}} or \code{\link{propagate}}.
  
}

\section{ Usage }{
\preformatted{
## S4 method for signatures x='IndexedImage', ref='Image' and ref='missing'

getObjects(x, ref)

## S4 method for signature x='IndexedImage', ref='Image' 

stackObjects(x, ref)

## S4 methods for signatures x='Image', value='list'

features(x)
features(x) <- value

}
}

\section{ Arguments }{ \describe{

  \item{\code{x}}{ An object of \code{\link{IndexedImage-class}}. }

  \item{\code{ref}}{ A reference image containing the image of the objects.
    This can be missing or gray scale for \code{getObjects}. It must be
    present and can be in any color mode for \code{stackObjects}. }
}}

\section{ Value }{

  The following is likely to change in the forthcoming version of the package.
  New structures for objects and their properties are to be introduced!

  \code{getObjects} returns a list of matrixes (one per image in stack) with
  image descriptors. This list can be stored with the image using the set
  function \code{features<-} and retrieved from the object using the corresponding
  get function \code{features}. Every matrix in the list will contain
  named columns of features starting with object position, size, perimeter etc.
  with one row per object detected.

  If \code{x} and \code{ref} contained only 1 frame, then an object of 
  \code{\link{Image-class}} with one object per frame, centered, is returned.
  Otherwise, if original images contained multiple stacks, a list of images like
  the former is returned. The result can be directly supplied to
  \code{\link{tile}} to generate tiled previews.

}

\section{See Also}{ \code{
  \link{Image-class}, \link{IndexedImage-class}, \link{watershed}, 
  \link{propagate}, \link{paintObjects}
}}

\examples{
  \dontrun{See ?watershed for an example}
}

\author{
  Oleg Sklyar: \email{osklyar@ebi.ac.uk}
}

\keyword{file}

