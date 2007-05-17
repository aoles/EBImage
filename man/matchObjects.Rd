\name{matchObjects}

\alias{matchObjects}

\concept{object detection}

\title{ Matching objects in two indexed images }

\description{
  For objects detected in one image, this function finds indexes of matching
  obnects in the other (i.e. indexes of objects at the locations of the centres
  of objects in the original image.
}

\section{ Usage }{
\preformatted{
## S4 method for signature x='IndexedImage', ref='IndexedImage'

matchObjects(x, ref)
}}

\section{ Arguments }{ \describe{

  \item{\code{x}}{An object of \code{\link{IndexedImage-class}}. }

  \item{\code{ref}}{A reference image \code{\link{IndexedImage-class}} with 
    objects to matched to. }

}}

\section{ Value }{

  If number of frames in \code{x} and \code{ref} is 1, then the result is an
  \code{\link{integer}} vector of matching indexes, similar to \code{\link{match}}.
  
  Otherwise, a list of such vectors, 1 per frame.
}

\seealso{ \code{
    \link{IndexedImage-class}, \link{watershed}, \link{getObjects}
}}

\author{
  Oleg Sklyar: \email{osklyar@ebi.ac.uk}
}

\keyword{file}

