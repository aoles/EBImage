\name{matchObjects}

\alias{matchObjects}
\alias{matchObjects,IndexedImage,IndexedImage-method}

\concept{object detection}

\title{ Matching objects in two indexed images }

\description{
  For objects detected in one image, this function finds indexes of matching
  obnects in the other (i.e. indexes of objects at the locations of the centres
  of objects in the original image.
}

\usage{

  \S4method{matchObjects}{IndexedImage,IndexedImage}(x, ref, ...)

}

\arguments{

  \item{x}{An object of \code{\linkS4class{IndexedImage}}. }

  \item{ref}{A reference image \code{\linkS4class{IndexedImage}} with 
    objects to matched to. }

  \item{...}{ Reserved. }
}

\value{

  If number of frames in \code{x} and \code{ref} is 1, then the result is an
  \code{\link{integer}} vector of matching indexes, similar to \code{\link{match}}.
  
  Otherwise, a list of such vectors, 1 per frame.
}

\seealso{ \code{
    \linkS4class{IndexedImage}, \link{watershed}, \link{getFeatures}
}}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2007
}

\keyword{manip}

