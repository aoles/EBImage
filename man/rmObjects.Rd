\name{rmObjects}

\alias{rmObjects}
\alias{rmObjects,IndexedImage,numeric-method}
\alias{rmObjects,IndexedImage,list-method}

\concept{object detection}
\concept{delete objects}

\title{ Object removal }

\description{
  The \code{rmObjects} functions deletes objects indexed by a list of
  integer vectors (indexes) from an image with indexed objects.
}

\usage{

  ## S4 methods for signatures 'x=IndexedImage,index=numeric'
  ## and 'x=IndexedImage,index=list'

  rmObjects(x, index, ...)
  
}

\arguments{

  \item{x}{An object of \code{\linkS4class{IndexedImage}}. }

  \item{index}{ A numeric (integer) vector of indexes of objects to
    remove in the frame if \code{x} contains one frame only. For multiple
    frames, a list of such vectors.}

  \item{...}{ Reserved. }
}

\value{

  An image of \code{\linkS4class{IndexedImage}}.

}

\seealso{ \code{
  \linkS4class{IndexedImage}, \link{watershed}, \link{getFeatures}, \link{paintObjects}
}}

\examples{
  ## see example(drawtext)
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2006-2007
}

\keyword{manip}

