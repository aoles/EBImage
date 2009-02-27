\name{rmObjects}

\alias{rmObjects}
\alias{reenumerate}

\concept{object detection}
\concept{delete objects}

\title{ Object removal }

\description{
  The \code{rmObjects} functions deletes objects indexed by a list of
  integer vectors (indexes) from an image with indexed objects. \code{reenumerate}
  re-enumerates all objects in an \linkS4class{Image} from 0 - background
  to the actual number of objects.
}

\usage{
  rmObjects(x, index)

  reenumerate(x)
}

\arguments{

  \item{x}{An object of \code{\linkS4class{Image}}. }

  \item{index}{ A numeric (integer) vector of indexes of objects to
    remove in the frame if \code{x} contains one frame only. For multiple
    frames, a list of such vectors.}
}

\value{

  An image of \code{\linkS4class{Image}}.

}

\seealso{ \code{
  \linkS4class{Image}, \link{watershed}, \link{getFeatures}, \link{paintObjects}
}}

\examples{
  ## see example(drawtext)
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2006-2007
}

\keyword{manip}

