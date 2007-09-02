\name{paintObjects}

\alias{paintObjects}
\alias{paintObjects,IndexedImage,Image-method}

\concept{object detection}

\title{ Marking detected objects in reference images }

\description{
  This function allows to mark objects detected with \code{\link{getFeatures}} or
  \code{\link{watershed}} in colour for preview.
}

\usage{

  \S4method{paintObjects}{IndexedImage,Image}(x, tgt, opac=c(0.4, 0.05, 0.4), col=c("#FFC72C","#5BABF6","#FF372C"), ...)

}

\arguments{

  \item{x}{An object of \code{\linkS4class{IndexedImage}} in the
    \code{\link{Grayscale}} mode with integer-absed object encoding, as
    returned by \code{\link{watershed}}. }

  \item{tgt}{A reference grayscale image to calculate object intensity.
    Should be \code{\link{TrueColor}} to produce coloured output. }

  \item{opac}{A numeric vector of opacity values for foreground (object
    boundary), object background and edges of object contacts. At least 3
    values in the above sequence must be supplied. Opacity range is \code{[0,1]}
    with 0 being fully transparent. }

  \item{col}{A character vector of full colours (before opacity applied),
    colour names supported, to draw object boundaries, object background and
    edges of object contacts. At least 3 values must be supplied. Default
    color scheme is yellow for edges, blue for background and red for object
    contacts and object on image edges. }

  \item{...}{ Reserved. }
  
}

\value{

  A copy of \code{tgt} in the same colour mode with objects marked on top of the
  image. \code{\link{features}} of \code{x} are not transfered -- this result is
  for visualization only.
}

\seealso{ \code{
  \linkS4class{IndexedImage}, \linkS4class{Image}, \link{watershed}, \link{getFeatures}
}}

\examples{
  \dontrun{See ?watershed for example}
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2006-2007
}

\keyword{manip}

