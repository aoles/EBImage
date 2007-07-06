\name{stackObjects}

\alias{stackObjects}

\concept{image manipulation}

\title{ Generate a stack of images for detected objects, one object per image }

\description{
  From an indexed image the functions generates an image stack with one image
  per object placing each object in the middle of the image. Objects can be
  automatically rotated to align them along the horizontal axis.
}

\section{ Usage }{
  \preformatted{
    ## S4 method for signature x='IndexedImage', ref='Image' 

    stackObjects(x, ref, rotate=TRUE, bg.col="black")
}}

\section{ Arguments }{ \describe{
  \item{\code{x}}{An object of \code{\link{IndexedImage-class}}.
    Images must be Grayscale and carry object indexing information, like those
    returned by \code{\link{watershed}} or \code{\link{propagate}}. }

  \item{\code{ref}}{ A reference image containing images of the objects to be 
    stacked using \code{x} both as index and the mask. Can be in any
    color mode. }

  \item{\code{rotate}}{ Specifies if the objects should be aligned rotationally. }

  \item{\code{bg.col}}{ Color for pixels outside the mask defined by \code{x}. }
}}

\section{ Value }{
  An image stack or a list of image stacks if \code{x} was itself a stack 
  (contained more than one image).
}

\seealso{
  \code{
    \link{tile}, \link{combine}, \link{paintObjects}
}}

\author{
  Oleg Sklyar <\email{osklyar@ebi.ac.uk}>, 2006-2007
}

\keyword{file}

