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

    stackObjects(x, ref, rotate=TRUE, bg.col="black", ext)
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

  \item{\code{ext}}{ Extension of the target bounding box. See details. If not
    provided, \ocde{ext} is calculated from data. }
}}

\section{ Value }{
  An image stack or a list of image stacks if \code{x} was itself a stack
  (contained more than one image).
}

\section{ Details }{
  The bounding box is set to be a square, which centre coinsides with the geometric
  center of the object. The \code{ext} argument can be used to specified its size,
  where edge length will be given by \code{2*ext+1}. If \code{ext} is not specified,
  it is calculated from the data by taking the 95\% quantile of the vector of
  \code{h.s2major} descriptor of \code{\link{hull.features}} taken over all,
  objects in the image. This descriptor specifies the extension of the object
  along its major axis starting from its center.

  The size of the bounding box is fixed for all frames to enable combining
  the resulting frames into a single multiframe image.
}

\seealso{
  \code{
    \link{tile}, \link{combine}, \link{paintObjects}
}}

\author{
  Oleg Sklyar <\email{osklyar@ebi.ac.uk}>, 2006-2007
}

\keyword{file}

