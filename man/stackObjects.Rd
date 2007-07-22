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
    ## S4 method for signature x='IndexedImage', ref='Image' and
    ## index='character'/'list'/'numeric'/'missing'

    stackObjects(x, ref, index, combine, rotate, bg.col, ext)
}}

\section{ Arguments }{ \describe{
  \item{\code{x}}{An object of \code{\link{IndexedImage-class}}.
    Images must be Grayscale and carry object indexing information, like those
    returned by \code{\link{watershed}} or \code{\link{propagate}}. }

  \item{\code{ref}}{ A reference image containing images of the objects to be
    stacked using \code{x} both as index and the mask. Can be in any
    color mode. }

  \item{\code{index}}{ Indexes of objects to stack. See Details for supported
    types. }

  \item{\code{combine}}{ Called on a stack if images, specifies if the resulting
    list of image stacks with individual objects should be combined into a single
    image stack. Defaults to \code{FALSE}. }

  \item{\code{rotate}}{ Specifies if the objects should be aligned rotationally.
    Defaults to \code{TRUE}. }

  \item{\code{bg.col}}{ Color for pixels outside the mask defined by \code{x}.
    Defaults to \code{TRUE}. }

  \item{\code{ext}}{ Extension of the target bounding box. See details. If not
    given, \code{ext} is calculated from data. }
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

  \code{index} must be coersable to numeric to specify object indexes that
  are taken into the resulting stack. It can be specified as \code{numeric}
  directly only for images with 1 frame. For images with multiple frames it can
  be either a \code{list} or a \code{character}. If specified as a \code{list},
  it can be a named list of numeric indexes where names are converted to
  \code{character} frame indexes in an arbitrary order, or it can be an unnamed
  list of the same length as the number of frames. If specifies as \code{character},
  each element must contain two numbers separated by a dot where the first number
  is the index of the frame and the second one is the index of the object within
  the frame, e.g. \code{"2.035"} will specify frame 2 and object 35.
}

\seealso{
  \code{
    \link{tile}, \link{combine}, \link{paintObjects}
}}

\author{
  Oleg Sklyar <\email{osklyar@ebi.ac.uk}>, 2006-2007
}

\keyword{file}

