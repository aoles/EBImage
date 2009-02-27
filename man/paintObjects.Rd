\name{paintObjects}

\alias{paintObjects}

\concept{object detection}

\title{ Marking detected objects in reference images }

\description{
  This function allows to mark objects detected with \code{\link{getFeatures}} or
  \code{\link{watershed}} in colour for preview.
}

\usage{

  paintObjects(x, tgt, opac=c(0.4, 0.05, 0.4), col=c("#FFC72C","#5BABF6","#FF372C"))

}

\arguments{

  \item{x}{An object of \code{\linkS4class{Image}} in the
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
}

\value{

  A copy of \code{tgt} in the same colour mode with objects marked on top of the
  image. \code{\link{features}} of \code{x} are not transfered -- this result is
  for visualization only.
}

\seealso{ \code{
  \linkS4class{Image}, \link{watershed}, \link{getFeatures}
}}

\examples{

  ## load images of nuclei (seed points later)
  f <- paste( system.file(package="EBImage"), "images/Gene1_G.tif", sep="/" )
  ii = readImage(f)
  ## normalize images
  ii = normalize(ii, separate=TRUE)
  ## segment
  mask = thresh(ii, 25, 25, 0.02)
  ## refine segmentation with morphology filters
  mk3 = makeBrush(3, shape='diamond')
  mk5 = makeBrush(5, shape='diamond')
  mask = dilate(erode(closing(mask, mk5), mk3), mk5)
  ## index objects with 'watershed'
  io = watershed( distmap(mask), 1.5, 1)

  ## load images of cells (the ones to segment with propagate)
  f <- paste( system.file(package="EBImage"), "images/Gene1_R.tif", sep="/" )
  xi = readImage(f)
  ## normalize images
  xi = normalize(xi, separate=TRUE)
  ## segment
  mask = thresh(xi, 40, 40, 0.0)
  ## refine segmentation with morphology filters
  mk7 = makeBrush(7, shape='diamond')
  mask = dilate(erode(closing(mask, mk7), mk5), mk7)
  ## index objects of xi with 'propagate' using ii as seeds
  xo = propagate(xi, io, mask, 1e-5, 1.5)
  
  ## create an RGB preview of a combination of ii and xi
  rgb = rgbImage(red=ii,green=xi)
  ## paint cells on the preview
  rgb = paintObjects(xo, rgb)
  ## paint nuclei on the preview
  rgb = paintObjects(io, rgb)
  if (interactive()) display(rgb)
  
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2006-2007
}

\keyword{manip}

