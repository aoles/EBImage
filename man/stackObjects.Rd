\name{stackObjects}

\alias{stackObjects}

\concept{image manipulation}

\title{Places detected objects into an image stack}

\description{
  Places detected objects into an image stack.
}

\usage{
  stackObjects(x, ref, combine=TRUE, bg.col='black', ext)
}

\arguments{
  \item{x}{An \code{Image} object or an array containing object masks.
    Object masks are sets of pixels with the same unique integer value.}
    
  \item{ref}{An \code{Image} object or an array, containing the
    intensity values of the objects.}

  \item{combine}{If \code{x} contains multiple images, specifies if the resulting
    list of image stacks with individual objects should be combined
    using \code{combine} into a single image stack.}

  \item{bg.col}{Background pixel color.}

  \item{ext}{A numeric controlling the size of the output simage. 
   If missing, \code{ext} is estimated from data. See details.}
}

\value{
  An \code{Image} object containing the stacked objects contained in
  \code{x}. If \code{x} contains multiple images and if \code{combine}
  is \code{TRUE}, \code{stackObjects} returns a list of \code{Image}
  objects.  
}

\details{
  \code{stackObjects} creates a set of \code{nbobj} images of size
  (\code{2*ext+1}, \code{2*ext+1}), where \code{nbobj} is the number of objects
  in \code{x}, and places each object of \code{x} in this set.
  
  If not specified, \code{ext} is estimated using the 95\% quantile of
  2*sqrt(\code{g.l1}), where \code{g.l1} is the semi-major axis
  descriptor extracted from \code{computeFeatures.moment}, taken over
  all the objects of the image \code{x}.
}

\seealso{ \code{\link{combine}}, \code{\link{tile}}, \code{\link{computeFeatures.moment}} 
}

\examples{
  ## simple example
  x = readImage(system.file('images', 'shapes.png', package='EBImage'))
  x = x[110:512,1:130]
  y = bwlabel(x)
  if (interactive()) display(normalize(y), title='Objects')
  z = stackObjects(y, normalize(y))
  if (interactive()) display(z, title='Stacked objects')

  ## load images
  nuc = readImage(system.file('images', 'nuclei.tif', package='EBImage'))
  cel = readImage(system.file('images', 'cells.tif', package='EBImage'))
  img = rgbImage(green=cel, blue=nuc)
  if (interactive()) display(img, title='Cells')

  ## segment nuclei
  nmask = thresh(nuc, 10, 10, 0.05)
  nmask = opening(nmask, makeBrush(5, shape='disc'))
  nmask = fillHull(bwlabel(nmask))

  ## segment cells, using propagate and nuclei as 'seeds'
  ctmask = opening(cel>0.1, makeBrush(5, shape='disc'))
  cmask = propagate(cel, nmask, ctmask)

  ## using paintObjects to highlight objects
  res = paintObjects(cmask, img, col='#ff00ff')
  res = paintObjects(nmask, res, col='#ffff00')
  if (interactive()) display(res, title='Segmented cells')

  ## stacked cells
  st = stackObjects(cmask, img)
  if (interactive()) display(st, title='Stacked objects')
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2006-2007
}
