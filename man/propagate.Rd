\name{propagate}

\alias{propagate}

\concept{object detection}

\title{Voronoi-based segmentation on image manifolds}

\description{
Find boundaries between adjacent regions in an image, where seeds
have been already identified in the individual regions to be
segmented. The method finds the Voronoi region of each seed on
a manifold with a metric controlled by local image properties.
The method is motivated by the problem of finding the borders of
cells in microscopy images, given a labelling of the nuclei
in the images.

Algorithm and implementation are from Jones et al. [1].
}

\usage{
  propagate(x, seeds, mask=NULL, lambda=1e-4, ext=1, seed.centers)
}

\arguments{
   \item{x}{An \code{Image} object or an array, containing the image to segment.}

   \item{seeds}{An \code{Image} object or an array, containing the seeding objects of
   the already identified regions.}	

   \item{mask}{An optional \code{Image} object or an array, containing
   the binary image mask of the regions that can be segmented. If missing, 
   the whole image is segmented.}

   \item{lambda}{A numeric value. The regularisation parameter used in the
   metric, determining the trade-off between the Euclidian distance in the
   image plane and the contribution of the gradient of \code{x}. See details.}

  \item{ext}{Radius in pixels of the neighborhood to estimate image
    gradient, e.g. ext=1 means a 3x3 neighborhood.}

  \item{seed.centers}{Deprecated.}
}

\value{
  An \code{Image} object or an array, containing the labelled objects.
}

\details{
  The method operates by computing a discretized approximation of the
  Voronoi regions for given seed points on a Riemann manifold with a
  metric controlled by local image features.

  Under this metric, the infinitesimal distance d between points
  v and v+dv is defined by: 
  \preformatted{d^2 = ( (t(dv)*g)^2 + lambda*t(dv)*dv )/(lambda + 1) },
  where g is the gradient of image \code{x} at point v.

  \code{lambda} controls the weight of the Euclidian distance term. 
  When \code{lambda} tends to infinity, d tends to the Euclidian
  distance. When \code{lambda} tends to 0, d tends to the intensity
  gradient of the image.

  To avoid to rely too much on single noisy pixels, the gradient is
  calculated on a neighborhood of \code{ext} pixels. 
}

\seealso{ \code{\link{bwlabel}}, \code{\link{watershed}}
}

\examples{
  ## load images
  nuc = readImage(system.file('images', 'nuclei.tif', package='EBImage'))
  cel = readImage(system.file('images', 'cells.tif', package='EBImage'))
  img = rgbImage(green=1.5*cel, blue=nuc)
  if (interactive()) display(img, title='Cells')

  ## segment nuclei
  nmask = thresh(nuc, 10, 10, 0.05)
  nmask = opening(nmask, makeBrush(5, shape='disc'))
  nmask = fillHull(nmask)
  nmask = bwlabel(nmask)
  if (interactive()) display(normalize(nmask), title='Cell nuclei mask')

  ## segment cells, using propagate and nuclei as 'seeds'
  ctmask = opening(cel>0.1, makeBrush(5, shape='disc'))
  cmask = propagate(cel, nmask, ctmask)
  if (interactive()) display(normalize(cmask), title='Cell mask')

  ## segmentation results
  res = paintObjects(cmask, img, col='#ff00ff')
  res = paintObjects(nmask, res, col='#ffff00')
  if (interactive()) display(res, title='Segmented cells')

}

\author{
  Original CellProfiler code: Anne Carpenter <carpenter@wi.mit.edu>,
  Thouis Jones <thouis@csail.mit.edu>, In Han Kang <inthek@mit.edu>.

  Port for this package: Oleg Sklyar <osklyar@ebi.ac.uk> and Wolfgang Huber
  <huber@ebi.ac.uk>.
}

\section{License}{
  The implementation is based on CellProfiler C++ source code [2, 3].
  An LGPL license was granted by Thouis Jones to use this part of CellProfiler's code for the \code{propagate} function.

}

\references{
    \code{[1]} T. Jones, A. Carpenter and P. Golland,
    "Voronoi-Based Segmentation of Cells on Image Manifolds",
    CVBIA05 (535-543), 2005

    \code{[2]} A. Carpenter, T.R. Jones, M.R. Lamprecht, C. Clarke, I.H. Kang,
    O. Friman, D. Guertin, J.H. Chang, R.A. Lindquist, J. Moffat,
    P. Golland and D.M. Sabatini, "CellProfiler: image analysis software
    for identifying and quantifying cell phenotypes", Genome Biology 2006, 7:R100

    \code{[3]} CellProfiler: http://www.cellprofiler.org
}


