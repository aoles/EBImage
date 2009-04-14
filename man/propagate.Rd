\name{propagate}

\alias{propagate}

\concept{object detection}

\title{Voronoi-based segmentation on image manifolds}

\description{
  R implementation of the Voronoi-based image segmentation on image
  manifolds from Carpenter et al. [2].
}

\usage{
  propagate(x, seeds, mask=NULL, lambda=0.1, 
     ext=1, seed.centers=FALSE)
}

\arguments{

   \item{x}{An \code{Image} object in \code{Grayscale} color mode
    or an array, containing the image to segment.}

   \item{seeds}{An \code{Image} object in \code{Grayscale} color mode
    or an array, containing the seeding objects from where to start
    segmentation.}

  \item{mask}{An optional \code{Image} object or an array, 
    containing the seeding objects from where to start
    segmentation. \code{mask} is considered as a binary mask,
    whose pixels of value 0 will be excluded from the final
    segmentation objects.}

  \item{lambda}{A numeric value. The regularisation parameter for the
    distance calculations, determining the trade-off between the Euclidian distance in
    the image plane and the contribution of the gradient of the values
    in \code{x}. See details.}

  \item{ext}{Radius in pixels of the neighborhood to estimate image
    gradient, e.g. ext=1 means a 3x3 neighborhood.}

  \item{seed.centers}{ If \code{TRUE}, only centers of the seeding objects
    are considered as seeds for the propagate algorithm.}
}

\value{
  An \code{Image} object in \code{Grayscale} color mode or
  an array, containing the labelled objects.
}

\details{
  The method operates by computing a discretized approximation of the
  Voronoi regions for given seed points on a manifold with a metric
  controlled by local image features.

  The metric is a Riemannian metric defined in terms of the image I and
  a regularization parameter lambda. With this metric the
  distance between pixels used to let the given seeds grow outwards
  (propagate) is:

  \preformatted{d^2 = (grad(I)^2 + lambda * (dx^2 + dy^2)) / (lambda + 1)}

  The above formulation was proposed by Carpenter et al, however in the 
  calculation we use a modified distance measture, in which sharp gradients
  are downregulated and large distances additionally penalized. Effectively,
  we use the following formula:
  
  \preformatted{d = sqrt(grad(I)) + 1e-3*lambda * (dx^2+dy^2)^2}

  The gradient is calculated on a neighborhood of \code{ext} pixels. \code{lambda}
  controls the weight of the Euclidian distance term. In case of large
  values, d turns into the Euclidian distance. For small values of 
  \code{lambda}, the distance will be dominated by the intensity gradient.
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

  Port for this package: Oleg Sklyar <osklyar@ebi.ac.uk> and Wolfgang Huber <huber@ebi.ac.uk>.
}

\section{ License }{

  The underlying C++ code is based on code from CellProfiler
  [1,3]. An LGPL license was granted by Thouis Jones to use this part
  of CellProfiler's code for the \code{propagate} function.

}

\references{
    \code{[1]} \emph{A. Carpenter, T.R. Jones, M.R. Lamprecht, C. Clarke, I.H. Kang,
    O. Friman, D. Guertin, J.H. Chang, R.A. Lindquist, J. Moffat,
    P. Golland and D.M. Sabatini}, "CellProfiler: image analysis software
    for identifying and quantifying cell phenotypes", Genome Biology 2006, 7:R100

    \code{[2]} \emph{T. Jones, A. Carpenter and P. Golland},
    "Voronoi-Based Segmentation of Cells on Image Manifolds",
    CVBIA05 (535-543), 2005

    \code{[3]} CellProfiler: http://www.cellprofiler.org
}


