\name{propagate}

\alias{propagate}

\concept{object detection}

\title{ Voronoi-based segmentation on image manifolds }

\description{
  R implementation of the Voronoi-based image segmentation on image
  manifolds [2].
}

\usage{

  propagate(x, seeds, mask=NULL, lambda=0.1, 
     ext=1, seed.centers=FALSE)
}

\arguments{

  \item{x}{An object of \code{\linkS4class{Image}} to be segmented,
    in the \code{\link{Grayscale}} mode. }

  \item{seeds}{An object of \code{\linkS4class{Image}} of the same
    size as x in all three dimensions. This image provides seed points for object detection. }

  \item{mask}{An object of \code{\linkS4class{Image}} of the same
    size as \code{x} in all three dimensions; in the \code{\link{Grayscale}} mode.
    All zero regions will be excluded from object detection. }

  \item{lambda}{ A numeric value. The regularisation parameter for the
    distance calculations, determines the trade-off between the Euclidian distance in
    the image plane and the contribution of the gradient of the values
    in \code{x}. See details. }

  \item{ext}{ Extension of the neighborhood to estimate image
    gradient, in pixels in every direction from the central point, i.e.
    ext=1 means a 3x3 neighborhood. }

  \item{seed.centers}{ If \code{TRUE}, only centers of the seed points are left
    in the seeds image supplied to the propagate algorithm. }
}

\value{

  An image of \code{\linkS4class{Image}}, with the same object indexing as
  \code{seeds}. No new objects are created, only those specified by
  seeds are propagated. Use \code{\link{getFeatures}} to assign the
  feature matrix.

}

\details{

  The method operates by computing a discretized approximation of the
  Voronoi regions for given seed points on a manifold with a metric
  controlled by local image features.

  The metric is a Riemannian metric defined in terms of the image I and
  a regularization parameter lambda. With this metric the
  distance between pixels used to let the given seeds grow outwards
  (propagate) is

  \preformatted{d^2 = (grad(I)^2 + lambda * (dx^2 + dy^2)) / (lambda + 1)}

  The above formulation was proposed by Carpenter et al, however in the 
  calculation we use a modified distance measture, in which sharp gradients
  are downregulated and large distances additionally penalized. Effectively,
  we use the following formula:
  
  \preformatted{d = sqrt(grad(I)) + 1e-3*lambda * (dx^2+dy^2)^2}
  
  The denominator is left out for speed reasons, so is the square root of the 
  distance.

  The gradient is calculated on a neighborhood of pixels (the width of which
  is controlled by the argument \code{ext}) to avoid relying on single
  (noisy) pixels. Lambda controls the weight of the Euclidian distance
  term. In case of large
  lambda, d turns into Euclidian distance in the \code{(x,y)}-plane.
  For small lambda, the distance will be dominated by the intensity gradient.

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
  if (interactive()) display(io)

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
  xo = propagate(xi, io, mask, 1e-2, 2)
  if (interactive()) display(xo)
  
}

\author{
  Original CellProfiler code: Anne Carpenter <carpenter@wi.mit.edu>,
  Thouis Jones <thouis@csail.mit.edu>, In Han Kang <inthek@mit.edu>.

  Port for this package: Oleg Sklyar <osklyar@ebi.ac.uk> and Wolfgang Huber <huber@ebi.ac.uk>.
}

\section{ License }{

  The underlying \code{C++} code is based on code from CellProfiler
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

\keyword{manip}

