\name{propagate}

\alias{propagate}
\alias{propagate,Image,IndexedImage-method}

\concept{object detection}

\title{ Voronoi-based segmentation on image manifolds }

\description{
  R implementation of the Voronoi-based image segmentation on image
  manifolds [2].
}

\usage{

  \S4method{propagate}{Image,IndexedImage}(x, seeds, mask=NULL, lambda=0.005, ext=1, ...)
}

\arguments{

  \item{x}{An object of \code{\linkS4class{Image}} to be segmented,
    in the \code{\link{Grayscale}} mode. }

  \item{seeds}{An object of \code{\linkS4class{IndexedImage}} of the same
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

  \item{...}{ Reserved. }
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

  The gradient is calculated on a neighborhood of pixels (the width of which
  is controlled by the argument \code{ext}) to avoid relying on single
  (noisy) pixels. Lambda controls the weight of the Euclidian distance
  term. In case of large
  lambda, d turns into Euclidian distance in the \code{(x,y)}-plane.
  For small lambda, the distance will be dominated by the intensity gradient.

}

\seealso{ \code{
    \linkS4class{IndexedImage}, \linkS4class{Image}, \link{watershed}, \link{getFeatures}
}}

\examples{
  
  ## FIXME: change to new images

  ddir <- paste( system.file(package="EBImage"), "images", sep="/" )
  a <- readImage( paste(ddir, c("A04w0.jpg", "A04w1.jpg"), sep="/") )
  rgb <- channel(a[,,1],"asred") + channel(a[,,2],"asgreen")

  ## detecting nuclei by watershed
  t <- thresh( blur(a[,,2], 4, 2), 15, 15)
  t <- opening( closing(t, morphKern(9)) )
  w <- watershed( distmap(t) )

  ## generating mask for propagate
  mask <- thresh( a[,,1], 50, 50, -0.01)
  mask <- closing( mask, morphKern(13) )
  mask <- opening( mask, morphKern(11) )
  mask <- erode(mask)

  ## propagate with the mask and gradient-driven metric
  wx <- propagate( a[,,1], w, mask, 0.0001, 2)
  prev <- paintObjects(wx, paintObjects(w, rgb))
  if ( interactive() ) display(prev)

  ## pure Voronoi: no mask and almost no gradient term in metric
  wx1 <- propagate( a[,,1], w, NULL, 1e8, 2)
  prev1 <- paintObjects(wx1, rgb)
  if ( interactive() ) display(prev1)
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

