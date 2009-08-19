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

## a paraboloid mountain in a plane
n = 400
a = (n/4)^2 - matrix(
       (rep(1:n, times=n) - n/2)^2 + (rep(1:n, each=n) - n/2)^2,
       nrow=400,
       ncol=400)
a[a<0] = 0
a = normalize(a)

## 'color' version of a
ac = rgbImage(green=a, red=a)

## two seeds: a little rectangle on the left,
##  a large rectangle on the right
seeds = Image(dim=dim(a))
seeds[1:10, n/2+(-5:5)] = 1
seeds[n+(-5:0), (n*0.2):(n*0.8)] = 2

lambda = 10^seq(-10, 0, by=2)
segmented = Image(dim=c(dim(a), 3, length(lambda)), colormode="Color")
flip = (seeds>0 | a>0)
for(i in seq(along=lambda)) {

  prop = propagate(a, seeds, lambda=lambda[i]) - 1

  img = paintObjects(prop, ac, col="lightblue") 
  segmented[,,,i] = paintObjects(seeds, img, col=rep("lightblue", 2))

}

if(interactive()){
  plot(a[200,], main="cut through 'a' at x=200")
  display(segmented, title="Voronoi regions")
}

}

\author{
  Original CellProfiler code: Anne Carpenter <carpenter@wi.mit.edu>,
  Thouis Jones <thouis@csail.mit.edu>, In Han Kang <inthek@mit.edu>.

  Port for this package: Greg Pau, Oleg Sklyar, Wolfgang Huber
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


