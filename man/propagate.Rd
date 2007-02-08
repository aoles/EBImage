\name{propagate}

\alias{propagate}

\concept{object detection}

\title{ Voronoi-based segmentation on image manifolds }

\description{
  R implementation of the Voronoi-based image segmentation on image
  manifolds [2].
}

\section{ Usage }{
  \preformatted{
    # S4 Method for Image-class

    propagate(x, seeds, mask=NULL, lambda=0.005, ext=1, ...)
  }
}

\section{ Arguments }{ \describe{

  \item{\code{x}}{An object of \code{\link{Image-class}} to be segmented,
    in the \code{\link{Grayscale}} mode with integer-absed object encoding, as
    returned by \code{\link{watershed}}. }

  \item{\code{seeds}}{An object of \code{\link{Image-class}} of the same
    size as x in all three dimensions; in the \code{\link{Grayscale}} mode
    with integer-absed object encoding, as returned by \code{\link{watershed}}.
    This image provides seed points for object detection. }

  \item{\code{mask}}{An object of \code{\link{Image-class}} of the same
    size as x in all three dimensions; in the \code{\link{Grayscale}}.
    All zero locations will be excluded from object detection. }

  \item{\code{lambda}}{ A numeric value. The regulation parameter for the
    distance calculations, larger lambda turn calculated distance to
    Euclidian. }

  \item{\code{ext}}{ Extension of the neighborhood to estimate image
    gradient, in pixels in every direction from the central point, i.e.
    ext=1 means a 3x3 neighborhood. }

  \item{\code{...}}{ Reserved for future use. }
}}

\section{ Value }{

  An image of \code{\link{Image-class}}, with the same object indexing as
  \code{seeds}. No new objects are created, only those specified by
  seeds are propagated. Use \code{\link{getObjects}} to assign the
  feature matrix.

}

\section{ Details }{

  The method operates by computing a discretized approximation of the
  Voronoi regions for given seed points on a manifold with a metric
  controlled by local image features.

  The metric is Riemannian metric defined in terms of the image I and
  a regularization parameter lambda. With this metric the
  distance between pixels used to let the given seeds grow outwards
  (propagate) is approximated by:

  $d^2 = (grad(I)^2 + lambda * (dx^2 + dy^2)) / (lambda + 1)$

  The gradient is calculated on a neighborhood of pixels (width of which
  is controlled by the argument \code{ext}) to avoid relying on single
  (noisy) pixels. Lambda regulated the weight of Euclidian distance
  in image distance calculations on the image manifold. In case of large
  lambda, d turns into Euclidian distance; for small lambda (< 0.005 for
  intensity distributions in typical cell images), the distance will be
  dominated by the local image gradients.

  The underlying \code{C++} code is based on the code from CellProfiler
  [1,3]. LGPL license was granted to use the part of CellProfiler's
  code for the propagate function. Using LGPL with this package does not
  grant you the rights to use CellProfiler's code directly under the
  terms of LGPL: you may only reuse what you see in this package
  under LGPL.
}

\seealso{
  \code{
    \link{Image-class}, \link{watershed}, \link{getObjects}
  }
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

\examples{
    if ( interactive() ) {
      ddir <- paste( system.file(package="EBImage"), "images", sep="/" )
      a <- read.image( paste(ddir, c("A04w0.jpg", "A04w1.jpg"), sep="/") )

      \dontrun{detecting nuclei by watershed}
      t <- thresh( blur(a[,,2], 4, 2), 15, 15)
      t <- opening( closing(t, morphKern(9)) )
      w <- watershed( distmap(t) )
      w <- combineObjects(w, 1, 0.4)

      \dontrun{generating mask for propagate}
      mask <- thresh( a[,,1], 50, 50, -0.01)
      mask <- closing( mask, morphKern(13) )
      mask <- opening( mask, morphKern(11) )
      mask <- erode(mask)
      \dontrun{doing propagate}
      wx <- propagate( a[,,1], w, mask, 0.0001, 2)
      prev <- paintObjects(w, channel(a[,,1],"asred") + channel(a[,,2],"asgreen"))
      prev <- paintObjects(wx, prev)
      display(prev)
    }
  }
}

\author{
Copyright (C) of the original CellProfiler code: Anne Carpenter <carpenter@wi.mit.edu>, Thouis Jones <thouis@csail.mit.edu>, In Han Kang <inthek@mit.edu>.

Copyright (C) of the EBImage implementation: Oleg Sklyar <osklyar@ebi.ac.uk> and Wolfgang Huber <huber@ebi.ac.uk>.

If reusing the code for this function, please see comments in \code{src/propagate.cpp}
}

\keyword{file}

