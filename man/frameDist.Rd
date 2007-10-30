\name{frameDist}

\alias{frameDist}
\alias{frameDist,Image,Image-method}
\alias{frameDist,Image,missing-method}

\concept{image manipulation}

\title{ Calculate pairwise distances between image frames or frames of two images }

\description{
  Given two images with multiple frames, \code{frameDist} calculates a matrix of
  distances between frames (the more similar are the images, the smaller are
  the distances). Prerequisite: images should be centered and rotationally and
  scale aligned. The function is recommended with \code{\link{stackObjects}} with
  arguments \code{rotate=combine=TRUE}.
}

\usage{
  \S4method{frameDist}{Image,Image}(x, y, r, g, b, blur=TRUE, 
      method="dist", verbose, ...)
  \S4method{frameDist}{Image,missing}(x, y, r, g, b, blur=TRUE, 
      method="dist", verbose, ...)
}

\arguments{
  \item{x,y}{A image stacks. }

  \item{r,b,g}{ If \code{x} and \code{y} are in TrueColor mode, these values
    specify weights of red, green and blue channels in the resulting distance. }

  \item{blur}{ A logical indicating whether frames ought to be blurred before
    comparison (\code{TRUE} recommended). }

  \item{method}{ Method to use: \code{dist} - Euclidian distance, \code{dot} -
    dot product. }

  \item{verbose}{ Provides additional output as the function can be lengthy. }

  \item{...}{ Reserved. }
}

\value{
  A matrix of distances. If \code{y} is missing - a square symmetric matrix for
  the distances between frames in the same image with diagonal elements set at
  \code{Inf}.
}

\details{
  For Grayscale images, the distance of each pair of frames, \code{xf} and 
  \code{yf}, is calculated as \code{mean(abs(xf-yf))}, where \code{mean} is 
  taken over the pixels with at least \code{xf} or \code{yf} being non-zero. 
  For TrueColor images, the distance is calculated as 
  \code{mean(sqrt(r*rc^2+g*gc^2+b*bc^2))}, where \code{rc,gc,bc} are \code{[0,1]}
  ranged values of red, green and blue channels and \code{r,g,b} are the weights.
  Again, the mean is taken only over non-zero pixels.
}

\seealso{ \code{
  \link{stackObjects}
}}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2007
}

\keyword{manip}

