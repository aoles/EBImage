\name{tile}

\alias{tile}
\alias{tile,Image-method}
\alias{tile,list-method}

\concept{image manipulation}

\title{ Generate a tiled image from a stack of images }

\description{
  Given an image stack (or a list of such those), \code{tile} generates
  for each stack a single image with frames tiled. 
}

\usage{

  \S4method{tile}{Image}(x, nx=10, lwd=1, fg.col="#E4AF2B", bg.col="gray", ...)
  \S4method{tile}{list}(x, nx=10, lwd=1, fg.col="#E4AF2B", bg.col="gray", ...)

}

\arguments{
  \item{x}{An object of \code{\linkS4class{Image}} or a list of such objects.
    Images must be grayscale and carry object indexing information, like those
    returned by \code{\link{watershed}} or \code{\link{propagate}}. }

  \item{nx}{ The number of tiled images in a row. }

  \item{lwd}{ The width of the grid lines between tiled images, can be 0. }

  \item{fg.col}{ The color of the grid lines (if \code{lwd > 0}).
    This will be converted to Grayscale if the color mode of \code{x} is \code{Grayscale}. }

  \item{bg.col}{ The color of the background for extra tiles. This will be
    converted to Grayscale if the color mode of \code{x} is \code{Grayscale}. }

  \item{...}{ Reserved. }
}

\value{
  An image of the same class and in the same color mode as \code{x}.
}

\details{
  The function for \code{x=list} is a useful addition to \code{stackObjects},
  which returns a list of stacks, thus it can be directly used on the result of
  the latter.
}

\seealso{ \code{
  \link{stackObjects}, \link{combine}
}}

\examples{
  ## see example(stackObjects)
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2006-2007
}

\keyword{manip}

