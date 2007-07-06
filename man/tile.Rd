\name{tile}

\alias{tile}

\concept{image manipulation}

\title{ Generate a tiled image from a stack of images }

\description{
  Given an image stack (or a list of such those), \code{tile} generates
  for each stack a single image with frames tiled. 
}

\section{ Usage }{
  \preformatted{
    # S4 methods for classes x='Image' and x='list':

    tile(x, nx=10, lwd=1, fg.col="#E4AF2B", bg.col="gray")
}}

\section{ Arguments }{ \describe{
  \item{\code{x}}{An object of \code{\link{Image-class}} or a list of such objects.
    Images must be grayscale and carry object indexing information, like those
    returned by \code{\link{watershed}} or \code{\link{propagate}}. }

  \item{\code{nx}}{ The number of tiled images in a row. }

  \item{\code{lwd}}{ The width of the grid lines between tiled images, can be 0. }

  \item{\code{fg.col}}{ The color of the grid lines (if \code{lwd > 0}).
    This will be converted to Grayscale if the color mode of \code{x} is Grayscale. }

  \item{\code{bg.col}}{ The color of the background for extra tiles. This will be
    converted to Grayscale if the color mode of \code{x} is Grayscale. }
}}

\section{ Value }{
  An image of the same class and in the same color mode as \code{x}.
}

\section{ Details }{
  The function for \code{x='list'} is a useful addition to \code{stackObjects},
  which returns a list of stacks, thus it can be directly used on the result of
  the latter.
}

\seealso{
  \code{
    \link{stackObjects}, \link{combine}
}}

\author{
  Oleg Sklyar <\email{osklyar@ebi.ac.uk}>, 2006-2007
}

\keyword{file}

