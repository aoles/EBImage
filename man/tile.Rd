\name{tile}

\alias{tile}

\concept{image manipulation}

\title{ Generates a tiled image from a stack of images }

\description{
  Generates a tiled image from a stack of images
}


\section{ Usage }{
  \preformatted{

    # S4 methods for classes 'Image' and 'list':
    tile(x, nx=10, grid.wd=1, grid.col="#E4AF2B", bg.col="black")
  }
}

\section{ Arguments }{ \describe{

  \item{\code{x}}{Objects of \code{\link{Image-class}} or a list of such objects.
    Images must be grayscale and carry object indexing information, like those
    returned by \code{\link{watershed}} or \code{\link{propagate}}. }

  \item{\code{nx}}{ The number of tiled images in a row. }

  \item{\code{grid.wd}}{ The width of the grid lines between tiled images, can be 0. }

  \item{\code{grid.col}}{ The color of the grid lines (if \code{grid.wd > 0}).
    This will be converted to Grayscale if the color mode of \code{x} is Grayscale. }

  \item{\code{bg.col}}{ The color of the background for extra tiles. This will be
    converted to Grayscale if the color mode of \code{x} is Grayscale. }

}}

\section{ Value }{

  An object of \code{\link{Image-class}} in the same color mode as \code{x}.
}


\seealso{
  \code{
    \link{stackObjects}, \link{combine}
  }
}

\author{
    2007 Oleg Sklyar : \email{osklyar@ebi.ac.uk}
}

\keyword{file}

