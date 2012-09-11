\name{displayRaster}

\alias{displayRaster}
\title{Image Display}

\description{
 Display images as raster objects using the R build-in graphics capabilities.
}
\usage{
displayRaster(image, frame)
}

\arguments{
  \item{image}{an \code{\linkS4class{Image}} object}
  \item{frame}{A numeric indicating which frame to draw; if not specified, all frames are rendered in a grid.}
}
\details{Displays images as raster graphics using the \code{rasterImage} function. By default all frames are rendered and positioned automatically in a grid; when a the frame number is specified only a single frame is displayed.
}

\author{Andrzej Oles, \email{andrzej.oles@gmail.com}, 2012}

\examples{
   x = readImage(system.file("images", "lena-color.png", package="EBImage"))
   y = list()
   for (t in seq(0.1, 5, len=9)) y = c(y, list(gblur(x, s=t)))
   y = combine(y)

   ## display single frame
   displayRaster(y,1)

   ## display all frames 
   displayRaster(y)
}
