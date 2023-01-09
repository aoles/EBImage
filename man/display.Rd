\name{display}

\alias{display}
\alias{plot.Image}

\concept{display}

\title{Image Display}

\description{
  Display images in an interactive JavaScript viewer or using R's built-in graphics capabilities.
}

\usage{
display(x, method, ...)

\method{plot}{Image}(x, ...)
}

\arguments{
  \item{x}{an \code{Image} object or an array.}
  \item{method}{the way of displaying images. Defaults to \code{"browser"} when R is used interactively, and to \code{"raster"} otherwise. The default behavior can be overridden by setting \code{options("EBImage.display")}. See Details.}
  \item{\dots}{arguments to be passed to the specialized display functions; for details see the sections on individual display methods.}
}

\details{
The default \code{method} used for displaying images depends on whether called from and interactive R session. If \code{interactive()} is \code{TRUE} images are displayed with the \code{"browser"} method, otherwise the \code{"raster"} method is used. This dynamic behavior can be overridden by setting \code{options("EBImage.display")} to either \code{"browser"} or \code{"raster"}.

\code{plot.Image} S3 method is a wrapper for \code{display(..., method="raster")}
}

\section{"browser" method}{
The \code{"browser"} method runs an interactive JavaScript image viewer. A list of available features along with corresponding mouse and keyboard actions is shown by pressing 'h'. This method takes the following additional arguments.
\describe{
  \item{\code{embed}}{logical(1), include images in the document as data URIs. Defaults to \code{TRUE} in non-interactive context (e.g. static R Markdown documents), otherwise to \code{FALSE}.}
  
  \item{\code{tempDir}}{character(1), file path for storing any temporary image files. Defaults to \code{tempfile("")}}
  
  \item{\code{\dots}}{arguments passed to \code{\link{createWidget}}, such as fixed \code{width} and \code{height} (in CSS units), \code{elementId}, or \code{preRenderHook}.}
  }
}

\section{"raster" method}{
The \code{"raster"} method displays images as R raster graphics. The user coordinates of the plotting region are set to the image pixel coordinates with the origin \code{(0, 0)} in the upper left corner.

By default only the first frame of an image stack is shown; a different \code{frame} can also be specified. When \code{all=TRUE} the whole image stack is rendered and the frames are automatically positioned next to each other in a grid. The grid layout can be modified through \code{nx} and \code{spacing} and \code{margin}.

This method provides to following additional arguments to \code{display}.
  \describe{
    \item{\code{interpolate}}{a logical vector (or scalar) indicating whether to apply linear interpolation to the image when drawing.}
    \item{\code{frame}}{a numeric indicating the frame number to display; only effective when \code{all = FALSE}.}
    \item{\code{all}}{logical, defaulting to \code{FALSE}. If set to \code{TRUE}, all frames of a stacked image are displayed arranged in a grid, otherwise (default) just a single frame specified in \code{frame} is displayed. The grid layout can be controlled by \code{nx}, \code{spacing} and \code{margin}.}
    \item{\code{drawGrid}}{a logical indicating whether to draw grid lines between individual frames. Defaults to \code{TRUE} unless \code{spacing} is non-zero. Line color, type and width can be specified through graphical parameters \code{col}, \code{lty} and \code{lwd}, respectively; see \code{\link{par}} for details.}
    \item{\code{nx}}{integer. Specifies the number images in a row. Negative numbers are interpreted as the number of images in a column, e.g. use \code{-1} to display a single row containing all the frames.}
    \item{\code{spacing}}{numeric. Specifies the separation between frames as a fraction of frame dimensions (positive numbers <1) or in pixels (numbers >=1). It can be either a single number or a vector of length 2, in which case its elements correspond to the horizontal and vertical spacing, respectively.}
    \item{\code{margin}}{numeric. Specifies the outer margins around the image, or a grid of images. Similarly as for \code{spacing}, different horizontal and vertical margins can be defined by providing a vector.}
    \item{\code{\dots}}{graphical parameters passed to \code{\link{par}}}
  }
}

\author{
  Andrzej Oles, \email{andrzej.oles@embl.de}, 2012-2017
}

\seealso{
  \code{\link{display-shiny}}
}

\examples{
## Display a single image
x = readImage(system.file("images", "sample-color.png", package="EBImage"))[257:768,,]
display(x)

## Display a thresholded sequence ...
y = readImage(system.file("images", "sample.png", package="EBImage"))[366:749, 58:441]
z = lapply(seq(from=0.5, to=5, length.out=6),
  function(s) gblur(y, s, boundary="replicate")
)
z = combine(z)

## ... using the interactive viewer ...
display(z)

## ... or using R's build-in raster device
display(z, method = "raster", all = TRUE)

## Display the last frame 
display(z, method = "raster", frame = numberOfFrames(z, type = "render"))

## Customize grid appearance
display(z, method = "raster", all = TRUE,
        nx = 2, spacing = 0.05, margin = 20, bg = "black")
}
