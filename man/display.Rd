\name{display}

\alias{display}

\concept{display}

\title{Image Display}

\description{
  Display images using an interactive JavaScript viewer or R's built-in graphics capabilities.
}

\usage{
display(x, 
        title = deparse(substitute(x), width.cutoff = 500L, nlines = 1),
        method,
        frame, all = FALSE)
}

\arguments{
  \item{x}{an \code{Image} object or an array.}
  \item{title}{a character string used as a window title.}
  \item{method}{the method used to display images, can be \code{browser} or \code{raster}. Defaults to \code{browser} when R is used interactively and \code{raster} otherwise. The default behavior can be overridden by setting \code{options("EBImage.display")}. See Details.}
  \item{frame}{a numeric indicating the frame number; only works in conjunction with \code{method = "raster"} and \code{all = FALSE}.}
  \item{all}{should all frames of a stacked image be displayed, or just a single \code{frame}?}
}

\value{
  Invisible \code{NULL}.
}

\details{
The default method used for displaying images depends on whether called from and interactive R session. If \code{interactive()} returns \code{TRUE} images are displayed using the \code{"browser"} method, otherwise the \code{"raster"} method is used. This behavior can be overridden and the default display method fixed regardless the interactivity by setting \code{options("EBImage.display")} to either \code{"browser"} or \code{"raster"}.

The \code{"browser"} method runs an interactive JavaScript image viewer using the default web browser. Multiple windows or tabs can be opened in this way. Pressing 'h' displays a list of available features along with corresponding mouse and keyboard actions.

The \code{"raster"} method displays images as raster graphics using R's build-in functions. By default only the first frame of a stacked image is shown; a different frame can be specified using the \code{frame} argument. When \code{all=TRUE} all frames are rendered and automatically positioned next to each other in a grid. The user coordinates of the plotting region are set to the image pixel coordinates with the origin \code{(0, 0)} in the upper left corner.
}

\note{For the \code{"browser"} method a compatible web browser with JavaScript enabled is required (e.g. Mozilla Firefox).}

\author{
  Andrzej Oles, \email{andrzej.oles@embl.de}, 2012
}

\references{
  \href{http://www.firefox.com}{Mozilla Firefox}
}

\examples{
  ## Display a single image
  x = readImage(system.file("images", "sample-color.png", package="EBImage"))[257:768,,]
  display(x, "Sample")

  ## Display a thresholded sequence ...
  y = readImage(system.file("images", "sample.png", package="EBImage"))
  yt = list()
  for (t in seq(0.1, 5, len=9)) yt=c(yt, list(gblur(y, s=t)))
  yt = combine(yt)

  ## ... using the browser viewer ...
  display(yt, "Blurred images")

  ## ... or using R's build-in raster functions
  display(resize(yt, 256, 256), method = "raster", all = TRUE)

  ## Display the last frame 
  display(yt, method = "raster", frame = numberOfFrames(yt, type = "render"))
}
