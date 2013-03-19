\name{display}

\alias{display}

\concept{display}

\title{Image Display}

\description{
  Displays images using an interactive JavaScript viewer or R's built-in graphics capabilities.
}

\usage{
display(x, title = deparse(substitute(x), width.cutoff = 500L, nlines = 1), method = c("browser", "raster"), frame, all = FALSE)
}

\arguments{
  \item{x}{an \code{Image} object or an array.}
  \item{title}{a character string used as a window title.}
  \item{method}{the method used to display images: either \code{browser} (default) for viewing images in a WWW browser, or \code{raster} for viewing them as raster graphics using R's native functions.}
  \item{frame}{a numeric indicating the frame number; only works in conjunction with \code{method = "raster"} and \code{all = FALSE}.}
  \item{all}{should all frames of a stacked image be displayed, or just a single \code{frame}?}
}

\value{
  Invisible \code{NULL}.
}

\details{
  By default (\code{method = "browser"}) images are displayed in a WWW browser using a JavaScript based image browser. Multiple windows or tabs can be opened in this way. Pressing H displays a list of available features along with corresponding mouse and keyboard actions.

If \code{method = "raster"} images are displayed using R's built-in \code{rasterImage} function. By default only the first frame of a stacked image is rendered; a different frame can be specified using \code{frame}. When \code{all = TRUE} all frames are rendered side by side and automatically positioned in a grid.
}

\note{For viewing images in a WWW browser a compatible browser with JavaScript enabled is required (e.g. Mozilla Firefox).}

\author{
  Andrzej Oles, \email{andrzej.oles@gmail.com}, 2012
}

\references{
  Mozilla Firefox: \url{http://www.firefox.com}
}

\examples{
  ## Display a single image
  x = readImage(system.file("images", "lena-color.png", package="EBImage"))
  if (interactive()) {
    display(x, "Lena")
    display(x, method = "raster")
  }

  ## Display a thresholded sequence ...
  y = readImage(system.file("images", "lena.png", package="EBImage"))
  yt = list()
  for (t in seq(0.1, 5, len=9)) yt=c(yt, list(gblur(y, s=t)))
  yt = combine(yt)

  ## ... using the browser viewer ...
  if (interactive()) display(yt, "Blurred Lenas")

  ## ... or using R's build-in raster functions
  if (interactive()) display(resize(yt, 256, 256), method = "raster", all = TRUE)

  ## Display the last frame 
  if (interactive()) display(yt, method = "raster", frame = getNumberOfFrames(yt, type = "render"))
}
