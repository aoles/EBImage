\name{Image}

\alias{Image}
\alias{Image-class}
\alias{Grayscale}
\alias{Color}
\alias{is.Image}
\alias{as.Image}
\alias{as.raster.Image}
\alias{print.Image}
\alias{median.Image}
\alias{quantile.Image}
\alias{colorMode}
\alias{colorMode<-}
\alias{getFrame}
\alias{getNumberOfFrames}
\alias{imageData}
\alias{imageData<-}
\alias{colormode}
\alias{Ops,Image,Image-method}
\alias{Ops,numeric,Image-method}
\alias{Ops,Image,numeric-method}
\alias{[,Image,ANY,ANY,ANY-method}
\alias{[,Image-method}
\alias{show,Image-method}
\alias{image,Image-method}
\alias{hist,Image-method}

\title{Image class}

\description{
  The package \code{EBImage} uses the class \code{Image} to store and process
  images. Images are stored as multi-dimensional arrays containing the pixel
  intensities. The class \code{Image} extends the base class \code{array} and
  uses the \code{colormode} slot to store how the color information of
  the multi-dimensional data is handled.

  The \code{colormode} slot could be either \code{Grayscale} or \code{Color}. Greyscale images containing an alpha channel are stored with \code{colormode = Color}. In both modes, the first two dimensions of the underlying array are understood to be the spatial
  dimensions of the image. In the \code{Grayscale} mode, the remaining dimensions
  contain other images. In the the \code{Color} mode, the third dimension contains color channels of the image: either
  the red, green, blue and, possibly, alpha channel, or the grey and alpha channels; the remaining dimensions contain
  other images.

  All methods of the package \code{EBImage} work either with \code{Image} objects or
  multi-dimensional arrays but in the latter case, the color mode is assumed to be
  \code{Grayscale}.
}

\usage{
  Image(data, dim, colormode)
  as.Image(x)
  is.Image(x)
  as.raster.Image(y, i)

  colorMode(y)
  colorMode(y) <- value

  imageData(y)
  imageData(y) <- value

  getFrame(y, i, type='total')
  getNumberOfFrames(y, type='total')
}


\arguments{
  \item{data}{A vector or array containing the pixel intensities of an image. If missing, a default 1x1
  null array is used.}

  \item{dim}{A vector containing the final dimensions of an \code{Image} object. If missing, equals to
  \code{dim(data)}.}

  \item{colormode}{A numeric or a character string containing the color mode which could be
  either \code{Grayscale} or \code{Color}. If missing, equals to
  \code{Grayscale}.}

  \item{x}{An R object.}

  \item{y}{An \code{Image} object or an array.}

  \item{i}{A numeric.}

  \item{value}{For \code{colorMode}, a numeric or a character string containing the color mode which
    could be either \code{Grayscale} or \code{Color}. For \code{imageData}, an \code{Image} object or an array.}

  \item{type}{A character string containing \code{total} or \code{render}. Default is \code{total}.}
}

\value{
  \code{Image} and \code{as.Image} return a new \code{Image} object.

  \code{is.Image} returns TRUE if \code{x} is an \code{Image} object and FALSE otherwise.

  \code{as.raster.Image} coerces an Image object to a raster object. For stacked images the \code{i}th frame is returned (by default the first one).

  \code{colorMode} returns the color mode of \code{y} and \code{colorMode<-} changes the color mode
  of \code{y}.

  \code{imageData} returns the array contained in an \code{Image} object.
}

\details{
  Depending of \code{type}, \code{getNumberOfFrames} returns the total number of frames contained
  in the object \code{y} or the number of renderable frames. The total number of frames is independent
  of the color mode and is equal to the product of all the dimensions except the two first ones. The
  number of renderable frames is equal to the total number of frames in the \code{Grayscale} color mode
  and is equal to the product of all the dimensions except the three first ones in the \code{Color} color mode.

  \code{getFrame} returns the i-th frame contained in the image \code{y}. If \code{type} is \code{total}, the
  function is unaware of the color mode and returns an xy-plane. If \code{render}, the function returns the
  i-th image as shown by the \code{display} function.
}

\seealso{
  \code{\link{readImage}}, \code{\link{writeImage}}, \code{\link{display}}
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2005-2007
}

\examples{
  s1 = exp(12i*pi*seq(-1, 1, length=300)^2)
  y = Image(outer(Im(s1), Re(s1)))
  if (interactive()) display(normalize(y))

  x = Image(rnorm(300*300*3),dim=c(300,300,3), colormode='Color')
  if (interactive()) display(x)

  w = matrix(seq(0, 1, len=300), nc=300, nr=300)
  m = abind::abind(w, t(w), along=3)
  z = Image(m, colormode='Color')
  if (interactive()) display(normalize(z))

  y = Image(c('red', 'violet', '#ff51a5', 'yellow'), dim=c(71, 71))
  if (interactive()) display(y)

  ## colorMode example
  x = readImage(system.file('images', 'nuclei.tif', package='EBImage'))
  x = x[,,1:3]
  if (interactive()) display(x, title='Cell nuclei')
  colorMode(x)=Color
  if (interactive()) display(x, title='Cell nuclei in RGB')
}


