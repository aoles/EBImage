\name{Image}

\alias{Image}
\alias{Image-class}
\alias{Grayscale}
\alias{TrueColor}
\alias{Color}
\alias{is.Image}
\alias{as.Image}
\alias{colorMode}
\alias{colormode}

\title{Image class}

\description{
  The package \code{EBImage} uses the class \code{Image} to store and process
  images. Images are stored as multi-dimensional arrays containing the pixel
  intensities. The class \code{Image} extends the base class \code{array} and
  uses the \code{colormode} slot to store how the color information of the multi-dimensional
  data should be handled.

  Colormode could be either \code{Grayscale} or \code{Color}. In both modes, the
  two first dimensions of the underlying array are understood to be the spatial
  dimensions of the image. In the \code{Grayscale} mode, the remanining dimensions
  contain other images. In the the \code{Color} mode, the third dimension contains
  the red, green and blue channels of the image and the remaining dimensions contains
  other images. The color mode \code{TrueColor} also exists but is deprecated.

  All methods of the package \code{EBImage} works either with \code{Image} objects or
  multi-dimensional arrays but in the latter case, the color mode is assumed to be
  \code{Grayscale}.
}

\usage{
  Image(data, dim, colormode)
  as.Image(x)
  is.Image(x)

  colorMode(y)
  colorMode(y)<-colormode

  getNumberOfFrames(y, type='total')
}


\arguments{
  \item{data}{A vector or array containing the pixel intensities of an image. If missing, a default 1x1 
  null array is used.}

  \item{dim}{A vector containing the final dimensions of an \code{Image} object. If missing, equals to
  \code{dim(data)}.}    

  \item{colormode}{A numeric or a character string containing the color mode which could be 
  either \code{Grayscale} or \code{Color}.} 
  
  \item{x}{An R object.}

  \item{y}{An \code{Image} object or an array.}

  \item{type}{A character string containing \code{total} or \code{render}.}
}

\value{
  \code{Image} and \code{as.Image} return a new \code{Image} object.

  \code{is.Image} returns TRUE if \code{x} is an \code{Image} object and FALSE otherwise.

  \code{colorMode} returns the color mode of \code{y} and \code{colorMode<-} changes the color mode 
  of \code{y}.

  Depending of \code{type}, \code{getNumberOfFrames} returns the total number of frames contained
  in the object \code{y} or the number of renderable frames. The total number of frames is independent
  of the color mode and is equal to the product of all the dimensions except the two first ones. The
  number of renderable frames is equal to the total number of frames in the \code{Grayscale} color mode
  and is equal of the product of all the dimensions except the three first ones in the \code{Color} color mode.
}

\seealso{
  \code{\link{readImage}}, \code{\link{display}}
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2005-2007
}

\examples{
  s1 = exp(12i*pi*seq(-1, 1, length=300)^2)
  y = Image(outer(Im(s1), Re(s1)))
  if (interactive()) display(normalize(y))

  x = Image(rnorm(100*100*3),dim=c(100,100,3), colormode='Color')
  if (interactive()) display(x)	

  s2 = exp(7i*pi*seq(-1, 1, length=300)^2)
  m = abind(outer(Im(s1), Re(s1)), outer(Im(s1), Re(s2)), outer(Im(s2), Re(s2)), along=3)
  z = Image(m, colormode='Color')
  if (interactive()) display(normalize(z))		
}

\keyword{file}

