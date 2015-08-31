\name{channel}

\alias{channel}
\alias{rgbImage}
\alias{toRGB}

\concept{color conversion}
\concept{color mode}

\title{Color and image color mode conversions}

\description{
  \code{channel} handles color space conversions between image modes.
  \code{rgbImage} combines \code{Grayscale} images into a \code{Color} one.
  \code{toRGB} is a wrapper function for convenient grayscale to RGB color space conversion; the call \code{toRGB(x)} returns the result of \code{channel(x, 'rgb')}.
}

\usage{ 
channel(x, mode)
rgbImage(red, green, blue)
toRGB(x)
}

\arguments{
  \item{x}{An \code{Image} object or an array.}
  
  \item{mode}{ A character value specifying the target mode for
    conversion. See Details.}
  
  \item{red, green, blue}{\code{Image} objects in \code{Grayscale}
    color mode or arrays of the same dimension. If missing, a
    black image will be used.} 
}

\value{
  An \code{Image} object or an array.
}

\details{
  Conversion modes:
  \describe{
    \item{\code{rgb}}{Converts a \code{Grayscale} image or an array
    into a \code{Color} image, replicating RGB channels.}

  \item{\code{gray, grey}}{Converts a \code{Color} image into a
    \code{Grayscale} image, using uniform 1/3 RGB weights.}
    
  \item{\code{luminance}}{Luminance-preserving \code{Color} to \code{Grayscale} conversion
  using CIE 1931 luminance weights: 0.2126 * R + 0.7152 * G + 0.0722 * B.}

  \item{\code{red, green, blue}}{Extracts the \code{red}, \code{green} or
    \code{blue} channel from a \code{Color} image. Returns a
    \code{Grayscale} image.}

  \item{\code{asred, asgreen, asblue}}{Converts a \code{Grayscale}
    image or an array into a \code{Color} image of the specified hue.}
}

NOTE: \code{channel} changes the pixel intensities, unlike \code{colorMode}
which just changes the way that EBImage renders an image.
}

\seealso{
  \code{\link{colorMode}}
} 

\examples{
 x = readImage(system.file("images", "shapes.png", package="EBImage"))
 display(x)
 y = channel(x, 'asgreen')
 display(y)

 ## rgbImage
 x = readImage(system.file('images', 'nuclei.tif', package='EBImage'))
 y = readImage(system.file('images', 'cells.tif', package='EBImage'))
 display(x, title='Cell nuclei')
 display(y, title='Cell bodies')

 cells = rgbImage(green=1.5*y, blue=x)
 display(cells, title='Cells')
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}
