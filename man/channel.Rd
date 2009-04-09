\name{channel}

\alias{channel}
\alias{rgbImage}

\concept{color conversion}
\concept{color mode}

\title{Color and image color mode conversions}

\description{
  \code{channel} handles color space conversions between image modes.
  \code{rgbImage} combines \code{Grayscale} images into a \code{Color} one.
}


\usage{ 
  channel(x, mode)
  rgbImage(red, green, blue)
}

\arguments{
  \item{x}{An \code{Image} object or an array.}
  
  \item{mode}{ A character value specifying the target mode for
    conversion. See Details.}
  
  \item{red, green, blue}{An \code{Image} object in \code{Grayscale}
    color mode or an array.}
}

\value{
  An \code{Image} object or an array.
}

\details{
  Conversion modes:
  \describe{
    \item{\code{rgb}}{Converts a \code{Grayscale} image or a matrix
    into a \code{Color} image, replicating RGB channels.}

  \item{\code{gray, grey}}{Converts a \code{Color} image into a
    \code{Grayscale} image, using uniform 1/3 RGB weights.}

  \item{\code{red, green, blue }}{Extracts the \code{red}, \code{green} or
    \code{blue} channel from a \code{Color} image. Returns a
    \code{Grayscale} image.}

  \item{\code{asred, asgreen, asblue}}{Converts a \code{Grayscale}
    image or an array into a \code{Color} image of the specified hue.}
}

\code{channel} changes the pixel intensities, unlike \code{colorMode}
which just changes the way that EBImage should render an image,
}

\seealso{
  \code{\link{colorMode}}
} 

\examples{
 x = readImage(system.file("images", "shapes.png", package="EBImage"))
 if (interactive()) display(x)
 y = channel(x, 'asgreen')
 if (interactive()) display(y)

 ## rgbImage
 x = readImage(system.file('images', 'nuclei.tif', package='EBImage'))
 y = readImage(system.file('images', 'cells.tif', package='EBImage'))
 if (interactive()) display(x, title='Cell nuclei')
 if (interactive()) display(y, title='Cell bodies')

 cells = rgbImage(green=y, blue=x)
 if (interactive()) display(cells, title='Cells')
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}


