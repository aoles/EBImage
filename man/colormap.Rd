\name{colormap}

\alias{colormap}

\title{Map a Greyscale Image to Color}

\description{
  Maps a greyscale image to color using a color palette.
}

\usage{
colormap(x, palette = heat.colors(256L))
}

\arguments{
  \item{x}{an \code{Image} object of color mode \code{Grayscale}, or an array}

  \item{palette}{character vector containing the color palette}
}

\details{
  The \code{colormap} function first linearly maps the pixel intensity values
  of \code{x} to the integer range \code{1:length(palette)}. It then 
  uses these values as indices to the provided color palette to create
  a color version of the original image.
  
  The default palette contains 256 colors, which is the typical number of
  different shades in a 8bit grayscale image.
}

\value{
  An \code{Image} object of color mode \code{Color}, containing the color-mapped version
  of \code{x}.
}

\author{
  Andrzej Oleś, \email{andrzej.oles@embl.de}, 2016
}

\examples{
  x = readImage(system.file("images", "sample.png", package="EBImage"))

  ## posterize an image using the topo.colors palette
  y = colormap(x, topo.colors(8))

  display(y, method="raster")

  ## mimic MatLab's 'jet.colors' colormap
  jet.colors = colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
  y = colormap(x, jet.colors(256))

  display(y, method="raster")
}
