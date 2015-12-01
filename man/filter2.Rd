\name{filter2}

\alias{filter2}

\title{2D Convolution Filter}

\description{
  Filters an image using the fast 2D FFT convolution product.
}

\usage{
filter2(x, filter, boundary = c("circular", "replicate"))
}

\arguments{
  \item{x}{An \code{Image} object or an array.}

  \item{filter}{An \code{Image} object or an array, with odd spatial
    dimensions. Must contain only one frame.}
    
  \item{boundary}{Behaviour at image borders. The default is to wrap the image around borders. For other modes see details.}
}

\value{
  An \code{Image} object or an array, containing the filtered version
  of \code{x}.
}

\details{
  Linear filtering is useful to perform low-pass filtering (to blur
  images, remove noise...) and high-pass filtering (to detect
  edges, sharpen images). The function \code{makeBrush} is useful to
  generate filters.

  The default \code{"circular"} behaviour at boundaries is to wrap the image around borders.
  In the \code{"replicate"} mode pixels outside the bounds of the image are assumed to equal the nearest border pixel value.
  Numeric values of \code{boundary} yield linear convolution by padding the image with the given value.
  
  If \code{x} contains multiple frames, the filter is applied separately to each frame.
  
}

\seealso{
  \code{\link{makeBrush}}, \code{\link{convolve}}, \code{\link{fft}}, \code{\link{blur}}
}

\author{
  Andrzej Oleś, Gregoire Pau
}

\examples{
  x = readImage(system.file("images", "sample-color.png", package="EBImage"))
  display(x, title='Sample')

  ## Low-pass disc-shaped filter
  f = makeBrush(21, shape='disc', step=FALSE)
  display(f, title='Disc filter')
  f = f/sum(f)
  y = filter2(x, f)
  display(y, title='Filtered image')

  ## High-pass Laplacian filter
  la = matrix(1, nc=3, nr=3)
  la[2,2] = -8
  y = filter2(x, la)
  display(y, title='Filtered image')
}
