\name{medianFilter}

\alias{medianFilter}

\title{2D constant time median filtering}

\description{
  Filters a 16-bit image using Perreault's modern constant time median filtering algorithm [1].
}

\usage{
medianFilter(x, size, cacheSize=512)
}

\arguments{
  \item{x}{An \code{Image} object or an array.}

  \item{size}{The sizelength of the square median filter in units of pixels.}

  \item{cacheSize}{The L2 cache size of the system CPU in kB.}
}

\value{
  An \code{Image} object or an array, containing the filtered version
  of \code{x}.
}

\details{
  Median filtering is useful as a smoothing technique, e.g. in the removal
  of speckling noise.
  
  If \code{x} contains multiple frames, the filter will be applied on each frame.
  
}

\seealso{
  \code{\link{makeBrush}}, \code{\link{fft}}, \code{\link{gblur}}
}

\author{Joseph Barry, \email{joseph.barry@embl.de}, 2012}

\examples{
  x = readImage(system.file("images", "nuclei.tif", package="EBImage"))
  display(x, title='Nuclei')
  y = medianFilter(x, 5)  
  display(y, title='Filtered nuclei')
}

\references{
  [1] S. Perreault and P. Hebert, "Median Filtering in Constant Time", IEEE Trans Image Process 16(9), 2389-2394, 2007
}

