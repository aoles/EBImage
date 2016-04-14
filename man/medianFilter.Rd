\name{medianFilter}

\alias{medianFilter}

\title{2D constant time median filtering}

\description{
  Process an image using Perreault's modern constant-time median filtering algorithm [1, 2].
}

\usage{
medianFilter(x, size, cacheSize=512)
}

\arguments{
  \item{x}{an \code{Image} object or an array.}
  \item{size}{integer, median filter radius.}
  \item{cacheSize}{integer, the L2 cache size of the system CPU in kB.}
}

\value{
  An \code{Image} object or an array, containing the filtered version
  of \code{x}.
}

\details{
  Median filtering is useful as a smoothing technique, e.g. in the removal
  of speckling noise.
  
  For a filter of radius \code{size}, the median kernel is a \code{2*size+1}
  times \code{2*size+1} square.
  
  Images are processed using 16-bit precision. The behavior at image boundaries
  is such as the source image \code{x} has been padded with pixels whose values
  equal the nearest border pixel value.
  
  If the image contains multiple channels or frames, 
  the filter is applied to each one of them separately.
}

\seealso{
  \code{\link{gblur}}
}

\author{
  Joseph Barry, \email{joseph.barry@embl.de}, 2012

  Andrzej Oleś, \email{andrzej.oles@embl.de}, 2016
}

\examples{
  x = readImage(system.file("images", "nuclei.tif", package="EBImage"))
  display(x, title='Nuclei')
  y = medianFilter(x, 5)  
  display(y, title='Filtered nuclei')
}

\references{
  [1] S. Perreault and P. Hebert, "Median Filtering in Constant Time", IEEE Trans Image Process 16(9), 2389-2394, 2007
  
  [2] \url{http://nomis80.org/ctmf.html}
}
