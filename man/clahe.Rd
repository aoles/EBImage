\name{clahe}

\alias{clahe}

\title{Contrast Limited Adaptive Histogram Equalization}

\description{
  Improve contrast locally by performing adaptive histogram equalization.
}

\usage{
clahe(x, nx = 8, ny = nx, bins = 256, limit = 2, keep.range = FALSE) 
}

\arguments{
  \item{x}{an \code{Image} object or an array.}
  \item{nx}{integer, number of contextual regions in the X direction (min 2, max 256)}
  \item{ny}{integer, number of contextual regions in the Y direction (min 2, max 256)}
  \item{bins}{integer, number of greybins for histogram ("dynamic range"). Smaller values (eg. 128) speed up processing while still producing good quality output.}
  \item{limit}{double, normalized clip limit (higher values give more contrast).  A clip limit smaller than 0 results in standard (non-contrast limited) AHE.}
  \item{keep.range}{logical, retain image minimum and maximum values rather then use the full available range}
}

\value{
  An \code{Image} object or an array, containing the filtered version
  of \code{x}.
}

\details{
  Adaptive histogram equalization (AHE) is a contrast enhancement 
  technique which overcomes the limitations of standard histogram equalization.
  Unlike ordinary histogram equalization the adaptive method redistributes the lightness values 
  of the image based on several histograms, each corresponding to a distinct section of the image.
  It is therefore useful for improving the local contrast and enhancing the definitions of edges in each region of an image.
  However, AHE has a tendency to overamplify noise in relatively homogeneous 
  regions of an image. Contrast limited adaptive histogram equalization (CLAHE) prevents this by limiting the amplification.
  
  The function is based on the implementation by Karel Zuiderveld [1].
  This implementation assumes that the X- and Y image dimensions are an integer
  multiple of the X- and Y sizes of the contextual regions.
  The input image \code{x} should contain pixel values in the range from 0 to 1, 
  inclusive; values lower than 0 or higher than 1 are clipped before applying 
  the filter. Internal processing is performed in 16-bit precision.
  If the image contains multiple channels or frames, 
  the filter is applied to each one of them separately.
}

\note{
  The interpolation step of the original implementation by Karel Zuiderveld [1] 
  was modified to use double precision arithmetic in order to make the filter 
  rotationally invariant for even-sized contextual regions, and the result is 
  properly rounded rather than truncated towards 0 in order to avoid a 
  systematic shift of pixel values.
}

\seealso{
  \code{\link{equalize}}
}

\author{
  Andrzej Oleś, \email{andrzej.oles@embl.de}, 2017
}

\examples{
  x = readImage(system.file("images", "sample-color.png", package="EBImage"))
  y = clahe(x)  
  display(y)
}

\references{
  [1] K. Zuiderveld: Contrast Limited Adaptive Histogram Equalization. In: P. Heckbert: Graphics Gems IV, Academic Press 1994
}
