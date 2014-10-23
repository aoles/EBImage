\name{otsu}

\alias{otsu}

\title{Calculate Otsu's threshold}

\description{
Returns a threshold value based on Otsu's method, which can be then used to reduce the grayscale image to a binary image.
}

\usage{
otsu(x, range = c(0, 1), levels = 256)
}

\arguments{
  \item{x}{A \code{Grayscale} \code{Image} object or an array.}
  
  \item{range}{Numeric vector of length 2 specifying the histogram range used for thresholding.}
  
  \item{levels}{Number of grayscale levels.}
}

\value{
A vector of length equal to the total number of frames in \code{x}. Each vector element contains the Otsu's threshold value calculated for the corresponding image frame.
}

\details{
Otsu's thresholding method [1] is useful to automatically perform clustering-based image thresholding. The algorithm assumes that the distribution of image pixel intensities follows a bi-modal histogram, and separates those pixels into two classes (e.g. foreground and background). 
The optimal threshold value is determined by minimizing the combined intra-class variance.

The threshold value is calculated for each image frame separately resulting in a output vector of length equal to the total number of frames in the image.

The default number of \code{levels} corresponds to the number of gray levels of an 8bit image. It is recommended to adjust this value according to the bit depth of the processed data, i.e. set \code{levels} to 2^16 = 65536 when working with 16bit images.
}

\seealso{
  \code{\link{thresh}}
}

\references{
   [1] Nobuyuki Otsu, "A threshold selection method from gray-level histograms". IEEE Trans. Sys., Man., Cyber. 9 (1): 62-66. doi:10.1109/TSMC.1979.4310076 (1979)
}

\author{
  Philip A. Marais \email{philipmarais@gmail.com}, Andrzej Oles \email{andrzej.oles@embl.de}, 2014
}

\examples{
  x = readImage(system.file("images", "lena.png", package="EBImage"))
  display(x)
  
  ## threshold using Otsu's method
  y = x > otsu(x)
  display(y)
}

\keyword{manip}
