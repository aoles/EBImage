\name{morphology}

\alias{dilate}
\alias{closing}
\alias{erode}
\alias{opening}
\alias{whiteTopHat}
\alias{blackTopHat}
\alias{selfComplementaryTopHat}
\alias{makeBrush}

\title{Perform morphological operations on images}

\description{
  Functions to perform morphological operations on binary and grayscale images.
}

\usage{
dilate(x, kern)
erode(x, kern)
opening(x, kern)
closing(x, kern)
whiteTopHat(x, kern)
blackTopHat(x, kern)
selfComplementaryTopHat(x, kern)

makeBrush(size, shape=c('box', 'disc', 'diamond', 'Gaussian', 'line'), step=TRUE, sigma=0.3, angle=45)
}

\arguments{
  \item{x}{An \code{Image} object or an array.}
  
  \item{kern}{An \code{Image} object or an array, containing the
    structuring element. \code{kern} is considered as a binary image, with
    pixels of value 0 being the background and pixels with values other than 0 being the foreground.}
  
  \item{size}{A numeric containing the size of the brush in pixels. This should be an odd number; even numbers are rounded to the next odd one, i.e., \code{size = 4} has the same effect as \code{size = 5}. Default is 5}
  
  \item{shape}{A character vector indicating the shape of the brush. Can
    be \code{box}, \code{disc}, \code{diamond}, \code{Gaussian} or \code{line}. Default is
    \code{box}.}
  
  \item{step}{a logical indicating if the brush is binary. Default is
    \code{TRUE}. This argument is relevant only for the \code{disc} and
    \code{diamond} shapes.}

  \item{sigma}{An optional numeric containing the standard deviation of
    the \code{Gaussian} shape. Default is 0.3.}

  \item{angle}{An optional numeric containing the angle at which the line should be drawn. The angle is
    one between the top of the image and the line.}
}

\details{
  \code{dilate} applies the mask \code{kern} by positioning its center over every pixel of the
  image \code{x}, the output value of the pixel is the maximum value of \code{x}
  covered by the mask. In case of binary images this is equivalent of putting the mask over every background pixel, and setting all the pixels under the mask to foreground if any of these pixels is from the foreground.

  \code{erode} applies the mask \code{kern} by positioning its center over every pixel of the
  image \code{x}, the output value of the pixel is the minimum value of \code{x}
  covered by the mask. In case of binary images this is equivalent of putting the mask over every foreground pixel, and setting all the pixels under the mask to background if any of these pixels is from the background.

  \code{opening} is an erosion followed by a dilation and \code{closing} is a dilation followed by an erosion.

  \code{whiteTopHat} returns the difference between the original image \code{x} and its opening by the structuring element \code{kern}.
  
  \code{blackTopHat} subtracts the original image \code{x} from its closing by the structuring element \code{kern}.

  \code{selfComplementaryTopHat} is the sum of the \code{whiteTopHat} and the \code{blackTopHat}, simplified
  the difference between the \code{closing} and the \code{opening} of the image.

  \code{makeBrush} generates brushes of various sizes and shapes that can be used
  as structuring elements.

  \subsection{Processing Pixels at Image Borders (Padding Behavior)}{
    Morphological functions position the center of the structuring element over each pixel in the input image. For pixels close to the edge of an image, parts of the neighborhood defined by the structuring element may extend past the border of the image. In such a case, a value is assigned to these undefined pixels, as if the image was padded with additional rows and columns. The value of these padding pixels varies for dilation and erosion operations. For dilation, pixels beyond the image border are assigned the minimum value afforded by the data type, which in case of binary images is equivalent of setting them to background. For erosion, pixels beyond the image border are assigned the maximum value afforded by the data type, which in case of binary images is equivalent of setting them to foreground.}
}

\value{
  \code{dilate}, \code{erode}, \code{opening}, \code{whiteTopHat}, \code{blackTopHat} and
  \code{selfComplementaryTopHat} return the transformed \code{Image} object
  or array \code{x}, after the corresponding morphological operation.
    
  \code{makeBrush} generates a 2D matrix containing the desired brush.
}

\note{
  Morphological operations are implemented using the efficient Urbach-Wilkinson algorithm [1]. Its required computing time is independent of both the image content and the number of gray levels used.
}

\author{
  Ilia Kats, \email{ilia-kats@gmx.net}, 2012
  Andrzej Oles, \email{andrzej.oles@embl.de}, 2015
}

\examples{	
  x = readImage(system.file("images", "shapes.png", package="EBImage"))
  kern = makeBrush(5, shape='diamond')  
  
  display(x)
  display(kern, title='Structuring element')
  display(erode(x, kern), title='Erosion of x')
  display(dilate(x, kern), title='Dilatation of x')

  ## makeBrush
  display(makeBrush(99, shape='diamond'))
  display(makeBrush(99, shape='disc', step=FALSE))
  display(2000*makeBrush(99, shape='Gaussian', sigma=10))
}

\references{
  [1] E. R. Urbach and M.H.F. Wilkinson, "Efficient 2-D grayscale morphological transformations with arbitrary flat structuring elements", IEEE Trans Image Process 17(1), 1-8, 2008
}
