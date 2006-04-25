\name{Enhancement}
\alias{Enhancement}

\alias{noise}
\alias{blur}
\alias{despeckle}
\alias{enhance}
\alias{gaussFilter}
\alias{medianFilter}
\alias{reduceNoise}
\alias{sharpen}
\alias{spread}
\alias{unsharpMask}

\concept{image enhancing}
\concept{image processing}

\title{
    Filters to Enhance the Appearance of 2D Images and Image Stacks
}

\description{
    Several filters to enhance the appearance of 2D images and image stacks. The filters can be
    roughly divided into 4 subcategories: edge enhancing and image sharpening filters, noise reduction
    filters, blur filters, and noise addition filters. The filters are
    listed below in this sequence. They operate on both grayscale and RGB images.
}

\usage{
    sharpen(x, radius = 1, sigma = 0.5)
    unsharpMask(x, radius = 2, sigma = 0.5, amount = 5, threshold = 2)

    enhance(x)
    despeckle(x)
    reduceNoise(x, order = NULL)

    blur(x, radius = 1, sigma = 0.5)
    medianFilter(x, radius = 2)
    gaussFilter(x, width = 1, sigma = 0.5)

    spread(x, amount = 3)
    noise(x, type = "gaussian")
}

\arguments{
    \item{x}{An instance of class \code{\link{Image}}. For 3D objects, filters
        will be applied separately to each 2D slice corresponding to the first two dimensions.}
    \item{radius}{Numeric scalar. For \code{sharpen} and \code{blur}, \code{radius}
      specifies the radius of the Gaussian, in pixels, not counting the center pixel. For
      \code{medianFilter}, the radius of the neighbourhoods in which medians are calculated.}
    \item{sigma}{Numeric scalar specifying the standard deviation,
      measured in pixels, of the Laplacian for \code{sharpen} and
      \code{blur} and of the Gaussian for \code{unsharpMask} and \code{gaussFilter}.}
    \item{amount}{Numeric scalar. For \code{unsharpMask}, specifies the percentage of the difference between the
        original and the blurred image that is added back to the original; for \code{spread} - the
        amount by which pixels are spread within the image.}
    \item{threshold}{Numeric scalar. Specifies the threshold in pixels needed to apply the difference amount in
        \code{unsharpMask}.}
    \item{order}{Order of noise reduction, or NULL}
    \item{width}{The number of neighbouring pixels to be included in convolution for
        Gaussian blur (\code{gaussFilter}). For example, a width of 1 gives a '3x3' convolution mask.}
    \item{type}{A character scalar that specifies the type of noise to be added. Possible values are:
        'uniform', 'gaussian', 'multi' (for multiplicative Gaussian), 'impulse', 'laplace', 'poisson'.}
}

\value{
    A new instance of class \code{\link{Image}}
with the same dimensions as \code{x}, but with filter(s) applied.
}

\details{
    \code{sharpen} and \code{unsharpmask} sharpen images. The sharpening
    by \code{unsharpmask} is done by adding a mask to the image
    which is obtained by blurring the original image.

    \code{enhance} enhances image by minimizing noise. \code{despeckle} reduces
    speckle noise. \code{reduceNoise} reduces noise by using a noise peak elimination algorithm.

    \code{medianFilter} filters image by replacing each pixel component with the median color in a
    circular neighborhood. \code{blur} applies a simple blur filter and \code{gaussFilter}
    applies a Gaussian blur algorithm.

    \code{spread} spreads pixels randomly within the image and \code{noise} adds
    a specified noise type to the image.
}

\seealso{
   \code{\link{Image}}, \code{\link{Filters 2D}}
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}

\examples{
}

\keyword{dplot}
\keyword{manip}
\keyword{array}

