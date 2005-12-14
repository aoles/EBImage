\name{Enhancing 2D Filters}
\alias{Enhancing 2D Filters}

\alias{noise}
\alias{blur}
\alias{despeckle}
\alias{enhance}
\alias{gaussFilter}
\alias{medianFilter}
\alias{redNoise}
\alias{sharpen}
\alias{spread}
\alias{unsharpMask}

\concept{image enhancing}
\concept{image processing}

\title{
    Filters to Enhance Appearance of 2D Images and Image Stacks
}

\description{
    Several filters to enhance the appearance of 2D images and image stacks. The filters can be
    roughly divided into 4 subcategories: edge enhancing and image sharpen filters, noise reduction
    filters, blur filters, and noise addition filters. In exactly this sequence the filters are
    listed below. Operate on both grayscale and RGB images.
}

\usage{
    sharpen(x, radius = 1, sigma = 0.5)
    unsharpMask(x, radius = 2, sigma = 0.5, amount = 5, threshold = 2)

    enhance(x)
    despeckle(x)
    redNoise(x, order = NULL)

    blur(x, radius = 1, sigma = 0.5)
    medianFilter(x, radius = 2)
    gaussFilter(x, width = 1, sigma = 0.5)

    spread(x, amount = 3)
    noise(x, type = "gaussian")
}

\arguments{
    \item{x}{An instance of class \code{\link{Image2D}} or \code{\link{Image3D}}. For 3D objects, filters
        will be applied to every single image composed of first two dimensions.}
    \item{radius}{For \code{sharpen} and \code{blur}, \code{radius} specifies the radius of the Gaussian,
        in pixels, not counting the center pixel; for \code{medianFilter} - the radius of the neighbourhood,
        for which the median is to be calculated.}
    \item{sigma}{Specifies the standard deviation, in pixel, of Laplacian for \code{sharpen} and
        \code{blur} and of Gaussian for \code{unsharpMask} and \code{gaussFilter}.}
    \item{amount}{For \code{unsharpMask}, specifies the percentage of the difference between the
        original and the blur image that is added back to the original; for \code{spread} - the
        amount by which pixels are spread within the image.}
    \item{threshold}{Specifies the threshold in pixels needed to apply the difference amount in
        \code{unsharpMask}.}
    \item{order}{Order of noise reduction, or NULL}
    \item{width}{The number of neighbouring pixels to be included in convolution for
        Gaussian blur (\code{gaussFilter}). For example, a width of 1 gives a '3x3' convolution mask.}
    \item{type}{A character value that specifies the type of noise to be added. Possible values are:
        'uniform', 'gaussian', 'multi' (for multiplicative Gaussian), 'impulse', 'laplace', 'poisson'.}
}

\value{
    New instance of class \code{\link{Image2D}} or \code{\link{Image3D}} with the same characteristics
    as the original, but with filter applied (to every single 2D image if source is 3D).
}

\details{
    \code{sharpen} sharpens edges in image. \code{unsharpmask} does similar job to
    \code{sharpen} by adding a mask to the image, which is obtained by blurring the original image.

    \code{enhance} enhances image by minimizing noise. \code{despeckle} reduces
    speckle noise. \code{redNoise} reduces noise by using a noise peak elimination im.

    \code{medianFilter} filters image by replacing each pixel component with the median color in a
    circular neighborhood. \code{blur} applies a simple blur filter and \code{gaussFilter}
    applies a Gaussian blur im.

    \code{spread} spreads pixels randomly withing the image and \code{noise} adds
    a specified noise type to the image.
}

\seealso{
   \code{\link{Image2D}}, \code{\link{Image3D}}, \code{\link{Filters 2D}}
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

