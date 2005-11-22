\name{Filters 2D Enhancing}
\alias{Filters 2D Enhancing}

\alias{im.addNoise}
\alias{im.blur}
\alias{im.despeckle}
\alias{im.enhance}
\alias{im.gaussian}
\alias{im.median}
\alias{im.redNoise}
\alias{im.sharpen}
\alias{im.spread}
\alias{im.unsharpmask}

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
    im.sharpen(x, radius = 1, sigma = 0.5, ...)
    im.unsharpmask(x, radius = 2, sigma = 0.5, amount = 5, threshold = 2, ...)

    im.enhance(x, ...)
    im.despeckle(x, ...)
    im.redNoise(x, order = NULL, ...)

    im.median(x, radius = 2, ...)
    im.blur(x, radius = 1, sigma = 0.5, ...)
    im.gaussian(x, width = 1, sigma = 0.5, ...)

    im.spread(x, amount = 3, ...)
    im.addNoise(x, type = "gaussian", ...)
}

\arguments{
    \item{x}{An instance of class \code{\link{Image2D}}. Must be 2D or 3D. For 3D objects, filters
        will be applied to every single image composed of first two dimensions. See \code{\dots}
        argument for further details on 3D image processing.}
    \item{\dots}{Two arguments can be accepted here: \code{do.stack} with default \code{FALSE} and
        \code{warn.stack} with default \code{TRUE}. \code{do.stack} tells the routine if 3D
        images should be processed or if an error is to be returned. If 3D images are to be
        processed, \code{warn.stack} specifies if a warning message should be displayed on a 3D
        image}
    \item{radius}{For \code{sharpen} and \code{blur}, \code{radius} specifies the radius of the Gaussian,
        in pixels, not counting the center pixel; for \code{median} - the radius of the neighbourhood,
        for which the median is to be calculated.}
    \item{sigma}{Specifies the standard deviation, in pixel, of Laplacian for \code{sharpen} and
        \code{blur} and of Gaussian for \code{unsharpmask} and \code{gaussian}.}
    \item{amount}{For \code{unsharpmask}, specifies the percentage of the difference between the
        original and the blur image that is added back to the original; for \code{spread} - the
        amount by which pixels are spread within the image.}
    \item{threshold}{Specifies the threshold in pixels needed to apply the difference amount in
        \code{unsharpmask}.}
    \item{order}{Order of noise reduction, or NULL}
    \item{width}{The number of neighbouring pixels to be included in convolution for
        Gaussian blur (\code{gaussian}). For example, a width of 1 gives a '3x3' convolution mask.}
    \item{type}{A character value that specifies the type of noise to be added. Possible values are:
        'uniform', 'gaussian', 'multi' (for multiplicative Gaussian), 'impulse', 'laplace', 'poisson'.}
}

\value{
    New instance of class 'Image' with the same characteristics as the original, but with filter
    applied (to every single 2D image if source is 3D).
}

\details{
    \code{im.sharpen} sharpens edges in image. \code{im.unsharpmask} does similar job t
    \code{sharpen} by adding a mask to the image, which is obtained by blurring the original image.

    \code{im.enhance} enhances image by minimizing noise. \code{im.despeckle} reduces
    speckle noise. \code{im.redNoise} reduces noise by using a noise peak elimination im.

    \code{im.median} filters image by replacing each pixel component with the median color in a
    circular neighborhood. \code{im.blur} applies a simple blur filter and \code{im.gaussian}
    applies a Gaussian blur im.

    \code{im.spread} spreads pixels randomly withing the image and \code{im.addNoise} adds
    a specified noise type to the image.
}

\seealso{
   \code{\link{Image2D}}, \code{\link{Filters 2D}}, \code{\link{Filters 2D Segmentation}}, \code{\link{Filters 2D Colors}}, \code{\link{Filters 2D Transforms}}
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

