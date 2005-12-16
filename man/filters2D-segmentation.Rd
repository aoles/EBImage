\name{Segmentation 2D Filters}
\alias{Segmentation 2D Filters}

\alias{segment}
\alias{ffill}
\alias{ffillEdge}
\alias{tresh}
\alias{edge}

\concept{image segmentation}
\concept{image processing}

\title{
    Filters that Add Some Segmentation Capabilities for 2D Images and Image Stacks
}

\description{
    Filters to start with image segmentation. Operate on both grayscale and RGB
    images.
}

\usage{
    segment(x, cluster = 1, smooth = 1.5)
    ffill(x, col, row, color)
    ffillEdge(x, col, row, color)
    tresh(x, width = 20, height = 20, offset = 1000, preprocess = TRUE)
    edge(x, radius = 1)
}

\arguments{
    \item{x}{An instance of class \code{\link{Image2D}} or \code{\link{Image3D}}. For 3D objects, filters
        will be applied to every single image composed of first two dimensions.}
    \item{cluster}{Specifies the cluster threashold, as the number of pixels each cluster must exceed
        for the cluster to be considered valid.}
    \item{smooth}{Smoothing threshold to eliminate noise in the second derivative of the histogram.
        A larger value results in a smoother second derivative.}
    \item{col}{x-position of the target pixel.}
    \item{row}{y-position of the target pixel.}
    \item{color}{For \code{ffill} the color to replace with, for \code{ffillEdge} the color
        of the border until which the flood fill will spread.}
    \item{width}{x-pixel region to evaluate the mean.}
    \item{height}{y-pixel region to evaluate the mean.}
    \item{offset}{In order to remove residual noise from the background, the threshold may be adjusted
        by subtracting this constant from the mean to compute the threshold.}
    \item{preprocess}{If \code{TRUE} \code{gaussFilter} with arguments 4, 2 is applied to the normalized image first.}
    \item{radius}{Specifies the radius of the neighborhood in \code{edge}, with a zero value leading to
        an automatic radius selection.}
}

\value{
    New instance of class \code{\link{Image2D}} or \code{\link{Image3D}} with the same characteristics
    as the original, but with filter applied (to every single 2D image if source is 3D).
}

\details{
    \code{segment} coalesces similar image components by analyzing histograms of the color
    components and identifying units that are homogeneous with the fuzzy c-means technique.

    \code{tresh} applies adaptive thresholding to the image. It is useful if the
    ideal threshold level is not known in advance, or if the illumination gradient is not constant
    across the image. Adaptive thresholding works by evaluating the mean of a pixel region and using the
    mean as the thresholding level.

    \code{edge} edges the image.

    \code{ffill} and \code{ffillEdge} not implemented yet!
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

