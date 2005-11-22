\name{Filters 2D Segmentation}
\alias{Filters 2D Segmentation}

\alias{im.segment}
\alias{im.fill}
\alias{im.fillEdge}
\alias{im.tresh}
\alias{im.edge}

\concept{image segmentation}
\concept{image processing}

\title{
    Filters that Add Some Segmentation Capabilities for 2D Images and Image Stacks
}

\description{
    Several basic filters to start with image segmentation. Operate on both grayscale and RGB
    images.
}

\usage{
    im.segment(x, cluster = 1, smooth = 1.5, ...)
    im.fill(x, col, row, color, ...)
    im.fillEdge(x, col, row, color, ...)
    im.tresh(x, width, height, offset = 0, ...)
    im.edge(x, radius = 0, ...)
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
    \item{cluster}{Specifies the cluster threashold, as the number of pixels each cluster must exceed
        for the cluster to be considered valid.}
    \item{smooth}{Smoothing threshold to eliminate noise in the second derivative of the histogram.
        A larger value results in a smoother second derivative.}
    \item{col}{x-position of the target pixel.}
    \item{row}{y-position of the target pixel.}
    \item{color}{For \code{floodfill} the color to replace with, for \code{floodfillborder} the color
        of the border until which the flood fill will spread.}
    \item{width}{x-pixel region to evaluate the mean.}
    \item{height}{y-pixel region to evaluate the mean.}
    \item{offset}{In order to remove residual noise from the background, the threshold may be adjusted
        by subtracting this constant from the mean to compute the threshold.}
    \item{radius}{Specifies the radius of the neighborhood in \code{edge}, with a zero value leading to
        an automatic radius selection.}
}

\value{
    New instance of class 'Image' with the same characteristics as the original, but with filter
    applied (to every single 2D image if source is 3D).
}

\details{
    \code{im.segment} coalesces similar image components by analyzing histograms of the color
    components and identifying units that are homogeneous with the fuzzy c-means technique.

    \code{im.tresh} applies adaptive thresholding to the image. It is useful if the
    ideal threshold level is not known in advance, or if the illumination gradient is not constant
    across the image. Adaptive thresholding works by evaluating the mean of a pixel region and using the
    mean as the thresholding level.

    \code{im.edge} edges the image.

    \code{im.fill} and \code{im.fillEdge} have not yet been implemented!
}

\seealso{
   \code{\link{Image2D}}, \code{\link{Filters 2D}}, \code{\link{Filters 2D Enhancing}}, \code{\link{Filters 2D Colors}}, \code{\link{Filters 2D Transforms}}
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

