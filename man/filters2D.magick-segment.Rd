\name{Segmentation}
\alias{Segmentation}

\alias{segment}
%\alias{ffill}
%\alias{ffillEdge}
\alias{edge}

\concept{image segmentation}
\concept{image processing}

\title{
2D segmentation filters for images and image stacks
}

\description{
Image segmentation filters, operating on both grayscale and RGB images.
}

\usage{
    segment(x, cluster = 1, smooth = 1.5)
%    ffill(x, col, row, color)
%    ffillEdge(x, col, row, color)
    edge(x, radius = 1)
}

\arguments{
    \item{x}{An instance of class \code{\link{Image}}. For 3D objects, filters
        will be applied separately to each 2D slice corresponding to the first two dimensions.}
    \item{cluster}{Numeric scalar specifying the cluster threshold, that
      is the number of pixels a cluster must exceed for it to be considered valid.}
    \item{smooth}{Numeric scalar specifying the smoothing parameter
      which is used to eliminate noise in the second derivative of the histogram.
        A larger value results in a smoother second derivative.}
%    \item{col}{x-position of the target pixel.}
%    \item{row}{y-position of the target pixel.}
%    \item{color}{For \code{ffill} the color to replace with, for \code{ffillEdge} the color
%        of the border until which the flood fill will spread.}
    \item{width}{Numeric scalar, window size in \emph{x}-direction.}
    \item{height}{Numeric scalar, window size in \emph{y}-direction.}
    \item{offset}{Numeric scalar, in order to remove noise from
      the background, the threshold may be adjusted
      by subtracting this constant from the mean to compute the threshold.}
    \item{preprocess}{Logical scalar, if \code{TRUE}, the image is
      normalized and a \code{gaussFilter} with arguments \code{width=4},
      \code{sigma=2} is applied before calling the adaptive thresholding.}
    \item{radius}{Numeric scalar, the  neighborhood radius. A value of zero leads to
        an automatic radius selection.}
}

\value{
A new instance of class \code{\link{Image}}
with the same dimensions as \code{x}, but with filter(s) applied.
}

\details{
    \code{segment} coalesces similar image components by analyzing histograms of the color
    components and identifying units that are homogeneous by fuzzy c-means clustering.

    \code{adaptThresh} applies adaptive thresholding to the image. It is useful if the
    ideal threshold level is not known in advance, or if the illumination gradient is not constant
    across the image. Adaptive thresholding works by evaluating the mean of a pixel region and using the
    mean as the thresholding level.

    \code{edge} edges the image.

%    \code{ffill} and \code{ffillEdge} not implemented yet!
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

