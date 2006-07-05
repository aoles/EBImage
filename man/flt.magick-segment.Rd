\name{Segmentation}
\alias{Segmentation}

\alias{segment}
%\alias{ffill}
%\alias{ffillEdge}
\alias{edge}

\concept{image segmentation}
\concept{image processing}

\title{
    Image segmentation filters
}

\description{
    Image segmentation filters operating on both grayscale and RGB images.
}

\usage{
    segment(x, cluster = 1, smooth = 1.5)
%    ffill(x, col, row, color)
%    ffillEdge(x, col, row, color)
    edge(x, radius = 1)
}

\arguments{
    \item{x}{An instance of class \code{\link{Image}}.}
    \item{cluster}{Numeric scalar specifying the cluster threshold, that
        is the number of pixels a cluster must exceed for it to be considered valid.}
    \item{smooth}{Numeric scalar specifying the smoothing parameter
        which is used to eliminate noise in the second derivative of the histogram.
        A larger value results in a smoother second derivative.}
%    \item{col}{x-position of the target pixel.}
%    \item{row}{y-position of the target pixel.}
%    \item{color}{For \code{ffill} the color to replace with, for \code{ffillEdge} the color
%        of the border until which the flood fill will spread.}
    \item{radius}{Numeric scalar, the  neighborhood radius. A value of zero leads to
        an automatic radius selection.}
}

\value{
    A new instance of class \code{\link{Image}} with the same dimensions as \code{x}, but
    with filter(s) applied.
}

\details{
    \code{segment} coalesces similar image components by analyzing histograms of the color
    components and identifying units that are homogeneous by fuzzy c-means clustering.

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
    # create an empty image of 300x200 size
    im1 <- Image(0, c(300,200,1))
    # fill image with data (waves)
    for (i in 1:dim(im1)[[1]])
        for (j in 1:dim(im1)[[2]]) {
            im1[i,j,1] <- abs(sin(i*0.0314)*sin(i*0.0314) + cos(j*0.0314)*cos(j*0.0314))
        }
    # normalize image
    im1 <- normalize(im1)
    \dontrun{display(im1)}
    # threshold
    seg <- thresh(im1, 20, 20, 0)
    \dontrun{display(seg)}
    # segment
    seg1 <- segment(im1, 100, 0.1)
    \dontrun{display(seg1)}
    # edge
    ed <- edge(seg1, 1)
    \dontrun{display(ed)}
}

\keyword{dplot}
\keyword{manip}
\keyword{array}

