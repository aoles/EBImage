\name{Filters 2D Transforms}
\alias{Filters 2D Transforms}

\alias{im.rotate}
\alias{im.sample}
\alias{im.scale}

\concept{image transformation}
\concept{image processing}

\title{
    Filters to Resize and Rotate 2D Images and Image Stacks
}

\description{
    Filters to Resize and Rotate 2D Images and Image Stacks
}

\usage{
    im.rotate(x, degrees = 90, ...)
    im.sample(x, dx, dy, ...)
    im.scale(x, dx, dy, ...)
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
    \item{degrees}{Angle in degrees for counter-clockwise rotation of the image.}
    \item{dx}{New image width.}
    \item{dy}{New image height.}
}

\value{
    New instance of class 'Image' with the same characteristics as the original, but with filter
    applied (to every single 2D image if source is 3D).
}

\details{
    \code{im.rotate} rotates images counter-clockwise.

    \code{im.sample} uses sampling algorithm to resize images, whereas \code{im.scale} uses
    simple ratio algorithm.
}

\seealso{
   \code{\link{Image2D}}, \code{\link{Filters 2D}}, \code{\link{Filters 2D Enhancing}}, \code{\link{Filters 2D Segmentation}}, \code{\link{Filters 2D Colors}}
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

