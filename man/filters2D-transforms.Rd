\name{Transform 2D Filters}
\alias{Transform 2D Filters}

\alias{rotate}
\alias{sampleImage}
\alias{scaleImage}

\concept{image transformation}
\concept{image processing}

\title{
    Filters to Resize and Rotate 2D Images and Image Stacks
}

\description{
    Filters to Resize and Rotate 2D Images and Image Stacks
}

\usage{
    rotate(x, degrees = 90)
    sampleImage(x, dx, dy)
    scaleImage(x, dx, dy)
}

\arguments{
    \item{x}{An instance of class \code{\link{Image2D}} or \code{\link{Image3D}}. For 3D objects, filters
        will be applied to every single image composed of first two dimensions.}
    \item{degrees}{Angle in degrees for counter-clockwise rotation of the image.}
    \item{modify}{if set to TRUE original image will be modified (non-standard R behaviour)!}
    \item{dx}{New image width.}
    \item{dy}{New image height.}
}

\value{
    A new instance of class \code{\link{Image2D}} or \code{\link{Image3D}}
    with the same characteristics as the original, but with filter applied (to every single 2D
    image if source is 3D).
}

\details{
    Transformation filters do not support argument \code{modify} and always generate a copy of the
    original image because image size changes.

    \code{rotate} rotates images counter-clockwise.

    \code{sampleImage} uses sampling algorithm to resize images, whereas \code{scaleImage} uses
    simple ratio algorithm.
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

