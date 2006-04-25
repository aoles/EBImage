\name{Transformation}
\alias{Transformation}

\alias{rotate}
\alias{sampleImage}
\alias{scaleImage}

\concept{image transformation}
\concept{image processing}

\title{
    Filters to resize and rotate images
}

\description{
    Filters to resize and rotate images.
}

\usage{
    rotate(x, degrees = 90)
    sampleImage(x, dx, dy)
    scaleImage(x, dx, dy)
}

\arguments{
    \item{x}{An instance of class \code{\link{Image}}.}
    \item{degrees}{Angle in degrees for counter-clockwise rotation of the image.}
    \item{dx}{New image width.}
    \item{dy}{New image height.}
}

\value{
    A new instance of class \code{\link{Image}}
    with the same characteristics as the original, but with filter applied.
}

\details{
    \code{rotate} rotates images counter-clockwise.

    \code{sampleImage} uses sampling algorithm to resize images, whereas \code{scaleImage} uses
    simple ratio algorithm.
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

