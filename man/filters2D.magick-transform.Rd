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
    \item{dy}{New image height. FIXME: this argument is something else, although
        ImageMagick tells it is what is written here, but the image is scaled/sampled
        proportionally taking in account only the first argument.}
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
    # create an empty image of 300x200 size
    im1 <- Image(0, c(300,200,1))
    # fill image with data (waves)
    for (i in 1:dim(im1)[[1]])
        for (j in 1:dim(im1)[[2]]) {
            im1[i,j,1] <- abs(sin(i*0.0314)*sin(i*0.0314) + cos(j*0.0314)*cos(j*0.0314))
        }
    # normalize image
    im1 <- normalize(im1)
    # segment and edge
    ed <- edge(segment(im1, 100, 0.1), 1)
    # rotate edged image
    \dontrun{display(rotate(ed, 90))}
    # scale image
    \dontrun{display(scaleImage(ed, 400, 300))}
    # sample image
    \dontrun{display(sampleImage(ed, 400, 300))}
}

\keyword{dplot}
\keyword{manip}
\keyword{array}

