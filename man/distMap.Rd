\name{distMap}

\alias{distMap}

\docType{function}

\title{Image Distance Maps}
\description{
    \code{distMap} operates on grayscale or binary images calculating for every pixel in an
    image distance to the closest background pixel (black). The result is an image itself, in which
    each pixel carries the distance of the original pixel to background. Distance Maps are used as
    input for \code{\link{objectCount}} algorithm.
}

\usage{
    distMap(x, alg = "EBImage", modify = FALSE)
}
\arguments{
    \item{x}{An instance of class \code{\link{Image2D}} or \code{\link{Image3D}}. For 3D objects,
        the algorithm will be applied to every single image composed of first two dimensions.}
    \item{alg}{Character for the algorithm to use. Default value is for algorithm designed
        specifically for EBImage. Currently, the only alternative is Lotufo\_Zampirolli
        which does not return real distances to the background though.}
    \item{modify}{If \code{TRUE}, original image will be modified without copying the data!}
}

\value{
    If \code{modify==FALSE} a new instance of class \code{\link{Image2D}} or \code{\link{Image3D}}
    with the same characteristics as the original, but with filter applied.

    If \code{modify==TRUE} invisible original image, modified by the function.
}

\seealso{
   \code{\link{Image2D}}, \code{\link{Image3D}}, \code{\link{Filters 2D}, \code{\link{objectCount}}}
}

\source{
    The C code for this function was adapted from the \code{animal} imaging library by Ricardo Fabbri.
}

\references{
    The algorithm \code{Lotufo\_Zampirolli} is from R. Lotufo, F. Zampirolli, SIBGRAPI 2001, 100-105, 2001.
}

\author{
    Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}

\examples{
    im = read.image("http://www.ebi.ac.uk/~osklyar/projects/EBImage/examples/segmented.tif")
    # get a copy of the image
    imCopy = copyImage(im)
    # convert original image to distance map
    distMap(im, modify = TRUE)
    # calculate distance map from the originally copied image
    dm = distMap(imCopy)
    # compare original image, im, with dm or run objectCount for im or dm
}

\keyword{dplot}
\keyword{manip}
\keyword{array}

