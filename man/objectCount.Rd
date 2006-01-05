\name{objectCount}

\alias{objectCount}

\docType{function}

\title{Counts Objects in an Image or a Stack of Images}
\description{
    Takes a Distance Map as input to count all objects in an image, determine their locations and sizes.
    The algorithm works by sequential detection of the maximum on the distance map and spreading the
    background from this maximum point into all directions while the values go downwards, until the
    background is found, the end of image, the values start go upwards with a given tolerance (difference in
    pixel).
}

\usage{
    objectCount(x, minArea = 20, maxRadius = 100, tolerance = 1, maxObjects = 1000)
}
\arguments{
    \item{x}{An instance of class \code{\link{Image2D}} or \code{\link{Image3D}} being a Distance Map.
        For 3D objects,  the algorithm will be applied to every single image composed of first two
        dimensions.}
    \item{minArea}{Objects with area smaller than this (in pixel) will be omitted.}
    \item{maxRadius}{Maximum radius of detected objects. If the algorithm spreads behind this value,
        pixel is considered outside of object automatically.}
    \item{tolerance}{The value of increasing pixel difference that still counts to the same object, if
        larger algorithm terminates in that direction.}
    \item{maxObjects}{Maximum number of objects - no more objects are added as soon as this value is
        reached and algorithm terminates.}
}

\value{
    For 2D images returns a matrix with two values for every object found - its index and its size in pixel.

    For 3D images a list of such matrices is returned, one for each 2D image.
}

\seealso{
   \code{\link{Image2D}}, \code{\link{Image3D}}, \code{\link{Filters 2D}, \code{\link{distMap}}}
}

\source{
    The C code for this routine is available in \code{inst/src/objectcount.cpp} of this package.
}

\references{
    Algorithm by Oleg Sklyar, EBI 2005.
}

\author{
    Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}

\examples{
    im = read.image("http://www.ebi.ac.uk/~osklyar/projects/EBImage/examples/segmented.tif")
    # get the distance map
    dm = distMap(im)
    # get object count and object indexes
    objects = objectCount(dm)
    index = objects[1,]
    # display object centers as red dots on the segmented image
    im = toRGB(im)
    im[index] = 255
    # run this: display(im)
}

\keyword{dplot}
\keyword{manip}
\keyword{array}

