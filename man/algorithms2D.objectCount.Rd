\name{objectCount}

\alias{objectCount}

\docType{function}

\title{Counts features in images or stacks}

\description{
    Takes a distance map as input to count objects in an image and determine their coordinates
    and sizes. The algorithm works iteratively and greedily by finding the maximum on the
    distance map, extending the region from the maximum point in all directions as long as
    the distance map values go downwards (allowing for a certain tolerance) or until the
    background is found or the boundary of the image is hit. Then the region
    is masked and the algorithm is applied to the remainder of the image.
}

% FIXME (wh):
% what about this algorithm:
% 1. Find all local maxima
% 2. Merge those that are too close together, allow not only for
% maximal point but also maximal regions (e.g. ridges)
% 3. Define as many regions as there as maxima, and seed them by the
% maxima.
% 4. Apply a simultaneous floodfill to all the regions
% (I might try out this algorithm as soon as a I find time; on advantage
% that I see is that it works simultaneously on the regions rather than
% greedily one after another, greedy algorithms are simply but often
% rather suboptimal)

\usage{
    objectCount(x, ref = NULL, minArea = 20, maxRadius = 100, tolerance = 1, maxObjects = 1000)
}
\arguments{
    \item{x}{An instance of class \code{\link{Image}} containing a distance map.}
    \item{ref}{An optional reference (original) image to enable evaluation of feature intensities.
        It must be of the same size as \code{x}.}
    \item{minArea}{Features with area smaller than this value (in pixel) will be omitted.}
    \item{maxRadius}{Maximum radius of detected features. If the algorithm spreads behind this value,
        pixel is considered outside of object automatically.}
    \item{tolerance}{The value of increasing pixel difference that still counts to the same object, if
        larger algorithm terminates in that direction.}
    \item{maxObjects}{Maximum number of objects - no more objects are added as soon as this value is
        reached and algorithm terminates.}
}

\value{
    A matrix with as many rows as the number of features found and five columns:
    index (\code{ID}) of the image in which the feature was found.
    % FIXME:
    % a value of "1" would be more consistent for Image considering that
    % an Image is (or should be) just a stack with one image.
    \code{x} coordinate, \code{y} coordinate of the feature (correspond to the location
    of the local maxima, from where the feature was detected), object size (in pixels),
    intensity/size.
  % FIXME:
  % 1. this is unclear. If 'ref' is not given, then the most
  % straightforward behavior would be that the matrix does not contain
  % the column 'intensity'
  % 2. If 'intensity and 'size' are returned, then 'intensity/size' can
  % be quickly computed on the fly by division, and returning it as a
  % separate columns seems like a waste of space.
}

\seealso{
   \code{\link{Image}}, \code{\link{Filters 2D}, \code{\link{distMap}}}
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
}

\keyword{dplot}
\keyword{manip}
\keyword{array}

