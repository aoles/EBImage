\name{objectCount}

\alias{objectCount}

\docType{function}

\title{Counts Objects in an Image or a Stack of Images}
\description{
Takes a distance map as input to count the objects in an image and determine their locations and sizes.
The algorithm works iteratively and greedily by finding the maximum on the
distance map, extending the region from the maximum point in all
directions as long as the distance map values go downwards (allowing
for a certain tolerance) or until the
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
    objectCount(x, ref = NULL, minArea = 20, maxRadius = 100, tolerance = 1, maxObjects = 1000, modify = FALSE)
}
\arguments{
    \item{x}{An instance of class \code{\link{Image2D}} or \code{\link{Image3D}} being a Distance Map.
        For 3D objects,  the algorithm will be applied to every single image composed of first two
        dimensions.}
    \item{ref}{An optional original (not segmented) image (from which segmented x was obtained) to enable
        evaluation of object intensities. It must have the same
	dimensions as \code{x}.}
    \item{minArea}{Objects with area smaller than this (in pixel) will be omitted.}
    \item{maxRadius}{Maximum radius of detected objects. If the algorithm spreads behind this value,
        pixel is considered outside of object automatically.}
    \item{tolerance}{The value of increasing pixel difference that still counts to the same object, if
        larger algorithm terminates in that direction.}
    \item{maxObjects}{Maximum number of objects - no more objects are added as soon as this value is
        reached and algorithm terminates.}
    \item{modify}{Logical scalar. If TRUE, \code{x} is replaced by an
      image in which the segmented objects are coded by integer numbers
      1, 2, 3, ....}
}

\value{
  A matrix with as many rows as found objects and five columns:
  the index (\code{ID}) of the image in which the object was found (0
  for \code{Image2D}, image index of \code{Image3D}).
  % FIXME:
  % a value of "1" would be more consistent for Image2D considering that
  % an Image2D is (or should be) just a stack with one image.
  \code{x} coordinate, \code{y} coordinate of the object,
  % FIXME:
  % how are these determined if the object is larger than 1 pixel (and not round)?
  object size, intensity/size (if size is non-zero)
  % FIXME:
  % what is an object of size zero?
  or just intensity value (meaningful only if \code{ref} argument is not
  NULL.)
  % FIXME:
  % 1. this is unclear. If 'ref' is not given, then the most
  % straightforward behavior would be that the matrix does not contain
  % the column 'intensity'
  % 2. If 'intensity and 'size' are returned, then 'intensity/size' can
  % be quickly computed on the fly by division, and returning it as a
  % separate columns seems like a waste of space.
}

\seealso{
   \code{\link{Image2D}}, \code{\link{Image3D}}, \code{\link{Filters 2D}, \code{\link{distMap}}}
}

%\source{
%    The C code for this routine is available in \code{inst/src/objectcount.cpp} of this package.
%}

% FIXME - this should be clear from the 'author' section
%\references{
%    Algorithm by Oleg Sklyar, EBI 2005.
%}

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

