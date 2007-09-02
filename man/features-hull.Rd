\name{hullFeatures}

\alias{hullFeatures}
\alias{hullFeatures,IndexedImage-method}

\concept{feature extraction}

\title{ Extraction of hull features from images of indexed objects }

\description{
  Hull features are a set of numeric descriptors of the hull of an object. These
  include coordinates, perimeter, size, acircularity etc.
}

\usage{

  \S4method{hullFeatures}{IndexedImage}(x, ...)
  
}

\arguments{

  \item{x}{An object of \code{\linkS4class{IndexedImage}}.}
  
  \item{...}{ Reserved. }

}

\value{
  For a single frame, a matrix of descriptors with objects in rows and features
  in columns. For image stacks, a list of such matrices.

}

\details{
  The extracted features are (names carry an \code{h.} prefix to indicate hull
  features): \code{x,y} - coordinates of the geometric center, \code{s} - size (area),
  \code{p} - perimeter, \code{pdm} - mean distance to perimeter (from the center),
  \code{pdsd} - standard deviation of the distance to perimeter, \code{effr} - 
  effective radius (is the radius of a circle with the same area), \code{acirc} -
  acircularity (fraction of pixels outside of the circle with \code{r=reff}), 
  \code{sf} - shape factor (\code{per / ( 2 * sqrt(Pi * s))}), \code{edge} - 
  number of pixels at the edge of the image, \code{theta} - hull's rotation
  angle (calculated without taking intensity values into account), \code{s2maj} -
  2 times semi major (square root of the larger eigenvalue of the 
  \code{\link{moments}} covariance matrix) - correlates with the distance from
  the center to the edge along the major axis, \code{s2min} - same but for the 
  smaller eigenvalue (minor axis), \code{ecc} - eccentricity 
  (\code{sqrt(eig1-eig2)/smaj}), \code{I1, I2} - first and second rotation invariant
  moments of the hull (as in \code{\link{moments}}).
}

\seealso{\code{
  \linkS4class{IndexedImage}, \link{watershed}, \link{getFeatures}, 
  \link{edgeFeatures}, \link{moments}
}}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2007
}

\examples{
  ## see example(getFeatures)
}

\keyword{manip}

