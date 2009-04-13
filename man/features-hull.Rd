\name{hullFeatures}

\alias{hullFeatures}

\concept{features extraction}

\title{Extraction of hull features from image objects }

\description{
  Computes hull features from image objects.
}

\usage{
  hullFeatures(x)
}

\arguments{
  \item{x}{An \code{Image} object or an array containing object masks.
    Object masks are sets of pixels with the same unique integer value.}
}

\value{
  A matrix (or a list of matrices if \code{x} contains multiple frames) of
  features computed of the objects present in \code{x}.
}

\details{
  Extracted object features are:
  \itemize{ 
  \item \code{g.x ,g.y} - coordinates of the geometric center.
  \item \code{g.s} - size in pixels.
  \item \code{g.p} - perimeter in pixels. 
  \item \code{g.pdm} - mean distance from the center to perimeter.
  \item \code{g.pdsd} - standard deviation of the distance to perimeter.
  \item \code{g.effr} - effective radius (is the radius of a circle with the same area).
  \item \code{g.acirc} - acircularity (fraction of pixels outside of the circle with radius \code{g.effr}). 
  \item \code{g.sf} - shape factor, equals to (\code{g.p/ ( 2*sqrt(i*g.s))}).
  \item \code{g.edge} - number of pixels at the edge of the image. 
  \item \code{g.theta} - hull orientation angle, in radians. See above.
  \item \code{g.l1} - largest eigeinvalue of the covariance matrix. See above.
  \item \code{g.l2} - lowest eigenvalue of the covariance matrix. See above.
  \item \code{g.ecc} - eccentricity, equals to \eqn{sqrt(1-g.l2/g.l1)}. See above.
  \item \code{g.I1, g.I2} - first and second Hu's translation/scale/rotation invariant moment. See above.
}

 The features \code{g.theta}, \code{g.l1}, \code{g.l2}, \code{g.ecc}, \code{g.I1}, 
 \code{g.I2} are computed with the function \code{moments} using the binary objects
 as intensity values, e.g. \code{g.theta = moment(x,x>0)[, 'm.theta']}. See \code{moments} for details 
 on properties of these features.
}

\seealso{
\code{\link{moments}}, \code{\link{getFeatures}}
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2007
}

\examples{
  x = readImage(system.file('images', 'shapes.png', package='EBImage'))
  x = x[110:512,1:130]
  y = bwlabel(x)
  if (interactive()) display(normalize(y), title='Objects')

  ## hullFeatures
  hf = hullFeatures(y)
  print(hf)
	
}

\keyword{manip}

