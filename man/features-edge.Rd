\name{edgeFeatures}

\alias{edgeFeatures}
\alias{edgeProfile}

\concept{feature extraction}

\title{ Extraction of edge profiles and edge features from image objects }

\description{
  Extract the edge profile from image objects, computing for each object
  the distances of edge points to the object geometric center, at
  different rotation angles.
}

\usage{
  edgeFeatures(x, ref)
  edgeProfile(x, ref, n=32, fft=TRUE, scale=TRUE, rotate=TRUE)
 
}

\arguments{
  \item{x}{An \code{Image} object or an array containing object masks.
    Object masks are sets of pixels with the same unique integer value.}
  
  \item{ref}{An \code{Image} object or an array, containing the
    intensity values of the objects.}

  \item{n}{ An integer value giving the number of angle measures. The
    full circle of \code{[-pi,pi]} is divided into \code{n-1} segments, at
    which edges the profile is approximated. }

  \item{fft}{ A logical value. If \code{TRUE}, the resulting profile is the
    \code{\link{fft}} transformation of the distance profile giving the
    frequences of angular changes in shape. }

  \item{scale}{ A logical value. If \code{TRUE}, the resulting profile
    is scaled by the effective radius (calcualted as part of
    \code{link{hull.features}}) making the profile scale invariant. }

  \item{rotate}{ A logical value. If \code{TRUE}, the resulting profile
    is shifted by the object's roation angle (calculated from the
    \code{\link{moments}} on the \code{ref} image, if provided, and on the hull
    otherwise. }

}

\value{
 \code{edgeFeatures} returns a matrix (or a list of matrices if
  \code{x} contains multiple frames) of features computed of the objects
  present in \code{x} and using the intensity values of \code{ref}.
  
  \code{edgeProfile} returns a matrix ((or a list of matrices if
  \code{x} contains multiple frames) of profile values, corresponding,
  from left to right, to the equidistant divisions of the range
  \code{[-pi,pi]} if \code{fft} is \code{FALSE}. Otherwise, the matrix
  contains the FFT transform of the corresponding distance profile.
}

\details{
  \code{edgeFeatures} returns the following features:
  \itemize{
    \item \code{e.irr}: difference between the smallest and largest
    distance profile values.
    \item \code{e.f2Pi}: 2pi/1 frequency component of the distance
      profile.
    \item \code{e.fPi}: 2pi/2 frequency component of the distance
      profile.
    \item \code{e.f2Pi3}: 2pi/3 frequency component of the distance
      profile.
    \item \code{e.fPi2}: 2pi/4 frequency component of the distance
      profile.
    }
}

\seealso{
  \code{\link{getFeatures}}, \code{\link{ocontour}}
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2007
}

\examples{
   example(getFeatures)
}

