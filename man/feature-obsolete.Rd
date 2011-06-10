\name{obsolete}

\alias{moments}
\alias{cmoments}
\alias{smoments}
\alias{rmoments}
\alias{edgeFeatures}
\alias{edgeProfile}
\alias{haralickMatrix}
\alias{haralickFeatures}
\alias{hullFeatures}
\alias{zernikeMoments}

\title{Obsolete feature computation functions}

\description{
  The following functions to compute object features are
  obsolete. Please use \code{computeFeatures} instead.
}

\usage{
moments(x, ref)
cmoments(x, ref)
rmoments(x, ref)
smoments(x, ref, pw=3, what="scale")
edgeFeatures(x, ref)
edgeProfile(x, ref, n=32, fft=TRUE, scale=TRUE, rotate=TRUE)
haralickFeatures(x, ref, nc = 32)
haralickMatrix(x, ref, nc = 32)
hullFeatures(x)
 zernikeMoments(x, ref, N = 12, R = 30)
}

\arguments{
  \item{x}{An \code{Image} object or an array containing object masks.
    Object masks are sets of pixels with the same unique integer value.}
    
  \item{ref}{An \code{Image} object or an array, containing the
    intensity values of the objects.}
  
  \item{pw}{A numeric value specifying the maximum moment order to
    compute. Default is 3.}
  
  \item{what}{A character string partially matching \code{central} or
    \code{scale}, specifiying what kind of moments to compute. Default
    is \code{scale}.}

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
  
  \item{nc}{ A numeric value. Specifies the number of gray levels
     used to compute the co-occurrence matrix. Default value is 32.}
   
   \item{N}{A numeric. Indicates the maximal order of Zernike polynomials
     to be computed. Default value is 12.}

  \item{R}{A numeric. Defines the radius of the circle in pixels around
    object centers from which the features are calculated.}
}

\value{
  Obsolete.
}

\seealso{
  \code{\link{computeFeatures}}, \code{\link{computeFeatures.basic}},
  \code{\link{computeFeatures.shape}}, \code{\link{computeFeatures.moment}},
  \code{\link{computeFeatures.haralick}}
}

\author{
  Gregoire Pau, \email{gregoire.pau@embl.de}, 2011
}

\examples{
 example(computeFeatures)
}


