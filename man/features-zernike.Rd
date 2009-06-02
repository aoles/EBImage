\name{zernikeMoments}

\alias{zernikeMoments}

\concept{feature extraction}

\title{ Extraction of Zernike moments from image objects.}

\description{
  Computation of Zernike moment features from image objects.
}

\usage{
  zernikeMoments(x, ref, N = 12, R = 30, apply.Gaussian, pseudo)
}

\arguments{
  \item{x}{An \code{Image} object or an array containing object masks.
    Object masks are sets of pixels with the same unique integer value.}
    
  \item{ref}{An \code{Image} object or an array, containing the
    intensity values of the objects.}
  
  \item{N}{A numeric. Indicates the maximal order of Zernike polynomials
    to be computed. Default value is 12.}

  \item{R}{A numeric. Defines the radius of the circle in pixels around
    object centers from which the features are calculated.}

  \item{apply.Gaussian, pseudo}{Deprecated.}
}

\value{
  Returns a matrix (or a list of matrices if \code{x} contains multiple
  frames) of features computed of the objects present in \code{x} and
  using the  intensity values of \code{ref}.
}

\details{
  Zernike features are computed by projecting image objects on the
  Zernike complex polynomials, using:

  \code{z.{nl} = (n+1) / pi * abs( sum_x,y(V*nl(x,y) * i(x,y)) )},

  where \code{0 <= l <= n, n - l} is even and \code{i(x,y)} is the
  intensity of the reference image at the coordinates \code{(x,y)} that
  fall withing a circle of radius \code{R} from the object's centre.
  Coordinates are taken relative to the object's centre.

  \code{V*nl} is a complex conjugate of a Zernike polynomial of degree
  \code{n} and angular dependence \code{l}:

  \code{Vnl(x,y) = Qnl(x,y) * exp(j*l*theta)}, where \code{j = sqrt(-1),
    theta=atan2(y,x)}, and

  \code{Qnl(x,y) = sum[_m=0^((n-l)/2)] ((-1)^m * (n-m)! * r^(n-2*m)) /
    (m! * ((n-2*m+l)/2)! * ((n-2*m-l)/2)!) }, where \code{r =
    sqrt(x^2+y^2)}.
}

\references{
  F. Zernike. \emph{Beugungstheorie des Schneidenverfahrens und seiner verbesserten
  Form, der Phasenkontrastmethode (Diffraction theory of the cut procedure and
  its improved form, the phase contrast method)}. Physica, 1:pp. 689-704, 1934.

  Jamie Shutler, \emph{Complex Zernike Moments}:
  \url{http://homepages.inf.ed.ac.uk/rbf/CVonline/LOCAL_COPIES/SHUTLER3/node11.html}
}

\seealso{\code{\link{getFeatures}}
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}; Mike Smith, \email{msmith@ebi.ac.uk}, 2007
}

\examples{
 example(getFeatures)
}

\keyword{manip}

