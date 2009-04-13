\name{moments}

\alias{moments}
\alias{cmoments}
\alias{smoments}
\alias{rmoments}

\concept{features extraction}

\title{Image moments and moment invariants}

\description{
  Computes moments and invariant moments from image objects.
}

\usage{
  moments(x, ref)
  cmoments(x, ref)
  rmoments(x, ref)
  smoments(x, ref, pw=3, what="scale")
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
}

\value{
  \code{moments}, \code{cmoments} and \code{rmoments} returns a matrix
  (or a list of matrices if \code{x} contains multiple frames) of
  features computed of the objects present in \code{x} and using the
  intensity values of \code{ref}.

  \code{smoments} returns a 3-dimensional array of size (\code{pw+1},
  \code{pw+1}, \code{n}) where \code{n} is the maximal value of
  \code{x}, or a list of such arrays if \code{x} contains multiple frames.
}

\details{
  \code{moments} returns the features returned by \code{cmoments}, 
  \code{rmoments} and the features \code{m.n20}, \code{m.n11},
  \code{m.n02}, \code{m.theta}, \code{m.l1}, \code{m.l2} and
  \code{m.ecc} for each objet. See \code{Definitions} for details
  on features.

  \code{cmoments} returns the features \code{m.pxs}, \code{m.int}, \code{m.x}
  and \code{m.y} for each objet. 

  \code{rmoments} returns Hu's translation/rotation/scale 7 invariant
  moments \code{m.Ik} for each object, where k spans from 1 to 7.

  \code{smoments} returns for each object the central moments \eqn{mu_pq}
  if \code{what}=\code{central} or the scale invariant moments \eqn{nu_pq} if
  \code{what}=\code{scale}. The variables (p, q) span the range [0, pw].  
}

\section{Definitions}{
  Image moments \eqn{m_pq} are defined for the k-th object in \code{x} by:
  \eqn{m_pq = sum_{i,j st. x_ij = k} i^p * j^q * ref_ij}.

  Central moments \eqn{mu_pq} are defined for the k-th object in \code{x} by:
  \eqn{mu_{pq} = sum_{i,j st. x_ij = k} (i - m_10/m_00)^p * (j -
    m_01/m_00)^q * ref_{ij}}. Central moments are invariant by translation.

  Scale moments \eqn{nu_pq} are defined for the k-th object in \code{x} by:
  \eqn{nu_{pq} = mu_pq / mu_00^(1+ (p+q)/2)}. Scale moments are
  invariant by translation and scaling.

  Features returned by \code{moments}, \code{cmoments} and
  \code{rmoments} are defined by:
  \itemize{
    \item \code{m.pxs} = \eqn{sum_{i,j st. x_ij = k} 1 }
    \item \code{m.int} = \eqn{m_00}
    \item \code{m.x} = \eqn{m_10 / m_00}
    \item \code{m.y} = \eqn{m_01 / m_00}
    \item \code{m.n20} = \eqn{nu_20}
    \item \code{m.n11} = \eqn{nu_11}
    \item \code{m.n02} = \eqn{nu_02}
    \item \code{m.theta} = \eqn{0.5 * atan(2*mu_11/(mu_20-mu_02))}
    \item \code{m.l1} = largest eigenvalue of the covariance matrix [\eqn{mu_20}, \eqn{mu_11} ;
     \eqn{mu_11}, \eqn{mu_02}]/\eqn{m_00}
    \item \code{m.l2} = smallest eigenvalue of the covariance matrix
    \item \code{m.ecc} = \eqn{sqrt(1-m.l2/m.l1)}
    \item \code{m.Ik} = k-th Hu's moment, see References.
  }

  Properties:
  \itemize{
   \item \code{m.pxs} is the surface of the objects, in pixels.
   \item \code{m.int} is the mass of the object.
   \item (\code{m.x}, \code{m.y}) is the center of gravity of the object.
   \item \code{m.n20}, \code{m.n11} and \code{m.n02} are translation/scale
   invariant moments.
   \item \code{m.theta} characterizes the orientation of an object in radians.
   \item 2*sqrt(\code{m.l1}) and 2*sqrt(\code{m.l2}) are the semi-major
   and semi-minor axes of the object and have the dimension of a length.
   \item \code{m.Ik} is the translation/scale/rotation invariant k-th Hu's moment.
 }
}

\references{
  M. K. Hu, \emph{Visual Pattern Recognition by Moment Invariants}, IRE Trans.
  Info. Theory, vol. IT-8, pp.179-187, 1962
  
  \emph{Image moments}: \url{http://en.wikipedia.org/wiki/Image_moments}
}

\seealso{
  \code{\link{getFeatures}}, \code{\link{bwlabel}}, \code{\link{watershed}}, \code{\link{propagate}}
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2007
}

\examples{
  ## load cell nucleus images
  x = readImage(system.file('images', 'nuclei.tif', package='EBImage'))
  if (interactive()) display(x)

  ## computes object mask
  y = thresh(x, 10, 10, 0.05)
  y = opening(y, makeBrush(5, shape='disc'))
  mask = fillHull(bwlabel(y))
  if (interactive()) display(mask, title='Cell nuclei')

  ## moments
  m = moments(mask, x)
  mc = do.call(rbind, m)
  print(mc[1:5,])
  cat('Mean nucleus size is', mean(mc[,'m.pxs']), '\n')
  cat('Mean nucleus eccentricity is', mean(mc[,'m.ecc']), '\n')

  ## paint nuclei with an eccentricity higher than 0.85
  maskb = mask
  for (i in 1:dim(mask)[3]) {
    id = which(m[[i]][,'m.ecc']<0.85)
    z = maskb[,,i]
    z[!is.na(match(z, id))] = 0
    maskb[,,i] = z
  } 
  img = paintObjects(maskb, channel(x, 'rgb'), col=c(NA, 'red'), opac=c(0,0.7))
  if (interactive()) display(img, title='Nuclei with high eccentricity')
}


