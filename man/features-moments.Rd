\name{moments}

\alias{moments}
\alias{moments,IndexedImage,Image-method}
\alias{moments,IndexedImage,missing-method}
\alias{moments,Image,missing-method}
\alias{cmoments}
\alias{cmoments,IndexedImage,Image-method}
\alias{cmoments,IndexedImage,missing-method}
\alias{smoments}
\alias{smoments,IndexedImage,Image-method}
\alias{smoments,IndexedImage,missing-method}
\alias{rmoments}
\alias{rmoments,IndexedImage,Image-method}
\alias{rmoments,IndexedImage,missing-method}

\concept{feature extraction}

\title{ Image moments and moment invariants, feature extraction }

\description{
  A set of functions to compute central, scale and rotation invariant moments
  as well as to estimate object rotation angles and elongations based on 
  central moments.
}

\usage{

  ## S4 methods for signatures 'x=IndexedImage,ref=Image'
  ## and 'x=IndexedImage,ref=missing'
  cmoments(x, ref, ...)

  \S4method{smoments}{IndexedImage,Image}(x, ref, pw=3, what="s", ...)
  \S4method{smoments}{IndexedImage,missing}(x, ref, pw=3, what="s", ...)

  ## S4 methods for signatures 'x=IndexedImage,ref=Image'
  ## and 'x=IndexedImage,ref=missing'
  rmoments(x, ref, ...)

  ## S4 methods for signatures 'x=IndexedImage,ref=Image'
  ## and 'x=IndexedImage,ref=missing'
  ## and 'x=Image,ref=missing'
  moments(x, ref, ...)
  
}

\arguments{

  \item{x}{An object of \code{\linkS4class{Image}}; a \code{\link{Grayscale}}
    object of \code{\linkS4class{Image}} in the last case.}

  \item{ref}{A \code{\link{Grayscale}} object of \code{\linkS4class{Image}}.}

  \item{pw}{ A numeric value. When calculating central os scale invariant 
    moments using \code{smoments} \code{pw} specifies the order of the 
    matrix of moments to calculate. }

  \item{what}{ A character string, or the first case insensitive letter, 
    for the type of the moments to calculate: \code{C}entral, \code{S}cale invariants,
    or \code{R}otation invariants (specifying \code{R} is essentiall the same
    as calling \code{rmoments}. }
  
  \item{...}{ Reserved. }
}

\value{

  For \code{x=IndexedImage}:
  
  For a single frame in \code{x} the result of \code{smoments} with types \code{S}
  and \code{C} is a 3D array with first two dimensions building square matrices
  of size \code{pw}*\code{pw} and the third dimension corresponds to the number of
  indexed objects in the frame. All other functions return 2D matrices with
  different moments or descriptors in columns and objects in rows.
  
  For multiple frames a list of the above will be returned.
  
  For \code{x=Image}:
  
  The result is a 2D matrix of with different moments or descriptors in 
  columns and objects in rows. (each frame is assumed to contain a single object).

}

\details{

  \code{cmoments} computes centers of the objects (\code{x=M10/M00, 
  y=M01/M00}), their "mass" (intensity, \code{int=M00}) and the area (number of 
  non-background pixels).

  \code{smoments} computes for each object in an image a square matrix of order 
  \code{pw} of either central moments (\code{mu[i,j]}) or scale invariant moments 
  (\code{nabla[i,j]}) depending on the parameter \code{what}.
  
  \code{rmoments} computes for each object the Hu's set of 7 rotation invariants.
  
  \code{moments} computes for each object a summary of interesting moments and
  derived descriprots that include mass (total intensity), location, elements of
  the covariance matrix and its eigenvalues, rotation angle and the Hu's 7 
  rotation invariants.

  Functions with signature \code{x=IndexedImage} work on images with multiple
  indexed objects in every frame. The index information is used to identify every
  object and serves as a mask resetting for every object pixels outside of the
  mask to background. The intensity is retrieved from the corresponding reference
  image. If it is not supplied, the intensity is set to 1 at each pixel of an 
  object.
  
  \code{moments} with \code{x='Image'} expect input image to contain one and only
  one object in each frame (such as those obtained by \code{\link{stackObjects}}).
  
}

\references{
  M.K. Hu, \emph{Visual Pattern Recognition by Moment Invariants}, IRE Trans.
  Info. Theory, vol. IT-8, pp.179-187, 1962
  
  \emph{Image moments}: \url{http://en.wikipedia.org/wiki/Image_moments}
}

\seealso{\code{
  \linkS4class{IndexedImage}, \link{watershed}, \link{getFeatures}
}}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2007
}

\examples{
  ## see example(getFeatures)
}

\keyword{manip}

