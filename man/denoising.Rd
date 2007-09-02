\name{denoise}

\alias{mediansmooth}
\alias{mediansmooth,Image-method}
\alias{blur}
\alias{blur,Image-method}
\alias{denoise}
\alias{denoise,Image-method}
\alias{despeckle}
\alias{despeckle,Image-method}
\alias{gblur}
\alias{gblur,Image-method}
\alias{noise}
\alias{noise,Image-method}
\alias{sharpen}
\alias{sharpen,Image-method}
\alias{umask}
\alias{umask,Image-method}

\concept{image color enhancement}

\title{ Noise removal, blurring and smoothing or images }

\description{
  This set of functions allows for the removal of noise, blurring and smoothing
  of images. The functions operate of images in any image mode. The functions and
  the corresponding help descriptions are ported from \emph{ImageMagick}, see
  the reference below.
}

\usage{

  # Noise removal:
  \S4method{denoise}{Image}(x, r=0, ...)
  \S4method{mediansmooth}{Image}(x, r=2, ...)
  \S4method{despeckle}{Image}(x, ...)

  # Sharpening images:
  \S4method{sharpen}{Image}(x, r=0, s=0.5, ...)
  \S4method{umask}{Image}(x, r=0, s=0.5, amount=5, t=2, ...)

  # Blurring images:
  \S4method{blur}{Image}(x, r=0, s=0.5, ...)
  \S4method{gblur}{Image}(x, r=0, s=0.5, ...)

  # Adding noise to images:
  \S4method{noise}{Image}(x, type="G", ...)
}

\arguments{

  \item{x}{ An object of \code{\linkS4class{Image}}. }

  \item{r}{ A numeric value for the radius of the pixel neighbourhood.
    Passing 0 enables automatic radius selection, default. }

  \item{s}{ A numeric value for the standard deviation of the Laplacian
    (\code{sharpen}) or Gaussian (\code{umask, blur, gblur}), in pixels. For
    reasonable results, in most functions \code{r} must be larger than \code{s}. }

  \item{amount}{ A numeric value for the percentage difference between
    the original and the blurred image that is added back into the original in
    the un-sharp mask algorithm. }

  \item{t}{ A numeric value for the threshold in pixels needed to apply
    the \code{amount} in the un-sharp mask algorithm.}

  \item{type}{ The type of noise to add. Supported noise types are:
    \code{Uniform, Gaussian} (default), \code{Multi, Impulse,
    Laplace} and \code{Poisson}. The value can be specified by one letter.
    Case insensitive. }

  \item{...}{ Reserved. }
}

\value{
  A transformed image in an object of \code{\linkS4class{Image}}.
}

\details{

  \code{despeckle} reduces the speckle-type, single-pixel, noise.

  \code{mediansmooth} smooths the noisy image by replacing each pixel by
    a median of pixel values in taken over the neighbouring as defined by radius.

  \code{blur, gblur} produce a blurred image. The \code{blur} method differs from
    the Gaussian blur, \code{gblur}, in that it uses a separable kernel which is
    faster but mathematically equivalent to the non-separable kernel.

  \code{sharpen, umask} sharpen an image. \code{umask} uses the un-sharp mask
    algorithm, in which the image is convolved with a Gaussian operator of the given
    radius and standard deviation, \code{s}.

}

\seealso{ \code{
    \linkS4class{Image}, \link{normalize}
}}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2005-2007
}

\examples{
  w <- 120
  a <- Image((0:(w^2))/w^2, c(w,w))
  if ( interactive() ) display(a)
  b <- normalize(noise(a) * 0.1)
  if ( interactive() ) display(b)
  dn <- despeckle(b)
  if ( interactive() ) display(dn)
  bl <- blur(dn, 4, 2)
  if ( interactive() ) display(bl)
}

\keyword{file}

