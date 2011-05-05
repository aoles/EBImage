\name{resize}

\alias{flip}
\alias{flop}
\alias{resize}
\alias{rotate}
\alias{affine}

\concept{transformation}
\concept{rotation}
\concept{resize}
\concept{mirror}

\title{Spatial linear transformations}

\description{
  Rotates, mirrors and resizes images. 
}

\usage{
  flip(x)
  flop(x)
  resize(x, w, h, blur=1, filter="Lanczos")
  rotate(x, angle=90)
  affine(x, m)
}

\arguments{
  \item{x}{An \code{Image} object or an array.}

  \item{w, h}{Width and height of a new image. One of these arguments
    can be missing to enable proportional resizing. }

  \item{blur}{The blur factor, where 1 (\code{TRUE}) is blurry,
    0 (\code{FALSE}) is sharp.}

  \item{filter}{Interpolating sampling filter.}
  \item{angle}{Image rotation angle in degrees.}
  \item{m}{The affine 3x2 transformation matrix.}
}

\value{
  An \code{Image} object or an array, containing the transformed version
  of \code{x}.
}

\details{
  \code{flip} transforms \code{x} in its vertical mirror image by
  reflecting the pixels around the central x-axis.
  
  \code{flop} transforms \code{x} in its horizontal mirror image by
  reflecting the pixels around the central y-axis.

  \code{resize} scales the image to the desired dimensions using the supplied
  interpolating filter. Available filters are: \code{Point}, \code{Box},
  \code{Triangle}, \code{Hermite}, \code{Hanning}, \code{Hamming},
  \code{Blackman}, \code{Gaussian}, \code{Quadratic}, \code{Cubic},
  \code{Catrom}, \code{Mitchell}, \code{Lanczos}, \code{Bessel} and
  \code{Sinc}. The filter \code{Box} performs a nearest-neighbor
  interpolation and is fast but introduces considerable aliasing. The
  filter \code{Triangle} performs a bilinear interpolation and is a
  good trade-off between speed adn aliasing. Cubic interpolation with
  the filter \code{Cubic} is also a good trade-off. High-quality and slower
  interpolation is achieved with the \code{Lanczos} filter. The
  algorithm used by this ImageMagick function is not well defined.
  
  \code{rotate} rotates the image counter-clockwise with the specified
  angle. Rotated images are usually larger than the originals and have
  empty triangular corners filled in black. The algorithm used by this
  ImageMagick function is not well defined.

  \code{affine} returns the affine transformation of the image, where
  pixels coordinates, denoted by the matrix \code{px}, are
  transformed to \code{cbind(px, 1)\%*\%m}.
}

\seealso{ \code{translate} 
}

\references{
  \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2006-2007
}

\examples{
   x = readImage(system.file("images", "lena.gif", package="EBImage"))
   if (interactive()) display(x)

   y = flip(x)
   if (interactive()) display(y, title='flip(x)')

   y = flop(x) 
   if (interactive()) display(y, title='flop(x)')

   y = resize(x, 128) 
   if (interactive()) display(y, title='resize(x, 128)')

   y = rotate(x, 30) 
   if (interactive()) display(y, title='rotate(x, 30)')

   m = matrix(c(0.6, 0.2, 0, -0.2, 0.3, 300), nrow=3)
   if (interactive()) display(affine(x, m), title='affine transform')
}
