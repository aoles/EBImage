\name{resize}

\alias{flip}
\alias{flop}
\alias{resample}
\alias{resize}
\alias{rotate}
\alias{affinet}
\alias{translate}

\concept{image transformation}
\concept{rotation}
\concept{affine transformation}

\title{ Image transformation: rotation, resize, etc. }

\description{
  Functions to rotate, mirror and resize images.
}

\usage{

  affinet(x, sx=0, rx=0, ry=0, sy=0, tx=0, ty=0)
  flip(x)
  flop(x)
  resample(x, w, h)
  resize(x, w, h, blur=1, filter="Lanczos")
  rotate(x, angle=90, col)

}

\arguments{

  \item{x}{An object of \code{\linkS4class{Image}}. }

  \item{sx, rx, ry, sy, tx, ty}{Elements of the affine matrix. }

  \item{w, h}{ Width and height of a new resized/resampled image.
    One of these arguments can be missing to enable proportional resize. }

  \item{blur}{ The blur factor, where 1 (\code{TRUE}) is blurry,
    0 (\code{FALSE}) is sharp. }

  \item{filter}{ Resize pixel sampling filter. }

  \item{angle}{ Image rotation angle in degrees. }

  \item{col}{ A numeric, integer or character specifying the background
    color of the rotated image. Not implemented yet, defaults to black. }
}

\value{

  A transformed image in an object of \code{\linkS4class{Image}}.

}

\details{

  \code{affinet} transforms an image as dictated by the affine matrix.

  \code{flip} creates a vertical mirror image by reflecting the pixels
    around the central x-axis.

  \code{flop} creates a horizontal mirror image by reflecting the pixels
    around the central y-axis.

  \code{resample} scales an image to the desired dimensions with pixel
    sampling. Unlike other scaling methods, this method does not introduce
    any additional color into the scaled image.

  \code{resize} scales an image to the desired dimensions using the supplied
    filter algorithm. Available filters are: \code{Point, Box, Triangle, Hermite,
    Hanning, Hamming, Blackman, Gaussian, Quadratic, Cubic, Catrom, Mitchell,
    Lanczos, Bessel, Sinc}. Most of the filters are FIR (finite impulse response),
    however, Bessel, Gaussian, and Sinc are IIR (infinite impulse response).
    Bessel and Sinc are windowed (brought down to zero) with the Blackman filter.

  \code{rotate} creates a new image that is a rotated copy of an existing one.
    Positive angles rotate counter-clockwise (right-hand rule), while negative
    angles rotate clockwise. Rotated images are usually larger than the
    originals and have 'empty' triangular corners. X axis. Empty triangles left
    over from shearing the image are filled with the background color.

}

\seealso{ \code{
    \linkS4class{Image}
}}

\references{
  \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2006-2007
}

