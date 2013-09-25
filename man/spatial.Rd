\name{resize}

\alias{flip}
\alias{flop}
\alias{resize}
\alias{rotate}
\alias{translate}
\alias{affine}

\concept{transformation}
\concept{rotation}
\concept{resize}
\concept{mirror}

\title{Spatial linear transformations}

\description{
  The following functions perform all spatial linear transforms: reflection, rotation,
  translation, resizing, and general affine transform.
}

\usage{
flip(x)
flop(x)
rotate(x, angle, filter="bilinear", output.dim, output.origin=c(0, 0))
translate(x, v, filter="none", output.dim)
resize(x, w, h, filter="bilinear", output.dim, output.origin=c(0, 0))
affine(x, m, filter=c("bilinear", "none"), output.dim)
}

\arguments{
  \item{x}{An \code{Image} object or an array.}
  \item{angle}{A numeric specifying the image rotation angle in degrees.}
  \item{filter}{A character string indicating the interpolating sampling filter.
                Valid values are 'none' or 'bilinear'. See Details.}
  \item{output.dim}{A vector of 2 numbers indicating the dimension of the output image. 
                    Default is \code{dim(x)} in all transforms except in \code{resize}, where 
                    it is \code{c(w, h)}.}
  \item{output.origin}{A vector of 2 numbers indicating the output coordinates of the origin in pixels. 
                       Default is \code{c(0, 0)}. }
  \item{v}{A vector of 2 numbers denoting the translation vector in pixels.}
  \item{w, h}{Width and height of the resized image. One of these arguments
    can be missing to enable proportional resizing. }
  \item{m}{A 3x2 matrix describing the affine transformation. See Details.}
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

  \code{rotate} rotates the image counter-clockwise with the specified
  angle, centered on the input image center. Rotation center is changed by modifying 
  the argument \code{output.origin}.
 
  \code{resize} resizes the image \code{x} to desired dimensions.
  Resizing center is changed by modifying the argument \code{output.origin}.
  Zooming, without changing the output dimension, is achieved by setting
  the arguments \code{w} and \code{h} to values different from \code{output.dim}.

  \code{affine} returns the affine transformation of \code{x}, where
  pixels coordinates, denoted by the matrix \code{px}, are
  transformed to \code{cbind(px, 1)\%*\%m}. 

  All spatial transforms excepted \code{flip} and \code{flop} are based on the 
  general \code{affine} transformation. Spatial interpolation could be of two type:
  \code{none}, also called nearest neighbor, where interpolated pixel value is
  computed by taking the closest pixel, or \code{bilinear}, where interpolated 
  pixel is computed by bilinear approximation of the 4 neighboring pixels. The
  \code{bilinear} filter gives the smoother results.
}

\seealso{ 
  \code{\link{transpose}}
}

\author{
  Gregoire Pau, 2012
}

\examples{
  x <- readImage(system.file("images", "lena.png", package="EBImage"))
  if (interactive()) display(x)

  y <- flip(x)
  if (interactive()) display(y, title='flip(x)')

  y = flop(x) 
  if (interactive()) display(y, title='flop(x)')

  y <- resize(x, 128) 
  if (interactive()) display(y, title='resize(x, 128)')

  y <- rotate(x, 30) 
  if (interactive()) display(y, title='rotate(x, 30)')

  y <- translate(x, c(120, -20)) 
  if (interactive()) display(y, title='translate(x, c(120, -20))')

  m <- matrix(c(0.6, 0.2, 0, -0.2, 0.3, 300), nrow=3)
  if (interactive()) display(affine(x, m), title='affine transform')
}
