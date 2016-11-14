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
rotate(x, angle, filter = "bilinear", output.dim, output.origin, ...)
translate(x, v, filter = "none", ...)
resize(x, w, h, filter = "bilinear", output.dim = c(w, h), output.origin = c(0, 0), antialias = FALSE, ...)

affine(x, m, filter = c("bilinear", "none"), output.dim, bg.col = "black", antialias = TRUE)
}

\arguments{
  \item{x}{An \code{Image} object or an array.}
  \item{angle}{A numeric specifying the image rotation angle in degrees.}
  \item{v}{A vector of 2 numbers denoting the translation vector in pixels.}
  \item{w, h}{Width and height of the resized image. One of these arguments
    can be missing to enable proportional resizing.}  
  \item{filter}{A character string indicating the interpolating sampling filter.
                Valid values are 'none' or 'bilinear'. See Details.}
  \item{output.dim}{A vector of 2 numbers indicating the dimension of the output image. 
                    For \code{affine} and \code{translate} the default is \code{dim(x)}, for \code{resize} it equals \code{c(w, h)}, and for \code{rotate} it defaults to the bounding box size of the rotated image.}
  \item{output.origin}{A vector of 2 numbers indicating the output coordinates of the origin in pixels.}
  
  \item{m}{A 3x2 matrix describing the affine transformation. See Details.}
  \item{bg.col}{Color used to fill the background pixels. The default is "black".}
  \item{antialias}{If \code{TRUE}, perform bilinear sampling at image edges using \code{bg.col}.}
  \item{...}{Arguments to be passed to \code{affine}, such as \code{filter}, \code{output.dim}, \code{bg.col} or \code{antialias}.}  
}
 
\value{
  An \code{Image} object or an array, containing the transformed version
  of \code{x}.
}

\details{
  \code{flip} mirrors \code{x} around the image horizontal axis (vertical reflection).
  
  \code{flop} mirrors \code{x} around the image vertical axis (horizontal reflection).

  \code{rotate} rotates the image clockwise by the given angle around the 
  origin specified in \code{output.origin}. If no \code{output.origin} is 
  provided, the result will be centered in a recalculated bounding box unless
  \code{output.dim} is provided.
 
  \code{resize} scales the image \code{x} to the desired dimensions.
  The transformation origin can be specified in \code{output.origin}. 
  For example, zooming about the \code{output.origin} can be achieved by setting
  \code{output.dim} to a value different from \code{c(w, h)}.

  \code{affine} returns the affine transformation of \code{x}, where
  pixels coordinates, denoted by the matrix \code{px}, are
  transformed to \code{cbind(px, 1)\%*\%m}. 

  All spatial transformations except \code{flip} and \code{flop} are based on the 
  general \code{affine} transformation. Spatial interpolation can be either
  \code{none}, also called nearest neighbor, where the resulting pixel value equals to
  the closest pixel value, or \code{bilinear}, where the new 
  pixel value is computed by bilinear approximation of the 4 neighboring pixels. The
  \code{bilinear} filter gives smoother results.
}

\seealso{ 
  \code{\link{transpose}}
}

\author{
  Gregoire Pau, 2012
}

\examples{
  x <- readImage(system.file("images", "sample.png", package="EBImage"))
  display(x)

  display( flip(x) )
  display( flop(x) ) 
  display( resize(x, 128) )
  display( rotate(x, 30) )
  display( translate(x, c(120, -20)) )

  m <- matrix(c(0.6, 0.2, 0, -0.2, 0.3, 300), nrow=3)
  display( affine(x, m) )
}
