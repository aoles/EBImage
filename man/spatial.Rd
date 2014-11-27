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
rotate(x, angle, filter = "bilinear", output.origin = c(0, 0), ...)
translate(x, v, filter = "none", ...)
resize(x, w, h, filter = "bilinear", output.dim = c(w, h), output.origin = c(0, 0), ...)

affine(x, m, filter = c("bilinear", "none"), output.dim, bg.col = "black")
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
                    Default is \code{dim(x)} in all transforms except in \code{resize}, where 
                    it is \code{c(w, h)}.}
  \item{output.origin}{A vector of 2 numbers indicating the output coordinates of the origin in pixels. 
                       Default is \code{c(0, 0)}. }
  
  \item{m}{A 3x2 matrix describing the affine transformation. See Details.}
  \item{bg.col}{Color used to fill the background pixels. The default is "black".}
  \item{...}{Arguments to be passed to the \code{affine} function, such as \code{output.dim} or \code{bg.col}}  
}
 
\value{
  An \code{Image} object or an array, containing the transformed version
  of \code{x}.
}

\details{
  \code{flip} mirrors \code{x} across the central horizontal (x-)axis.
  
  \code{flop} mirrors \code{x} across the central vertical (y-)axis.

  \code{rotate} rotates the image clockwise by the specified
  angle around the origin. The rotation origin defaults to the center of the input image and can by changed by modifying 
  the argument \code{output.origin}.
 
  \code{resize} resizes the image \code{x} to desired dimensions.
  Resizing center is changed by modifying the argument \code{output.origin}.
  Zooming, without changing the output dimension, is achieved by setting
  the arguments \code{w} and \code{h} to values different from \code{output.dim}.

  \code{affine} returns the affine transformation of \code{x}, where
  pixels coordinates, denoted by the matrix \code{px}, are
  transformed to \code{cbind(px, 1)\%*\%m}. 

  All spatial transformations except \code{flip} and \code{flop} are based on the 
  general \code{affine} transformation. Spatial interpolation can be one of the following types:
  \code{none}, also called nearest neighbor, where the interpolated pixel value equals to
  the closest pixel value, or \code{bilinear}, where the interpolated 
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
