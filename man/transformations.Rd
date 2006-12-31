\name{resize}

\alias{flip}
\alias{flop}
\alias{resample}
\alias{resize}
\alias{rotate}
\alias{affinet}

\concept{image transformation}
\concept{rotation}
\concept{affine transformation}

\title{ Image transformation: rotation, resize, etc. }

\description{
  Functions to rotate, mirror and resize images.
}

\section{ Usage }{
  \preformatted{
    affinet(x, sx=0, rx=0, ry=0, sy=0, tx=0, ty=0, ...)
    flip(x, ...)
    flop(x, ...)
    resample(x, w, h, ...)
    resize(x, w, h, blur=1, ...)
    rotate(x, angle=90, ...)
  }
}

\section{ Arguments }{ \describe{

  \item{\code{x}}{An object of \code{\link{Image-class}}. }

  \item{\code{sx, rx, ry, sy, tx, ty}}{Elements of the affine matrix. }

  \item{\code{w, h}}{ Width and height of a new resized/resampled image.
    One of these arguments can be missing to enable proportional resize. } 

  \item{\code{blur}}{ The blur factor, where 1 (\code{TRUE}) is blurry, 
    0 (\code{FALSE}) is sharp. } 

  \item{\code{angle}}{ Image rotation angle in degrees. } 

  \item{\code{...}}{ Reserved for future use. }
}}

\section{ Value }{

  A transformed image in an object of \code{\link{Image-class}}.
    
}

\section{ Details }{

  \code{affinet} transforms an image as dictated by the affine matrix. 

  \code{flip} creates a vertical mirror image by reflecting the pixels 
    around the central x-axis.
    
  \code{flop} creates a horizontal mirror image by reflecting the pixels 
    around the central y-axis.
  
  \code{resample} scales an image to the desired dimensions with pixel 
    sampling. Unlike other scaling methods, this method does not introduce 
    any additional color into the scaled image.
    
  \code{resize} scales an image to the desired dimensions with one of the 
    filters that can be set using the \code{\link{Image-class}} accessor
    method \code{\link{sampleFilter}}. Most of the filters are FIR (finite 
    impulse response), however, Bessel, Gaussian, and Sinc are IIR (infinite 
    impulse response). Bessel and Sinc are windowed (brought down to zero) with 
    the Blackman filter.
    
  \code{rotate} creates a new image that is a rotated copy of an existing one. 
    Positive angles rotate counter-clockwise (right-hand rule), while negative 
    angles rotate clockwise. Rotated images are usually larger than the 
    originals and have 'empty' triangular corners. X axis. Empty triangles left 
    over from shearing the image are filled with the background color.
    
}

\seealso{
  \code{
    \link{Image-class}, \link{sampleFilter}
  }
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Copyright (c) 2005-2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\keyword{file}

