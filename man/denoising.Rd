\name{denoise}

\alias{mediansmooth}
\alias{blur}
\alias{denoise}
\alias{despeckle}
\alias{gblur}
\alias{noise}
\alias{sharpen}
\alias{umask}

\concept{image color enhancement}

\title{ Enhancing image and image colors }

\description{
  Functions to enhance image colours.
}

\section{ Usage }{
  \preformatted{
    denoise(x, r=0, ...)
    mediansmooth(x, r=2, ...)
    despeckle(x, ...)

    sharpen(x, r=0, s=0.5, ...)
    umask(x, r=0, s=0.5, amount=5, t=2, ...)

    blur(x, r=0, s=0.5, ...)
    gblur(x, r=0, s=0.5, ...)

    noise(x, type="gaussian", ...)
  }
}

\section{ Arguments }{ \describe{

  \item{\code{x}}{ An object of \code{\link{Image-class}}. }

  \item{\code{r}}{ The radius of the pixel neighbourhood to take into account. 
    The 0 value enables automatic radius selection. }

  \item{\code{s}}{ The standard deviation of the Laplacian (\code{sharpen}) or
    Gaussian (\code{umask, blur, gblur}), in pixels. For reasonable results of
    most functions, \code{r} must be larger than \code{s}. } 

  \item{\code{amount}}{ The percentage of the difference between the original 
    and the blur image that is added back into the original. }

  \item{\code{t}}{ The threshold in pixels needed to apply the diffence amount.}

  \item{\code{type}}{ The type of noise to add. }

  \item{\code{...}}{ Reserved for future use. }
}}

\section{ Value }{

  A transformed image in an object of \code{\link{Image-class}}.
    
}

\section{ Details }{

  \code{despeckle} reduces the speckle noise.
  
  \code{mediansmooth} smoothes a noisy image by replacing each pixel value by 
    the median in a set of neighbouring pixels as defined by radius.
    
  \code{blur, gblur} blur an image. \code{blur} differs from Gaussian blur, 
    \code{gblur}, in that it uses a separable kernel which is faster but
    mathematically equivalent to the non-separable kernel (implemented in
    \code{ImageMagick}).
  
  \code{sharpen, umask} sharpen an image. \code{umask} uses the unsharp mask
    algorithm. The the image is convolved with a Gaussian operator of the given
    radius and standard deviation, \code{s}. 
    
  \code{noise} adds noise to an image. Supported noise types are: 
    \code{'uniform', 'gaussian'} (default) \code{, 'multi', 'impulse', 
    'laplace'} and \code{'poisson'}.
    
}

\seealso{
  \code{
    \link{Image-class}
  }
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Copyright (c) 2005-2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\keyword{file}

