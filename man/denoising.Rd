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

\title{ Noise removal, blurring and smoothing or images }

\description{
  This set of functions allows for the removal of noise, blurring and smoothing 
  of images. The functions operate of images in any image mode. The functions and
  the corresponding help descriptions are ported from \emph{ImageMagick}, see 
  the reference below.
}

\section{ Usage }{
\preformatted{
## S4 methods for signature x='Image'
}

Noise removal:
\preformatted{
denoise(x, r=0)
mediansmooth(x, r=2)
despeckle(x)
}

Sharpening images:
\preformatted{
sharpen(x, r=0, s=0.5)
umask(x, r=0, s=0.5, amount=5, t=2)
}

Blurring images:
\preformatted{
blur(x, r=0, s=0.5)
gblur(x, r=0, s=0.5)
}

Adding noise to images:
\preformatted{
noise(x, type="G")
}
}

\section{ Arguments }{ \describe{

  \item{\code{x}}{ An object of \code{\link{Image-class}}. }

  \item{\code{r}}{ A numeric value for the radius of the pixel neighbourhood. 
    Passing 0 enables automatic radius selection, default. }

  \item{\code{s}}{ A numeric value for the standard deviation of the Laplacian 
    (\code{sharpen}) or Gaussian (\code{umask, blur, gblur}), in pixels. For 
    reasonable results, in most functions \code{r} must be larger than \code{s}. } 

  \item{\code{amount}}{ A numeric value for the percentage difference between 
    the original and the blurred image that is added back into the original in
    the un-sharp mask algorithm. }

  \item{\code{t}}{ A numeric value for the threshold in pixels needed to apply 
    the \code{amount} in the un-sharp mask algorithm.}

  \item{\code{type}}{ The type of noise to add. Supported noise types are: 
    \code{'Uniform', 'Gaussian'} (default) \code{, 'Multi', 'Impulse', 
    'Laplace'} and \code{'Poisson'}. The value can be specified by one letter.
    Case insensitive. }

}}

\section{ Value }{

  A transformed image in an object of \code{\link{Image-class}}.
    
}

\section{ Details }{

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
    \link{Image-class}, \link{normalize}
}}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Oleg Sklyar: \email{osklyar@ebi.ac.uk}   
}

\examples{
w <- 120
a <- Image((0:(w^2))/w^2, c(w,w))
if ( interactive() ) display(a)
b <- normalize(a + noise(a) * 0.1)
if ( interactive() ) display(b)
dn <- despeckle(b)
if ( interactive() ) display(dn)
bl <- blur(dn, 4, 2)
if ( interactive() ) display(bl)
}

\keyword{file}

