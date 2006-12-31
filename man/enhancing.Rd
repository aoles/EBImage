\name{enhance}

\alias{cgamma}
\alias{contrast}
\alias{enhance}
\alias{equalize}
\alias{modulate}

\concept{image color enhancement}

\title{ Enhancing image and image colors }

\description{
  Functions to enhance image colours.
}

\section{ Usage }{
  \preformatted{
    cgamma(x, level=1, ...)
    contrast(x, sharpen=TRUE, )
    enhance(x, ...)
    equalize(x, ...)
    modulate(x, value=100, ...)
  }
}

\section{ Arguments }{ \describe{

  \item{\code{x}}{An object of \code{\link{Image-class}}. }

  \item{\code{sharpen}}{A logical specifying whether the contrast should
    be increased (\code{TRUE}) or decreased (\code{FALSE}). }

  \item{\code{value}}{ A percent change in brightness, saturation, and hue. 
    The default value to keep the values unchanged. } 

  \item{\code{...}}{ Reserved for future use. }
}}

\section{ Value }{

  A transformed image in an object of \code{\link{Image-class}}.
    
}

\section{ Details }{

  \code{cgamma} gamma-corrects image. The same image viewed on different 
    devices will have perceptual differences in the way the image's intensities 
    are represented on the screen. Adjust all three channels with the level 
    parameter. Values typically range from 0.8 to 2.3.
  
  \code{contrast} enhances the intensity differences between the lighter and 
    darker elements of the image.
    
  \code{enhance} applies a digital filter that improves the quality of a 
    noisy image.
  
  \code{equalize} applies a histogram equalization to the image.
    
  \code{modulate} lets you control the brightness, saturation, and hue of 
    an image. Modulate represents the brightness, saturation, and hue as one 
    parameter.
    
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

