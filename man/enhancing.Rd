\name{enhance}

\alias{cgamma}
\alias{contrast}
\alias{enhance}
\alias{equalize}
\alias{modulate}

\concept{image color enhancement}

\title{ Enhancing images and colors }

\description{
  Functions to enhance and modify colors in images.
}

\usage{
  cgamma(x, level=1)
  contrast(x, sharpen=TRUE)
  enhance(x)
  equalize(x)
  modulate(x, value=100)

}

\arguments{

  \item{x}{An object of \code{\linkS4class{Image}}. }

  \item{level}{ A numeric for the gamma level. }

  \item{sharpen}{A logical specifying whether the contrast should
    be increased (\code{TRUE}) or decreased (\code{FALSE}). }

  \item{value}{ A percent change in brightness, saturation, and hue. 
    The default value to keep the values unchanged. } 
}

\value{

  A transformed image in an object of \code{\linkS4class{Image}}.
    
}

\details{

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
    \linkS4class{Image}
  }
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2006-2007
}

\keyword{manip}

