\name{morphology}

\alias{dilate}
\alias{closing}
\alias{erode}
\alias{opening}
\alias{makeBrush}

\title{Perform morphological operations on images}

\description{
  Functions to perform morphological operations on binary images.
}

\usage{
  dilate(x, kern, iter)
  erode(x, kern, iter)
  opening(x, kern, iter)
  closing(x, kern, iter)

  makeBrush(size, shape=c('box', 'disc', 'diamond'), step=TRUE)
}

\arguments{
  \item{x}{An \code{Image} object or an array.
    \code{x} is considered as a binary image, whose pixels of value 0
    are considered as background ones and other pixels as
    foreground ones.}
  
  \item{kern}{An \code{Image} object or an array, containing the
    structuring element. \code{kern} is considered as a binary image, whose
    pixels of value 0 are considered as background ones and other pixels as
    foreground ones.}
  
  \item{size}{A numeric containing the size of the brush, in pixels.}
  \item{shape}{A character vector indicating the shape of the brush. Can
    be \code{box}, \code{disc} or \code{diamond}. Default is \code{box}.}
  \item{step}{a logical indicating if the brush is binary. Default is \code{TRUE}.}
  
  \item{iter}{Deprecated argument.}
}

\value{
  \code{dilate}, \code{erode}, \code{opening} and \code{closing} return the
  transformed \code{Image} object or array,  after the corresponding
  morphological operation.
    
  \code{makeBrush} generates a 2D matrix containing the desired brush.
}

\details{
  \code{dilate} applies the mask positioning its centre over every background pixel
  (0), every pixel which is not covered by the mask is reset to foreground (1).
  
  \code{erode} applies the mask positioning its centre over every foreground pixel
  (!=0), every pixel which is not covered by the mask is reset to background (0).

  \code{opening} is an erosion followed by a dilation and \code{closing} is a dilation
  followed by an erosion.

  \code{makeBrush} generates brushes of various sizes and shapes that can be used
  as structuring elements.
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2006
}

\examples{	
    x = readImage(system.file("images", "shapes.png", package="EBImage"))
    if (interactive()) display(x)
    kern = makeBrush(5, shape='diamond')
    if (interactive()) display(kern, title='Structuring element')
    if (interactive()) display(erode(x, kern), title='Erosion of x')
    if (interactive()) display(dilate(x, kern), title='Dilatation of x')

    ## makeBrush 
    x = makeBrush(100, shape='diamond')
    if (interactive()) display(x, title="makeBrush(100, shape='diamond')")
    x = makeBrush(100, shape='disc', step=FALSE)
    if (interactive()) display(x, title="makeBrush(100, shape='disc', step=FALSE)")
}


