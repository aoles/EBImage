\name{morphology}

\alias{dilate}
\alias{closing}
\alias{erode}
\alias{opening}
\alias{makeBrush}
\alias{dilateGreyScale}
\alias{erodeGreyScale}
\alias{openingGreyScale}
\alias{closingGreyScale}
\alias{whiteTopHatGreyScale}
\alias{blackTopHatGreyScale}
\alias{selfcomplementaryTopHatGreyScale}


\title{Perform morphological operations on images}

\description{
  Functions to perform morphological operations on binary images.
}

\usage{
  dilate(x, kern)
  erode(x, kern)
  opening(x, kern)
  closing(x, kern)
  dilateGreyScale(x, kern)
  erodeGreyScale(x, kern)
  openingGreyScale(x, kern)
  closingGreyScale(x, kern)
  whiteTopHatGreyScale(x, kern)
  blackTopHatGreyScale(x, kern)
  selfcomplementaryTopHatGreyScale(x, kern)

  makeBrush(size, shape=c('box', 'disc', 'diamond', 'gaussian', 'line'), step=TRUE, sigma=0.3, angle=45)
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
  
  \item{size}{A numeric containing the size of the brush in pixels. This should be an odd number; even numbers are rounded to the next odd one, i.e., \code{size = 4} has the same effect as  \code{size = 5}. If shape is \code{line}, \code{size} represents the length of the line.}
  
  \item{shape}{A character vector indicating the shape of the brush. Can
    be \code{box}, \code{disc}, \code{diamond}, \code{gaussian} or \code{line}. Default is
    \code{box}.}
  
  \item{step}{a logical indicating if the brush is binary. Default is
    \code{TRUE}. The argument is relevant only for the \code{disc} and
    \code{diamond} shapes.}

  \item{sigma}{An optional numeric containing the standard deviation of
    the Gaussian shape. Default is 0.3.}

  \item{angle}{An optional numeric containing the angle at which the line should be drawn. The angle is
    one between the top of the image and the line.}

}

\value{
  \code{dilate}, \code{erode}, \code{opening}, \code{closing}, \code{dilateGreyScale},
  \code{erodeGreyScale}, \code{openingGreyScale}, \code{closingGreyScale},
  \code{whiteTopHatGreyScale}, \code{blackTopHatGreyScale} and
  \code{selfcomplementaryTopHatGreyScale} return the transformed \code{Image} object
  or array,  after the corresponding morphological operation.
    
  \code{makeBrush} generates a 2D matrix containing the desired brush.
}

\details{
  \code{dilate} applies the mask positioning its centre over every background pixel
  (0), every pixel which is not covered by the mask is reset to foreground (1).
  
  \code{erode} applies the mask positioning its centre over every foreground pixel
  (!=0), every pixel which is not covered by the mask is reset to background (0).

  \code{opening} is an erosion followed by a dilation and \code{closing} is a dilation
  followed by an erosion. The same goes for the greyscale versions.

  \code{dilateGreyScale} applies the mask positioning its centre over every pixel of the
  \code{Image}, the output value of the pixel is the maximum value of the \code{Image}
  covered by the mask.

  \code{erodeGreyScale} applies the mask positioning its centre over every pixel of the
  \code{Image}, the output value of the pixel is the minimum value of the \code{Image}
  covered by the mask.

  \code{whiteTopHatGreyScale} substracts the opening of the \code{Image} from the \code{Image}

  \code{blackTopHatGreyScale} substracts the \code{Image} from the closing of the \code{Image}

  \code{selfcomplementaryTopHatGreyScale} is the sum of a white top-hat and a black top-hat, simplified
  the difference between closing and opening of the \code{Image}

  \code{makeBrush} generates brushes of various sizes and shapes that can be used
  as structuring elements.

  Operations on greyscale images use an implementation of the Urbach-Wilkinson algorithm[1] and can
  only handle flat (i.e. binary) brushes.
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2006
  Ilia Kats, \email{ilia-kats@gmx.net}, 2012

}

\examples{	
    x = readImage(system.file("images", "shapes.png", package="EBImage"))
    if (interactive()) display(x)
    kern = makeBrush(5, shape='diamond')
    if (interactive()) display(kern, title='Structuring element')
    if (interactive()) display(erode(x, kern), title='Erosion of x')
    if (interactive()) display(dilate(x, kern), title='Dilatation of x')

    ## makeBrush 
    x = makeBrush(99, shape='diamond')
    if (interactive()) display(x, title="makeBrush(99, shape='diamond')")
    x = makeBrush(99, shape='disc', step=FALSE)
    if (interactive()) display(x, title="makeBrush(99, shape='disc', step=FALSE)")
    x = makeBrush(99, shape='gaussian', sigma=10)
    if (interactive()) display(2000*x, title="makeBrush(99, shape='gaussian', sigma=10)")
}

\references{
    [1] E. R. Urbach and M.H.F. Wilkinson, "Efficient 2-D grayscale morphological transformations with arbitrary flat structuring elements", IEEE Trans Image Process 17(1), 1-8, 2008
}

