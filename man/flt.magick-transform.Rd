\name{Transformation}
\alias{Transformation}

\alias{rotate}
\alias{sample.image}
\alias{zoom.image}
\alias{trim}
\alias{tile}

\concept{image transformation}
\concept{image processing}

\title{
    Filters to resize and rotate images
}

\description{
    Filters to resize and rotate images.
}

\usage{
    rotate(x, degrees = 90)
    sample.image(x, dx, dy)
    zoom.image(x, dx, dy)
    trim(x, bg = 0)
    tile(x, width, ...)
}

\arguments{
    \item{x}{An instance of class \code{\link{Image}}.}
    \item{degrees}{Angle in degrees for counter-clockwise rotation of the image.}
    \item{dx}{New image width to fit in.}
    \item{dy}{New image height to fit in.}
    \item{bg}{Background color shade to trim.}
    \item{width}{An integer value for the number of image in a row of a tiled image.}
    \item{...}{Further arguments for \code{tile} -- \code{fg} for foreground colour (default 
    "yellow") and \code{bg} for background (default "black"). Colours can be specified
    as gray levels in the range [0,1] or as integer representations of RGB (like in
    RGB images) or as X11 string, e.g. "red" or "\#5522ee".}
}

\value{
    A new instance of class \code{\link{Image}} and filter applied.
}

\details{
    \code{rotate} rotates images counter-clockwise.

    \code{sample.image} uses sampling algorithm to proportionally scale the image
    
    Functions \code{zoom.image} and \code{trim} have not been tested yet - they are
    1:1 wrappers around corresponding Magick++ functions, please refer to ImageMagick
    documentation pages for further help.
    
    \code{tile} takes an image stack and arranges all images of this stack on a single 
    2D image in a tiled manner with \code{width} number of images in a row. The number
    of images in columns is automatically evaluated from \code{width} and the total
    number of images. 
}

\seealso{
   \code{\link{Image}}, \code{\link{Filters 2D}}
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Copyright (c) 2005-2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\examples{
    # create an empty image of 300x200 size
    im1 <- Image(0, c(300,200,1))
    # fill image with data (waves)
    for (i in 1:dim(im1)[[1]])
        for (j in 1:dim(im1)[[2]]) {
            im1[i,j,1] <- abs(sin(i*0.0314)*sin(i*0.0314) + cos(j*0.0314)*cos(j*0.0314))
        }
    # normalize image
    im1 <- normalize(im1)
    # segment and edge
    ed <- edge(segment(im1, 100, 0.1), 1)
    # rotate edged image
    \dontrun{display(rotate(ed, 90))}
    # sample image
    \dontrun{display(sample.image(ed, 400, 300))}
}

\keyword{dplot}
\keyword{manip}
\keyword{array}

