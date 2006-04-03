\name{thresh}

\alias{thresh}

\docType{function}

\title{Internal Adeptive Threshold Algorithm}
\description{
    \code{thresh} is the \code{EBImage} implementation of adaptive threshold. It uses
    a moving frame of \code{width} and \code{height} given as arguments to the function
    to estimate the intensity mean values and calculate the threshold. The function is
    implemented to substitute the \code{ImageMagick} \code{\link{adaptThresh}} function
    and prevent converting data from \code{R} to \code{ImageMagick} format.
}

\usage{
    thresh(x, width = 20, height = 20, offset = 0.05, preprocess = FALSE, modify = FALSE)
}
\arguments{
    \item{x}{An instance of class \code{\link{Image2D}} or \code{\link{Image3D}}. For 3D objects,
        the algorithm will be applied to every single image composed of first two dimensions.}
    \item{width}{x-pixel region to evaluate the mean.}
    \item{height}{y-pixel region to evaluate the mean.}
    \item{offset}{Threshold offset from the mean.}
    \item{preprocess}{If \code{TRUE} \code{gaussFilter} with arguments 4, 2 is applied to the normalized image first.}
    \item{modify}{If \code{TRUE}, original image will be modified without copying the data!}
}

\value{
    If \code{modify==FALSE} a new instance of class \code{\link{Image2D}} or \code{\link{Image3D}}
    with the same characteristics as the original, but with filter applied.

    If \code{modify==TRUE} invisible original image, modified by the function.
}

\seealso{
   \code{\link{Image2D}}, \code{\link{Image3D}}, \code{\link{Filters 2D}, \code{\link{objectCount}}, \code{\link{adaptThresh}}, \code{\link{distMap}}}
}

\source{
    \code{./src/adapt_thresh.cpp}
}

\author{
    Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}

\examples{
    im = read.image("http://www.ebi.ac.uk/~osklyar/projects/EBImage/examples/example.tif")
    im1 = thresh(im)
    dm = distMap(im1)
}

\keyword{dplot}
\keyword{manip}
\keyword{array}

