\name{Image3D}

\docType{class}

\alias{Image3D}
\alias{Image3D.CopyHeader}

\concept{image representation}
\concept{image analysis}
\concept{image processing}

\title{
    Class To Work With 3D Images (Image Stacks) in R
}

\description{
    A class to work with 3D images or image stacks in R. This class is directly derived from
    \code{\link{Image2D}} class in this same package. It therefore has all capabilities of \code{\link{Image2D}}
    class extended to an additional dimension. Whereas data in Image2D is a 2D \code{\link{matrix}}, data
    in \code{Image3D} are 3D arrays. Filter functions for this class assume that 3D image is a stack of
    2D images, thus running the same filter on all 2D images (defined by first two dimensions) concequently.
}

\section{Creating Objects}{
    \describe{
        \item{\code{Image3D(data, dim, rgb)}:}{Default constructor. Executed with no parameters,
            an empty (black) 2D grayscale image stack of 2x2x2 pixels is created. If \code{data} is an
            array of 2x2x2, \code{dim} can be omitted.}
    }
}

\section{Slots}{
    \describe{
        \item{\code{.Data}:}{Matrix of image data - inherited from \code{array}. Will be changed later to
            an access function.}
        \item{\code{rgb}:}{A logical value that specifies if the image is RGB or grayscale}
        \item{\code{...}:}{Further slots for image metrics, resolution, compression and compression
            quality: not implemented yet - will follow shortly.}
    }
}

\section{Methods}{
    All methods inherited from classes \code{array} and \code{\link{Image2D}}. Most methods are simply redefined
    in a way to operate on stacks of images rather than on simple images here. No additional methods defined in
    \code{Image3D} so far.
}

\details{
    See details section for \code{\link{Image2D}}.
}

\seealso{
   \code{\link{Image2D}}, \code{\link{Image IO}}, \code{\link{Filters 2D}}.
}

\source{
   R-sources are available from 'R/class.Image3D.R' with c++ sources in 'inst/src/conversions.cpp',
   'inst/src/display.cpp' and 'inst/src/io.cpp' of the package 'EBImage'.
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}

\examples{
    # more to follow
    \dontrun{im = read.image("test.tif")}
%    \dontshow{exp(im)}
    \dontrun{display(im)}
}

\keyword{methods}
\keyword{classes}
