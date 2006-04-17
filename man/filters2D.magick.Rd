\name{Filters 2D}
\alias{Filters 2D}

\docType{class}

\title{
    Summary of Filters for 2D Images and 3D Image Stacks
}

\description{
Image filters operate on 2D images or on image stacks (\code{Image3D}).
Many of them are from the ImageMagick library.
When applied to a 3D image stack, filters are run separately and
independently on each 2D image.
}

\details{
 The functions accept an object of class \code{\link{Image2D}} or \code{\link{Image3D}} plus
 further filter-specific arguments.
}

\seealso{
    \code{\link{Image2D}}, \code{\link{Image3D}}
% FIXME:
% why so much redundancy in the documentation - this will be tedious to
% maintain when things evolve. Here just link to the "segment", "enhance"
% etc. and provide further details (e.g. function names) there.
    ImageMagick-based filters:
    \describe{
        \item{\code{\link{Enhancing 2D Filters}}:}{\code{noise}, \code{blur},
            \code{despeckle}, \code{enhance}, \code{gaussFilter},
            \code{medianFilter}, \code{reduceNoise}, \code{sharpen},
            \code{spread}, \code{unsharpMask}}
        \item{\code{\link{Segmentation 2D Filters}}:}{\code{adaptThresh}, \code{edge},
            \code{segment}, \code{ffill}, \code{ffillEdge}}
        \item{\code{\link{Color 2D Filters}}:}{\code{contrast}, \code{equalize},
            \code{colorGamma}, \code{mod}, \code{shade},
            \code{solarize}}
        \item{\code{\link{Transform 2D Filters}}:}{\code{rotate}, \code{sampleImage},
            \code{scaleImage}}
    }
}

\references{
  \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}

\examples{
    # FIXME: load image, increase brightness, save image
% FIXME: this does not really look like an example for filters
    \dontrun{im = read.image("test.tif")}
    \dontrun{im = im + 1000}
    \dontrun{write.image(im, "test1.tif")}
}

\keyword{dplot}
\keyword{manip}
\keyword{array}

