\name{Filters 2D}
\alias{Filters 2D}

\docType{class}

\title{
    Summary of Filters for 2D Images and 3D Image Stacks
}

\description{
    So far \code{EBImage} features 24 image filters available from ImageMagick that
    operate on 2D images or on image stacks (\code{Image3D}). It is important to
    understand that for these filters every single 2D image in a 3D stack is considered
    as independent, therefore if for example \code{\link{blur}} is applied, it
    is sequentially applied to every 2D image in a stack without blurring in the third
    dimension.
}

\details{
    All functions accept an object of class \code{\link{Image2D}} or \code{\link{Image3D}} plus
    arguments specific for that filter.
}

\seealso{
    \code{\link{Image2D}}, \code{\link{Image3D}}

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

\source{
    R sources in 'R/filters.standard2D.R' and C++ sources in 'inst/src/stdFilters.cpp' of package 'EBImage'.
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}

\examples{
    # FIXME: load image, increase brightness, save image
    \dontrun{im = read.image("test.tif")}
    \dontrun{im = im + 1000}
    \dontrun{write.image(im, "test1.tif")}
}

\keyword{dplot}
\keyword{manip}
\keyword{array}

