\name{Filters 2D}
\alias{Filters 2D}

\docType{class}

\title{
    Summary of Filters for 2D Images and 3D Image Stacks
}

\description{
    So far 'imagine' features 24 image filters available from ImageMagick that
    operate on 2D images or sequences of 2D images (image stacks). It is important to
    understand that for these filters, every single 2D image in a 3D stack is considered
    as independent, therefore if for example \code{\link{im.blur}} is applied, it
    is sequentially applied to every 2D image in a stack without blurring in the third
    direction.
}

\details{
    All functions have three common arguments apart from filter parameters, \code{x} -
    object of class 'Image' (can be 2D or 3D only), \code{do.stack} - if the routine
    should process 3D stacks or should show error if such is supplied, and
    \code{warn.stacks} - if the routine should process stack with a warning that a stack
    was supplied.

    Description of other arguments can be found in particular help files, see links below.
}

\seealso{
    \code{\link{Image2D}}

    ImageMagick-based filters:
    \describe{
        \item{\code{link{Filters 2D Enhancing}}:}{\code{\link{im.addNoise}}, \code{\link{im.blur}},
            \code{\link{im.despeckle}}, \code{\link{im.enhance}}, \code{\link{im.gaussian}},
            \code{\link{im.median}}, \code{\link{im.redNoise}}, \code{\link{im.sharpen}},
            \code{\link{im.spread}}, \code{\link{im.unsharpmask}}}
        \item{\code{link{Filters 2D Segmentation}}:}{\code{\link{im.tresh}}, \code{\link{im.edge}},
            \code{\link{im.segment}}, \code{\link{im.fill}}, \code{\link{im.fillEdge}}}
        \item{\code{link{Filters 2D Colors}}:}{\code{\link{im.contrast}}, \code{\link{im.equalize}},
            \code{\link{im.gamma}}, \code{\link{im.mod}}, \code{\link{im.shade}},
            \code{\link{im.solarize}}}
        \item{\code{link{Filters 2D Transforms}}:}{\code{\link{im.rotate}}, \code{\link{im.sample}},
            \code{\link{im.scale}}}
    }
}

\source{
    R sources in 'R/filters.standard2D.R' and C++ sources in 'inst/src/stdFilters.cpp' of package 'imagine'.
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

