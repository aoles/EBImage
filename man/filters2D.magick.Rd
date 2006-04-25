\name{Filters 2D}
\alias{Filters 2D}

\docType{class}

\title{
    Summary of image filters
}

\description{
    Some filters are ported from ImageMagick library. Applied to a image stacks, filters are executed
    sequentially and independently for each 2D image.
}

\details{
    The functions accept objects of class \code{\link{Image}} plus filter-specific arguments.
% FIXME:
% why so much redundancy in the documentation - this will be tedious to
% maintain when things evolve. Here just link to the "segment", "enhance"
% etc. and provide further details (e.g. function names) there.
    ImageMagick-based filters:
    \describe{
        \item{\code{\link{Enhancement}}:}{\code{noise}, \code{blur},
            \code{despeckle}, \code{enhance}, \code{gaussFilter},
            \code{medianFilter}, \code{reduceNoise}, \code{sharpen},
            \code{spread}, \code{unsharpMask}}
        \item{\code{\link{Segmentation}}:}{\code{\link{thresh}}, \code{edge},
            \code{segment}, \code{\link{distMap}}}
%            , \code{ffill}, \code{ffillEdge}}
        \item{\code{\link{Colors}}:}{\code{\link{normalize}}, \code{contrast}, \code{equalize},
            \code{colorGamma}, \code{mod}, \code{shade}, \code{solarize}}
        \item{\code{\link{Transformation}}:}{\code{rotate}, \code{sampleImage},
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
    %# FIXME: load image, increase brightness, save image
% FIXME: this does not really look like an example for filters
    %\dontrun{im = read.image("test.tif")}
    %\dontrun{im = im + 1000}
    %\dontrun{write.image(im, "test1.tif")}
}

\keyword{dplot}
\keyword{manip}
\keyword{array}

