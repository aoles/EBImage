\name{Colors}
\alias{Colors}

\alias{contrast}
\alias{equalize}
\alias{colorGamma}
\alias{mod}
\alias{shade}
\alias{solarize}

\concept{image color adjustment}
\concept{image processing}

\title{
    Filters to adjust colors of images
}

\description{
    A collection of basic filters to adjust image colors or apply some artistic effects.
    Operate on both grayscale and RGB images.
}

\usage{
    contrast(x, sharpen)
    equalize(x)
    colorGamma(x, level)
    mod(x, brightness = 1, saturation = 1, hue = 1)
    shade(x, azimuth = 30, elevation = 30, shading = FALSE)
    solarize(x, factor = 50)
}

\arguments{
    \item{x}{An instance of class \code{\link{Image}}.}
    \item{sharpen}{Numeric scalar, a measure of intensity differences.}
    \item{level}{Numeric scalar, uiform gamma level.}
    \item{brightness}{Numeric scalar, ratio of the current brightness value to adjust,
        1.0 to keep the value.}
    \item{saturation}{Numeric scalar, ratio of the current saturation value to adjust,
        1.0 to keep the value.}
    \item{hue}{Numeric scalar, an absolute rotation of -180 to +180 degrees from the
        current position corresponding to an argument range of 0 to 2.0, 1.0 for no change.}
    \item{azimuth}{Numeric scalar, together with \code{elevation} defines the position
        of the light source for \code{shade}.}
    \item{elevation}{Numeric scalar, together with \code{azimuth} defines the position
        of the light source for \code{shade}.}
    \item{shading}{Specifies if color components (red, green and blue) should be shaded.}
    \item{factor}{Magnitude of solarization.}
}

\value{
    A new instance of class \code{\link{Image}} with the same dimensions as \code{x},
    but with filter(s) applied.
}

\details{
    \code{contrast} adjusts the image contrast by enhancing intensity differences in the image.

    \code{equalize} performs a histogram equalization of the image.

    \code{colorGamma} adjusts the overall image gamma value.

    \code{mod} modulates percents of brightness, saturation and hue of the image.

    \code{shade} shades the image using a distant light source. If \code{shading} argument is
    set to FALSE, the result is converted to a grayscale internally (the return type however
    preserves the mod, although all channels will have the same values).

    \code{solarize} solarizes the images similarly to exposing a photographic film to light
    during the development process.
}

\seealso{
   \code{\link{Image}}, \code{\link{Filters 2D}}
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}

\examples{
}

\keyword{dplot}
\keyword{manip}
\keyword{array}

