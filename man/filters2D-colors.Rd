\name{Color 2D Filters}
\alias{Color 2D Filters}

\alias{contrast}
\alias{equalize}
\alias{colorGamma}
\alias{mod}
\alias{shade}
\alias{solarize}

\concept{image color adjustment}
\concept{image processing}

\title{
    Filters to Adjust Colors of 2D Images and Image Stacks (3D Images)
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
    \item{x}{An instance of class \code{\link{Image2D}} or \code{\link{Image3D}}. For 3D objects, filters
        will be applied to every single image composed of first two dimensions.}
    \item{sharpen}{A measure of intensity differences for \code{contrast}.}
    \item{level}{Uniform gamma level.}
    \item{brightness}{Ratio of the current brightness value to adjust, 1.0 to keep the value.}
    \item{saturation}{Ratio of the current saturation value to adjust, 1.0 to keep the value.}
    \item{hue}{An absolute rotation of -180 to +180 degrees from the current position corresponding
        to an argument range of 0 to 2.0, 1.0 for no change.}
    \item{azimuth}{Together with \code{elevation} defines the position of the light source for
        \code{shade}.}
    \item{elevation}{Together with \code{azimuth} defines the position of the light source for
        \code{shade}.}
    \item{shading}{Specifies if color components (red, green and blue) should be shaded.}
    \item{factor}{Magnitude of solarization.}
}

\value{
    New instance of class \code{\link{Image2D}} or \code{\link{Image3D}} with the same characteristics
    as the original, but with filter applied (to every single 2D image if source is 3D).
}

\details{
    \code{contrast} adjusts the image conrtast by enhancing intensity differences in the image.

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
   \code{\link{Image2D}}, \code{\link{Image3D}}, \code{\link{Filters 2D}}
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

