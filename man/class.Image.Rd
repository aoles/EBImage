\name{Image}

\docType{class}

\alias{Image}
\alias{Image-class}
\alias{EBImage}
\alias{copy}
\alias{display}
\alias{channels}
\alias{toGray}
\alias{toRGB}
\alias{toRed}
\alias{toGreen}
\alias{toBlue}
\alias{getRed}
\alias{getGreen}
\alias{getBlue}
\alias{normalize}

\concept{image representation}
\concept{image analysis}
\concept{image processing}

\title{
    Class to work with images in R
}

\description{
    A class to work with images in R. This is the basic class to hold image
    data and parameters. The class is directly derived from \code{\link{array}},
    which enables efficient operations on image data (including subsetting,
    arithmetic etc).

    RGB images are coded in 8 bits per channel, 24 bits per pixel, stored in
    an R \code{integer} array. Grayscale images are coded \code{double} precision.
}

\section{Creating Objects}{
    \describe{
        \item{\code{Image(data, dim, rgb)}:}{Default constructor. If called with no parameters,
            a 3D grayscale image is created with 1x1x1 0-value pixels. If \code{data} is a
            two or three dimensional array, \code{dim} can be omitted.}
        \item{\code{copy(x)}:}{In most cases is essentially the same as assignment operator, i.e.
            image2 = image1. This method is for developers only who want to ensure that
            new objects allocate new memory block and do not point to the old ones.}
    }
}

\section{Slots}{
    \describe{
        \item{\code{.Data}:}{Matrix of image data, inherited from \code{\link{array}}.}
        \item{\code{rgb}:}{A logical value that specifies if the image is RGB or grayscale}
    }
}

\section{Methods}{
    All methods of class \code{array} plus the following defined specifically for class \code{Image}:

    \describe{
        \item{\code{display(Image)}:}{Displays an image on the screen. Only one image at a time
            can be displayed. Trying to call \code{display} when another image is on the
            screen will generate an error. At the moment it is not possible to automatically
            close the display window, please do not report it as a wish-feature.}
        \item{\code{channels(Image)}:}{Returns a list of 3 grayscale images (named \code{red},
            \code{green} and \code{blue}) that represent channels of the input image. If the input
            image is grayscale, all three resulting images are the same.}
        \item{\code{toGray(Image)}, \code{toRGB(Image)}; and \code{toRed(Image)},
            \code{toGreen(Image)}, \code{toBlue(Image)}, \code{getRed(Image)},
            \code{getGreen(Image)}, \code{getBlue(Image)}:}{Allow for conversions between RGB and
            grayscale images. Results are instances of class \code{Image} in the
            desired mode and of the same size as the source.}
        \item{\code{show(Image)} and \code{summary(Image)}:}{Print summaries of the image}
        \item{\code{write.image(Image, files)}, \code{read.image(files)} and
            \code{ping.image(files)}:}{I/O operations for images in all formats supported by
            ImageMagick. See \code{\link{Image IO}} for further details.}
        \item{\code{normalize(Image, from = 0, to = 1.0, independent = FALSE)}:}{Normalizes
            image to the given range (grayscales only). \code{independent} indicates whether
            images in a stack are to be normalized independently.}
    }
}

\details{
    Grayscales can store data in any range but many processing as well as displaying and I/O
    functions require data in the range 0..1 (from black to white). RGB images are stored as
    integers with 3 significant bytes for red, green and blue respectively. The fourth byte
    is not used and must be kept zero to allow for correct channel decomposition.

    Many image processing routines are based on ImageMagick library. The package provides an
    interface between R and ImageMagick library and extends the functionality with additional
    processing routines relevant for microscopy and biological imaging.

    Image I/O operations are supported for many 2D image formats and for image stacks,
    e.g. TIFF. Image I/O routines are based on ImageMagick routines.

    Further information on the package and this class is available in the vignette.
}

\note{
    Image processing routines are implemented as separate functions (not methods), filters,
    that accept \code{Image} as argument.
}

\seealso{
   \code{\link{Image IO}}, \code{\link{Filters 2D}}
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}

%\examples{
% FIXME:
% don't talk about stuff that doesn't exist, until it exists :)
%    \dontrun{im = read.image("test.tif")}
%    \dontshow{exp(im)}
%    \dontrun{display(im)}
%}

\keyword{methods}
\keyword{classes}
