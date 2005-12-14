\name{Image2D}

\docType{class}

\alias{Image2D}
%\alias{Class Image2D}
\alias{Image2D.CopyHeader}
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
\alias{to16bit}
\alias{minMax}
\alias{normalize}

\concept{image representation}
\concept{image analysis}
\concept{image processing}

\title{
    Class To Work With 2D Images in R
}

\description{
    A class to work with 2D images in R. This is the basic class to hold image
    data and parameters. The class is directly derived from \code{array}, which ensures
    fast operations with image data (including subsetting etc). Data is stored in \code{integer}
    format for both RGB and grayscale images. RGB images are 8-bit-based per channel, whereas
    grayscale images are 16-bit in the range 0..65535. User must take care that the internal
    data type of the \code{Image2D} array stays \code{integer}, if \code{is.integer} returns
    FALSE, it is recommended to run the overrriden \code{as.integer} on the image to turn image
    data to the correct representation. \code{as.integer} is used automatically wherever necessary
    (e.g. in calls to C++ routines).
}

\section{Creating Objects}{
    \describe{
        \item{\code{Image2D(data, dim, rgb)}:}{Default constructor. Executed with no parameters,
            an empty (black) 2D grayscale image of 2x2 pixels is created. If \code{data} is an
            array of 2x2, \code{dim} can be omitted.}
        \item{\code{Image2D.CopyHeader(x, data, dim)}:}{Copies all source image parameters into the
            new one except the data. The data is taken from \code{data} variable. Used mostly
            to create new images with the same parameters as a given one, but nil data. This
            method is prefered over simple assignment of one image instance to a new variable if
            the data of the target image is to be smaller than those of the original one. It is also
            possible to copy the headers from one image, \code{object} and assign it data,
            \code{ext.data} that can be coersed to an array of size \code{ext.dim} in one call.}
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
    All methods inherited from class 'array', some redefined here.

    The following list gives an overview of all methods of class 'Image2D' (except directly inherited
    from \code{\link{array}}). Detailed description is given here only for generic methods: follow
    the links to read more on other methods that have their own help pages.

    \describe{
        \item{\code{display(Image2D)}:}{Displays image on the screen using Magick++
            display capabilities. The image is displayed in a separate thread to allow for input
            control returning to the R session. Known bug/feature: display window must be closed
            before any next call to \code{display}, otherwise an error message is generated. This is
            to be corrected in one of the following versions.}
        \item{\code{channels(Image2D)}:}{Returns a list of 3 grayscale images (named \code{red},
            \code{green} and \code{blue}) that represent channels of the input image. If the input
            image is grayscale, all three resulting images are the same.}
        \item{\code{toGray(Image2D)} and \code{toRGB(Image2D)} as well as \code{toRed(Image2D)},
            \code{toGreen(Image2D)}, \code{toBlue(Image2D)}, \code{getRed(Image2D)},
            \code{getGreen(Image2D)}, \code{getBlue(Image2D)}:}{Allow for conversions between RGB and
            grayscale images. Results are instances of class \code{Image2D} (\code{Image3D}) in the
            desired mode  and of the same dim as the source.}
        \item{\code{show(Image2D)} and \code{summary(Image2D)}:}{Wrappers for the generic
            \code{\link{show}} and \code{\link{summary}} methods, that display summaries of the image.}
%            Although RGB images are stored with colors byte-aligned in integer
%            variables, for display purposes (as well as for mathematical and arithmentical routines)
%            every color is converted to a double in the range 0..1. Therefore, every pixel of
%            an RGB image is displayed as \code{R(double):G(double):B(double)}, e.g.
%            \code{0.77:0.58:0.89}, where 0.77 is red, 0.58 - green and 0.89 blue on the scale 0..1.
%            See additionally \code{\link{print}}.}
        \item{\code{write.image(Image2D, files)}, \code{read.image(files)} and \code{ping.image(files)}:
            }{Enable I/O operations for images in all formats supported by ImageMagick. See
            \code{\link{Image IO}} for details.}
        \item{\code{min.max(Image2D)}:}{Returns a numeric vector of two elements with the minimum and maximum
            values of the image data.}
        \item{\code{normalize(Image2D, from = 0, to = 65535)}:}{Returns a new image (reasonable only for
            grayscales) normalized to the given range. For internal calculations, values up to 15 Mln. are
            supported, but only first two bytes are used on all operations requiring ImageMagick calls.}
        \item{\code{as.integer(Image2D, ...)}:}{Returns a new image, with all data in correct integer
            representationon. All other attributes are copied.}
        \item{\code{to16bit(Image2D, bits=12)}:}{Scales integer grayscale data to 16-bit range [0..65535)
            assuming that original data were obtained with \code{bits}-bit camera.}
    }
}

\details{
    Grayscales should be normalised in the range between 0 (black) and 65535 (white or full single-channel
    color). RGB images are hold as integers with 3 significant bytes for red, green and
    blue respectively. The fourth byte is not used and must be kept zero to allow for correct
    channel decomposition.

    Many operations of 2D image processing are based on the ImageMagick::Magick++ library and are
    written in C++. C++ code of this package features simple functions to convert between this 'Image2D'
    class data and Magick::Image class to allow for easy extension of the package to the full range of
    ImageMagick image processing features.

    Image I/O are supported for most 2D image formats and image stacks, \code{\link{Image3D}},
    i.e. TIFF and similar file formats. Image I/O routines are based on ImageMagick routines.
    To read or write an image there is no need to specify the particular file format, only the file
    extension(s) is required.

    Further information on the package and this class is available in a vignette, see
    (\code{\link{openVignette}}) from the 'Biobase' package.
}

\note{
    Image processing routines are implemented as separate functions (filters), accepting \code{Image2D}
    as argument.
}

\seealso{
   \code{\link{Image3D}}, \code{\link{Image IO}}, \code{\link{Filters 2D}}.
}

\source{
   R-sources are available from 'R/class.Image2D.R' with c++ sources in 'inst/src/conversions.cpp',
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
