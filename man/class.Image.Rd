\name{Image}

\docType{class}

\alias{Image}
\alias{Image-class}
\alias{is.validImage}
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
\alias{assert}
\alias{plot,Image-method}
\alias{show,Image-method}
\alias{print,Image-method}
\alias{as.array,Image-method}

% deprecating
\alias{Image2D}
\alias{Image3D}
\alias{minMax}
\alias{copyImage}
\alias{copyImageHeader}



\concept{image representation}
\concept{image analysis}
\concept{image processing}

\title{
    Class/methods definitions to work with images in R
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
        \item{\code{display(Image)}:}{Displays an image on the screen. Only one Image at a time
            can be displayed. Trying to call \code{display} when another image is on the
            screen will generate an error. At the moment it is not possible to automatically
            close the display window, please do not report it as a wish-feature.}
        \item{\code{plot(Image)}:}{A slow substitution for \code{display} based on \code{graphics:::image}
            method. This method works for grayscale images only and supports multiple windows.
            In contrast to \code{display}, only the first image of a stack is displayed.}
        \item{\code{channels(Image)}:}{Returns a list of 3 grayscale images (named \code{red},
            \code{green} and \code{blue}) that represent channels of the input image. If the input
            image is grayscale, all three resulting images are the same.}
        \item{\code{toGray(Image)}, \code{toRGB(Image)}; and \code{toRed(Image)},
            \code{toGreen(Image)}, \code{toBlue(Image)}, \code{getRed(Image)},
            \code{getGreen(Image)}, \code{getBlue(Image)}:}{Allow for conversions between RGB and
            grayscale images. Results are instances of class \code{Image} in the
            desired mode and of the same size as the source.}
        \item{\code{show(Image)}, \code{print(Image, data = FALSE)} and \code{summary(Image)}:}{Print
            summaries of the image. Specify \code{data = TRUE} in \code{print()} to print all
            data instead of the summary.}
        \item{\code{write.image(Image, files)}, \code{read.image(files)} and
            \code{ping.image(files)}:}{I/O operations for images in all formats supported by
            ImageMagick. See \code{\link{Image IO}} for further details.}
        \item{\code{normalize(Image, from = 0, to = 1.0, independent = FALSE)}:}{Normalizes
            image to the given range (grayscales only). \code{independent} indicates whether
            images in a stack are to be normalized independently.}
        \item{\code{assert(Image)} and \code{is.validImage(Image)}:}{Checks if the object is Image and
            if it is a valid image. The former is implemented as method and will generate an error if
            supplied object is not Image. The latter is a simple function and will generate a warning.}
        \item{\code{assert(Image,Image)}:}{Checks if two images have the same size and color scheme.
            If yes, returns TRUE, otherwise generates warnings and returns FALSE.}
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

    DEPRECATED:

    Classes \code{Image2D} and \code{Image3D} are deprecated and combined into one class \code{Image}.
    Functions \code{Image2D} and \code{Image3D} are deprecated, please use \code{Image} with the same
    syntax instead. \code{minMax} is deprecated, please use \code{\link{range}} instead. \code{copyImage}
    image is deprecated, please use \code{copy} instead. \code{copyImageHeader} is deprecated and should
    not be used. The substitution function \code{.copyHeader} should be used by the package
    developers/package functions only.
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

\examples{
    # create 2 empty images of 300x200 size
    im1 <- Image(0, c(300,200,2))
    # fill images with two different data sets (waves)
    for (i in 1:dim(im1)[[1]])
        for (j in 1:dim(im1)[[2]]) {
            im1[i,j,1] <- abs(sin(i*0.0314)*sin(i*0.0314) + cos(j*0.0314)*cos(j*0.0314))
            im1[i,j,2] <- abs(sin(i*0.0314)) + abs(cos(j*0.0314))
        }
    # normalize images independently
    im1 <- normalize(im1, independent = TRUE)
    # get a copy of the second image
    imx <- copy(im1)
    # or simply
    imx <- im1[,,1]
    # invert image (pay attention it was first normalized to 0..1)
    imx <- 1.0 - imx
    # combine two images as two color channels in one RGB image
    im <- toRed(im1[,,1]) + toGreen(im1[,,2])
    # save image
    write.image(im, "Image.Rd.example.tif")
    \dontrun{display(im1)}
}

\keyword{methods}
\keyword{classes}
