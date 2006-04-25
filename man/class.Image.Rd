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
data and parameters. The class is directly derived from \code{\link{array}}, which enables
efficient operations on image data (including subsetting, arithmetic etc).

RGB images are coded in 8 bits per channel, 24 bits per pixel, stored in
an R \code{integer} array. Grayscale images are coded \code{double} precision.

%% Fixme: I am not sure whether this is the right place to explain this.
%% Better would be a separate man page for 'correctType'. Do the users
%% really always need to worry about such "low-level" stuff, or just if
%% they do wild things? Also, the next sentence cannot be parsed as English :)
%
% User must take care that the internal
% data type of the \code{Image} array stays correct\code{integer}, otherwise it is recommended
% to run the \code{correctType} or overrriden \code{as.integer} and \code{as.double} to turn image
% data into the correct representation. \code{correctType} is used automatically wherever necessary
% (e.g. in calls to C++ routines if argument is copied!).
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
%            Although RGB images are stored with colors byte-aligned in integer
%            variables, for display purposes (as well as for mathematical and arithmentical routines)
%            every color is converted to a double in the range 0..1. Therefore, every pixel of
%            an RGB image is displayed as \code{R(double):G(double):B(double)}, e.g.
%            \code{0.77:0.58:0.89}, where 0.77 is red, 0.58 - green and 0.89 blue on the scale 0..1.
%            See additionally \code{\link{print}}.}
        \item{\code{write.image(Image, files)}, \code{read.image(files)} and
            \code{ping.image(files)}:}{I/O operations for images in all formats supported by
            ImageMagick. See \code{\link{Image IO}} for further details.}
        \item{\code{normalize(Image, from = 0, to = 1.0, independent = FALSE)}:}{Normalizes
            image to the given range (grayscales only). \code{independent} indicates whether
            images in a stack are to be normalized independently.}

% FIXME (wh)
% I think this should be done using the conventional way instead, using the
% 'validObject' function and the 'validity' argument to 'setClass'
% (os)
% not sure if you suggest what I need here - unfortunately internal representation of array
% changes dynamically between floating point and integer and I need to assure what is that type
% but you are right in a way - this is not used outside usually, can be hidden
%        \item{\code{isCorrectType(Image)}:}{Checks if the internal represantation of data is correct -
%            double for grayscales and integer for RGBs.}

% FIXME (wh)
% I don't like the idea of exposing this function to the user. All
% methods that work on Image objects should automatically return a valid
% object and there should be no need for such fudge; and if a user wants
% to write a his own methods, again it is his responsibility and he
% should use your existing functions as a template. This should never be necessary.
% (os) - internal now
%   \item{\code{correctType(Image)}:}{Corrects the internal representation of data to
%         double for grayscales and integer for RGBs.}
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
