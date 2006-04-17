\name{Image2D}

\docType{class}

\alias{Image2D}
\alias{EBImage}
%\alias{Class Image2D}
\alias{copyImageHeader}
\alias{copyImage}
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
\alias{minMax}
\alias{normalize}
\alias{isCorrectType}
\alias{correctType}

\concept{image representation}
\concept{image analysis}
\concept{image processing}

\title{
    Class To Work With 2D Images in R
}

\description{
A class to work with 2D images in R. This is the basic class to hold image
data and parameters. The class is directly derived from \code{\link{array}}, which permits
efficient operations on image data (including subsetting, arithmetic etc).

RGB images are coded by 8 bits per channel, 24 bits per pixel, stored in
an R \code{integer} array.
Grayscale images are coded by 8 Byte (\code{double} precision floating
point) per pixel.

%% Fixme: I am not sure whether this is the right place to explain this.
%% Better would be a separate man page for 'correctType'. Do the users
%% really always need to worry about such "low-level" stuff, or just if
%% they do wild things? Also, the next sentence cannot be parsed as English :)
%
% User must take care that the internal
% data type of the \code{Image2D} array stays correct\code{integer}, otherwise it is recommended
% to run the \code{correctType} or overrriden \code{as.integer} and \code{as.double} to turn image
% data into the correct representation. \code{correctType} is used automatically wherever necessary
% (e.g. in calls to C++ routines if argument is copied!).
}

\section{Creating Objects}{
  \describe{
    \item{\code{Image2D(data, dim, rgb)}:}{Default constructor. If called with no parameters,
      a 2D grayscale image is created with 2x2 pixels whose values are 0. If \code{data} is a
      two-dimensional array, \code{dim} can be omitted.}
    
%% Fixme:
%% I don't understand the following item. When I do this, the image seems to
%% be copied anyway:
%% a=Image2D(1:16, dim=c(4,4)); b=a; a[1,1]=4711; show(a); show(b)

%        \item{\code{copyImage(x)}:}{Ensures that object data is copied into a new variable and not
%            just a pointer is associated to a new variable, as in case of a=b. Adviseable to do
%            a = copyImage(b)}

%% Fixme:
%% again, this seems like gory detail that first-time users of the
%% package need not worry about -- only later, for specific "power applications"?

%\item{\code{copyImageHeader(x, newClass = "Image2D", rgb = FALSE)}:}{Copies all source image
%            parameters into the new one except the data. The data can be copied by issuing
%            myNewImage@.Data = myOldImage@.Data. Used mostly to create new images with the same
%            parameters as a given one, but nil data. This method is prefered over simple assignment of one
%            image instance to a new variable if the data of the target image is to be smaller than
%            those of the original one.}
    }
  }

\section{Slots}{
    \describe{
        \item{\code{.Data}:}{Matrix of image data, inherited from \code{\link{array}}.}
        \item{\code{rgb}:}{A logical value that specifies if the image
	  is RGB or grayscale}
% Fixme:
% I think man pages should describe what _is_ there, not would
% could/should/might be there... (that can go in a ToDo file):
%        \item{\code{...}:}{Further slots for image metrics, resolution, compression and compression
%            quality: not implemented yet - will follow shortly.}
    }
}

\section{Methods}{
Methods from class \code{array} are inherited, some are
redefined. Additional methods and functions that work with objects of
this class are defined by this package. Besides the methods listed
below, see also the functions described in the "See Also" Section of
this manual page.

\describe{
  \item{\code{display(Image2D)}:}{Displays an image on the screen using Magick++
    display capabilities. A separate thread is created to allow for input
    control returning to the R session. Known limitation: the display window must be closed
    before the next call to \code{display}, otherwise an error message
    is generated. I expect to correct this in a future package versions.}
  \item{\code{channels(Image2D)}:}{Returns a list of 3 grayscale images (named \code{red},
    \code{green} and \code{blue}) that represent channels of the input image. If the input
    image is grayscale, all three resulting images are the same.}
  \item{\code{toGray(Image2D)} and \code{toRGB(Image2D)} as well as \code{toRed(Image2D)},
    \code{toGreen(Image2D)}, \code{toBlue(Image2D)}, \code{getRed(Image2D)},
    \code{getGreen(Image2D)}, \code{getBlue(Image2D)}:}{Allow for conversions between RGB and
    grayscale images. Results are instances of class \code{Image2D} (\code{Image3D}) in the
    desired mode and of the same dimensions as the source.}
  \item{\code{show(Image2D)} and \code{summary(Image2D)}:}{print summaries of the image.}
%            Although RGB images are stored with colors byte-aligned in integer
%            variables, for display purposes (as well as for mathematical and arithmentical routines)
%            every color is converted to a double in the range 0..1. Therefore, every pixel of
%            an RGB image is displayed as \code{R(double):G(double):B(double)}, e.g.
%            \code{0.77:0.58:0.89}, where 0.77 is red, 0.58 - green and 0.89 blue on the scale 0..1.
%            See additionally \code{\link{print}}.}
  \item{\code{write.image(Image2D, files)}, \code{read.image(files)} and
    \code{ping.image(files)}:}{I/O operations for images in all formats supported by ImageMagick. See
    \code{\link{Image IO}} for details.}
% FIXME (wh)
% It would be nicer if this were called "range"  
  \item{\code{minMax(Image2D)}:}{Returns a numeric vector of two elements with the minimum and maximum
    values of the image data.}
  \item{\code{normalize(Image2D, from = 0, to = 1.0, independent = FALSE, modify =
    FALSE)}:}{Normalizes the image to the given range (grayscales only). \code{independent}
    argument is used only for 3D images to indicate that every image in a stack should be
    normalized independently. Set \code{modify} to TRUE to modify the
    original object in place (without copying).}

% FIXME (wh)
% I think these two functions (as.integer, as.double)  are misnamed. In
% R, the convention is that the class of the result of as.foo(x) should be 'foo'!
% Do we really need them? If possible, I'd vote for removing them.
% Also, what do you mean by "correct"? I saw no code in this method that
% ensures "correctness", whatever that means.
  \item{\code{as.integer(Image2D, ...)}:}{Returns a new image, with all data in correct integer
         representation for RGB. All other attributes are copied.}
  \item{\code{as.double(Image2D, ...)}:}{Returns a new image, with all data in correct double
    representation for grayscale. All other attributes are copied.}

% FIXME (wh)
% I think this should be done using the conventional way instead, using the
% 'validObject' function and the 'validity' argument to 'setClass'
   \item{\code{isCorrectType(Image2D)}:}{Checks if the internal represantation of data is correct -
     double for grayscales and integer for RGBs.}

% FIXME (wh)
% I don't like the idea of exposing this function to the user. All
% methods that work on Image2D objects should automatically return a valid
% object and there should be no need for such fudge; and if a user wants
% to write a his own methods, again it is his responsibility and he
% should use your existing functions as a template. This should never be necessary.   
   \item{\code{correctType(Image2D)}:}{Corrects the internal representation of data to
         double for grayscales and integer for RGBs.}
    }
}

\details{
    Grayscales should be normalised in the range between 0 (black) and 1.0 (white or full single-channel
    color). RGB images are held as integers with 3 significant bytes for red, green and
    blue respectively. The fourth byte is not used and must be kept zero to allow for correct
    channel decomposition.

    Many of the 2D image processing operations provided in this package are
    based on the ImageMagick::Magick++ library, which is written in C++.
    This package provides an interface between R and the \code{Image2D}
    class, on one side, and the Magick::Image
    class and the functions in the ImageMagick::Magick++ library on the
    other side.

    Image I/O is supported for many 2D image formats and for image stacks,
    e.g. TIFF. Image I/O routines are based on ImageMagick routines.

    Further information on the package and this class is available in a vignette.
}

\note{
    Image processing routines are implemented as separate functions (filters), accepting \code{Image2D}
    as argument.
}

\seealso{
   \code{\link{Image3D}}, \code{\link{Image IO}}, \code{\link{Filters 2D}}.
}


% Fixme: the intention of this documentation section is to specify where
% the the algorithm or the code for a function come from, not where you
% can find the source code - because this is standardized in R package
% and hence (fortunately) needs no package-specific documentation.
%\source{
%   R-sources are available from 'R/class.Image2D.R' with c++ sources in 'inst/src/conversions.cpp',
%   'inst/src/display.cpp' and 'inst/src/io.cpp' of the package 'EBImage'.
%}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}

\examples{
% FIXME:
% don't talk about stuff that doesn't exist, until it exists :)
%    # more to follow
%    \dontrun{im = read.image("test.tif")}
%    \dontshow{exp(im)}
%    \dontrun{display(im)}
}

\keyword{methods}
\keyword{classes}
