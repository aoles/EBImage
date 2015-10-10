\name{EBImage-NEWS}
\title{News for Package 'EBImage'}

\section{Changes in version 4.12.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \sQuote{antialias} argument to the function \sQuote{affine} allowing to switch off bilinear filtering around image borders
    }
  }
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item deprecated \sQuote{...GreyScale} morphological functions; use common functions \sQuote{dilate}, \sQuote{erode}, \sQuote{opening}, \sQuote{closing}, \sQuote{whiteTopHat}, \sQuote{blackTopHat} and \sQuote{selfComplementaryTopHat} for filtering both binary and grayscale images
      \item \sQuote{resize} doesn't perform bilinear filtering at image borders anymore in order to prevent the blending of image edges with the background when the image is upscaled; to switch on bilinear sampling at image borders use the function argument \sQuote{antialias = TRUE}
      \item \sQuote{floodFill} and \sQuote{fillHull} preserve storage mode
    }
  }
  \subsection{PERFORMANCE IMPROVEMENTS}{
    \itemize{
      \item all morphological operations use the efficient Urbach-Wilkinson algorithm (up to 3x faster compared to the previous implementation)
      \item \sQuote{rotate}: perform lossless 90/180/270 degree rotations by disabling bilinear filtering
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item reimplemented the Urbach-Wilkinson algorithm used to perform grayscale morphological transformations
      \item improved pixel-level accuracy of spatial linear transformations: \sQuote{affine}, \sQuote{resize}, \sQuote{rotate} and \sQuote{translate}
      \item \sQuote{display(..., method = "raster")}: displaying of single-channel color images
      \item \sQuote{drawCircle}: corrected x-y offset
      \item \sQuote{equalize}: in case of a single-valued histogram issue a warning and return its argument
      \item \sQuote{hist}: accept images with \sQuote{colorMode = Color} containing less than three color channels
      \item \sQuote{image}: corrected handling of image frames
      \item \sQuote{medianFilter}: filter size check
      \item \sQuote{normalize}: normalization of a flat image when the argument \sQuote{separate} is set to \sQuote{FALSE}
      \item \sQuote{reenumerate}: corrected handling of images without background
      \item \sQuote{stackObjects}: corrected handling of blank images without any objects
      \item \sQuote{tile}: reset dimnames
    }
  }
}

\section{Changes in version 4.10.1}{
\subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item \sQuote{rgbImage} and \sQuote{combine} use dimnames if present
      \item \sQuote{makeBrush(..., shape = "line")}: the line is drawn symmetrically around the center of the kernel, and the argument \sQuote{size} sets the maximum x-y dimension of kernel rather than the line length
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item \sQuote{drawCircle} and \sQuote{stackObjects}: corrected handling of color channels
      \item \sQuote{ocontour}: doubled the size of the temporary buffer to accommodate longer contours and fixed indexing issue
    }
  }
}

\section{Changes in version 4.10.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item 'paintObjects' allows control over behavior at image edges
      \item 'getFrames' function returning a list of Image objects
      \item default 'display' method can be set by \code{options("EBImage.display")}
      \item 'short' argument to the Image 'print' method
      \item 'equalize' function performing histogram equalization
    }
  }
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item 'translate' moves the image in the opposite direction as before
      \item 'rotate' returns an image centered in a recalculated bounding box 
      \item 'as.Image' coerces subclasses to Image objects
      \item 'getFrame' returns individual channels of a Color image as Grayscale frames
      \item  'display' method defaults to 'raster' unless used interactively
      \item 'combine' allows to combine images of the same color mode only
    }
  }
  \subsection{PERFORMANCE IMPROVEMENTS}{
    \itemize{
      \item 'filter2': calculate FFT using methods from the 'fftwtools' package
      \item 'as.nativeRaster': now implemented in C
      \item 'medianFilter': double to integer conversion moved to C code
      \item 'hist': fixed binning issues and improved performance
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item 'medianFilter': fixed multi-channel image handling; preserve original object class
      \item 'as.nativeRaster': allow for an arbitrary number of color channels
      \item 'display': set missing color channels to blank
    }
  }
}

\section{Changes in version 4.8.3}{
  \subsection{BUG FIXES}{
    \itemize{
      \item 'paintObjects': the body of an object with ID=1 was not highlighted when option \code{thick} was set
    }
  }
}

\section{Changes in version 4.8.2}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Clip images in 'as.nativeRaster'
    }
  }
}

\section{Changes in version 4.8.1}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item New sample PNG images
    }
  \subsection{BUG FIXES}{
    \itemize{
      \item Workaround to broken 'Math2' S4GroupGenerics in R 3.1.2
    }
  }
  }
}

\section{Changes in version 4.8.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item 'otsu' thresholding method (contributed by Philip A. Marais, University of Pretoria, South Africa)
      \item Support for dimnames in Image objects
      \item 'bg.col' argument to 'affine' transformations
      \item 'reenumerate' argument to 'rmObjects'
      \item 'names' argument to 'readImage'
      \item 'as.array' method for Image objects
      \item 'as.nativeRaster' function
    }
  }
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item Performance improvements to 'Image', 'selectChannel', 'combine ' and 'reenumerate'
      \item Use a more efficient 'nativeRaster' representation in 'displayRaster'
      \item Cleaner output of the 'show-Image' method; print true object class name and dimorder (if set)
      \item 'readImage' sets Image dimnames to corresponding file names
      \item 'filter2' and 'affine' return object of the same class as input
      \item Renamed 'getNumberOfFrames' to 'numberOfFrames'
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Handling of dimensions of character arrays
      \item Drawing of grid lines in 'displayRaster'
      \item Passing of '...' arguments in 'readImage'
    }
  }
}

\section{Changes in version 4.6.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item 'toRGB' function for convenient grayscale to RGB conversion
      \item 'luminance' mode in 'channel' for luminance-preserving RGB to grayscale conversion
    }
  }
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item Performance improvements to: 'Image', 'is.Image', 'readImage', 'writeImage', 'show', 'normalize', 
      'getFrame', 'selectChannel', 'rgbImage', 'colorLabels', 'flip'/'flop'
      \item Reduced memory footprint of 'readImage'
      \item When called on an 'Image' object, 'as.Image' returns its argument rather than the Grayscale-coerced copy  
      \item 'displayRaster' sets 'par$usr' coordinates to image pixel coordinates easing up plot annotation
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item 'getFrame', 'getNumberOfFrames' and 'colorLabels' support multi-dimensional images
      \item Proper handling of multi-dimensional character arrays by the 'Image' constructor
      \item Fixed 'getFrame' and 'combine' in case of single-channel Color Images
      \item Fixed color mode check in 'validImageObject'
      \item Proper 'fg.col' and 'bg.col' color handling in 'tile'
      \item Updates to documentation
    }
  }
}

\section{Changes in version 4.4.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item New 'colorLabels' function for color-coding labels of object masks by a random permutation (Bernd Fisher)
      \item Additional 'inputRange' argument to 'normalize' allowing for presetting a limited input intensity range
      \item Additional 'thick' argument to 'paintObjects' controlling the thickness of boundary contours
    }
  }
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item 'normalize' and 'combine' use the generics from BiocGenerics
      \item Removed the 'along' argument from 'combine'
      \item Re-introduced calculation of 's.radius.sd' (standard deviation of the mean radius) in cell features
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item XY dimensions equal 1 were dropped by 'getFrame'
    }
  }
}

\section{Changes in version 4.2.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item 'localCurvature' function for computing local curvature along a line (J. Barry)
    }
  }
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item The range of pixel coordinates displayed in the JavaScript viewer is now (1,1):(w,h) rather than (0,0):(w-1,h-1) and matches the indices of the corresponding Image array
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item 'erode'/'dilate': fixed a bug introduced in the previous version (4.0.0)
      \item 'resize': new image width was calculated incorrectly when only height was provided (reported by B. Fischer)
      \item 'medianFilter': incorrect [0:1] <-> integer range conversion (thanks to K. Johnson)
    }
  }
}

\section{Changes in version 4.0.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item 'transpose' function for transposing an image by swapping its spatial dimensions
      \item Grayscale functions for computing the self-complementary top-hat (I. Kats)
      \item Median filter based on Perreault's constant time median filter (J. Barry)
    }
  }
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item Removed all dependencies towards GTK+ and ImageMagick
      \item Replaced the former GTK+ based 'display' function by a new one displaying images using either a JavaScript image viewer, or R's built-in raster graphics
      \item 'readImage' and 'writeImage' now rely on 'jpeg', 'png' and 'tiff' packages and do not depend on ImageMagick any more
      \item Added support for images containing an alpha channel; both grayscale and color images with an alpha channel are stored as an Image with 'colormode = Color'
      \item Re-factored the functions, not using ImageMagick any longer: 'translate', 'affine', 'rotate', 'resize'
      \item Deprecated: 'blur', 'equalize', 'drawtext', 'drawfont', 'getFeatures', 'hullFeatures', 'zernikeMoments', 'edgeProfile', 'edgeFeatures', 'haralickFeatures', 'haralickMatrix', 'moments', 'smoments', 'rmoments', 'cmoments', 'animate', 
      \item Improved 'getFrame': better performance by reassigning array dimension only when needed
      \item Modified 'as.raster'
      \item 'inst/images/lena.gif' is now 'inst/images/lena.png'
      \item Overhauled the testing procedure in 'tests/test.R'
      \item Added 'NEWS.Rd'
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item 'erode'/'dilate': incorrect range of loop indices caused memory reads from outside the kernel vector
    }
  }
}
