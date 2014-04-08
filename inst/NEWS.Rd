\name{EBImage-NEWS}
\title{News for Package 'EBImage'}

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
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item 'getFrame', 'getNumberOfFrames' and 'colorLabels' support multi-dimensional images
      \item Proper handling of multi-dimensional character arrays by the 'Image' constructor
      \item Fixed 'getFrame' and 'combine' in case of single-channel Color Images
      \item Fixed color mode check in 'validImageObject'
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
