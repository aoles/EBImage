\name{EBImage-NEWS}
\title{News for Package 'EBImage'}

\section{Changes in version 4.6.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item 'toRGB' function for convenient Grayscale to RGB conversion
      \item 'luminance' mode in 'channel' for luminance-preserving RGB to Grayscale conversion
    }
  }
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item Performance improvements to: 'Image', 'is.Image', 'readImage', 'writeImage', 'show', 'normalize', 
      'getFrame', 'selectChannel', 'rgbImage', 'colorLabels', 'flip'/'flop'
      \item Reduced memory footprint of 'readImage'
      \item 'as.Image' returns it's argument rather than the Grayscale-coerced copy when called on an 'Image' object 
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item 'getFrame', 'getNumberOfFrames' and 'colorLabels' functions support multi-dimensional images
      \item Proper handling of multi-dimensional character arrays by the 'Image' constructor
      \item Fixed 'getFrame' and 'combine' in case of single-channel Color Images
      \item Fixed color mode check in 'validImageObject'
      \item Updated man page for 'flip', 'flop', and 'rotate'
    }
  }
}

\section{Changes in version 4.4.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item New 'colorLabels' function color-coding labels of object masks by a random permutation (Bernd Fisher)
      \item Additional argument 'inputRange' to 'normalize' allowing presetting a limited input intensity range
      \item Additional argument 'thick' to 'paintObjects' controlling the thickness of boundary contours
    }
  }
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item 'normalize' and 'combine' use the generics from BiocGenerics
      \item removed the 'along' argument from 'combine'
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
      \item the range of pixel coordinates displayed in the JavaScript viewer is now (1,1):(w,h) rather than (0,0):(w-1,h-1) and matches the indices of the corresponding Image array
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
      \item greyscale functions for computation of the self-complementary top-hat (I. Kats)
      \item a median filter based on Perreault's constant time median filter (J. Barry)
    }
  }
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item removed all dependencies towards GTK+ and ImageMagick
      \item replaced the former GTK+ based 'display' function by a new one displaying images using either a JavaScript image viewer, or R's built-in raster graphics
      \item 'readImage' and 'writeImage' now rely on 'jpeg', 'png' and 'tiff' packages and do not depend on ImageMagick any more
      \item added support for images containing an alpha channel; both greyscale and color images with an alpha channel are stored as a 'colormode = Color' Image
      \item refactored the functions, not using ImageMagick any longer: 'translate', 'affine', 'rotate', 'resize'
      \item deprecated: 'blur', 'equalize'
      \item deprecated: 'drawtext', 'drawfont'
      \item deprecated: 'getFeatures', 'hullFeatures', 'zernikeMoments', 'edgeProfile', 'edgeFeatures'
      \item deprecated: 'haralickFeatures', 'haralickMatrix'
      \item deprecated: 'moments', 'smoments', 'rmoments', 'cmoments'
      \item removed 'animate'
      \item improved 'getFrame': better performance by reassigning array dimension only when needed
      \item modified 'as.raster'
      \item 'inst/images/lena.gif' is now 'inst/images/lena.png'
      \item overhauled the testing procedure in 'tests/test.R'
      \item added 'NEWS.Rd'
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item 'erode'/'dilate': incorrect range of loop indices caused memory reads from outside the kernel vector
    }
  }
}
