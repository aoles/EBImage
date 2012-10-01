\name{NEWS}

\title{News for Package 'EBImage'}

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
      \item 'erode' and 'dilate': incorrect range of loop indices caused memory reads from outside the kernel vector
    }
  }
}




