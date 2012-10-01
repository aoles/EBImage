\name{NEWS}

\title{News for Package 'EBImage'}

\section{Changes in version 3.99.19}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item improved JavaScript image viewer with some minor bugs fixed
      \item removed unused files from 'scripts/'
      \item added 'NEWS.Rd'
    }
  }
}

\section{Changes in version 3.99.18}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item further improvements to the JavaScript image viewer; exit on-screen help using either 'ESC' or 'Q'
      \item added 'transpose' to unit tests
    }
  }
}

\section{Changes in version 3.99.17}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item removed all dependencies towards GTK+ and ImageMagick
      \item deprecated: 'blur', 'equalize'
      \item deprecated: 'drawtext', 'drawfont'
      \item deprecated: 'getFeatures', 'hullFeatures', 'zernikeMoments', 'edgeProfile', 'edgeFeatures'
      \item deprecated: 'haralickFeatures', 'haralickMatrix'
      \item deprecated: 'moments', 'smoments', 'rmoments', 'cmoments'
      \item overhauled the testing procedure in 'tests/test.R'
    }
  }
}

\section{Changes in version 3.99.16}{
  \subsection{BUG FIXES}{
    \itemize{
      \item CSS fix disabling bilinear filtering of rescaled images in the JavaScript image viewer
    }
  }
}

\section{Changes in version 3.99.15}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item numerous improvements and bug fixes to the JavaScript image viewer, including: on-screen help displayed by pressing 'H', and fixed mouse wheel action (now consistent behavior across browsers and platforms)
    }
  }
}

\section{Changes in version 3.99.13}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item improved JavaScript image viewer with upgraded user interface
    }
  }
}

\section{Changes in version 3.99.12}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item renamed 'swapXY' to 'transpose'
      \item updated documentation on 'transpose', 'readImage', writeImage', and 'display'
    }
  }
}

\section{Changes in version 3.99.11}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item better argument handling in the 'display' function
      \item a message rather than a warning is now issued when displaying multiframe images using the 'raster' method
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item range clipping to [0:1] in 'writeImage'
    }
  }
}

\section{Changes in version 3.99.10}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item improved 'getFrame' function: better performance by reassigning array dimension only when needed
    }
  }
}

\section{Changes in version 3.99.9}{
  \subsection{BUG FIXES}{
    \itemize{
      \item 'font-face' misspelling in 'display.template'
      \item restored the original 'getFrame' function
    }
  }
}

\section{Changes in version 3.99.8}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item substituted the former GTK+ based 'display' function by the new one relying on 'displayInBrowser' and 'displayRaster' functions
      \item substituted all calls to 'paste0' by 'paste'
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item disabled bilinear filtering of rescaled images in the JavaScript image viewer
      \item a warning message in the 'displayRaster' function was triggered for single-frame images
    }
  }
}

\section{Changes in version 3.99.7}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item changed handling of greyscale images containing an alpha channel (aka. GA): instead of discarding the second channel and reading pixel data into a 'Greyscale' Image both channels are now stored in a 'Color' Image
      \item removed 'readImageOld' and 'writeImageOld'
      \item removed 'inst/scripts/newAgainstOld.R'
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item updated 'inst/scripts/readWriteTest.R'
    }
  }
}

\section{Changes in version 3.99.5}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item 'displayInBrowser': preview of the new JavaScript based image viewer
    }
  }
}

\section{Changes in version 3.99.4}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item 'readImage' can now read directly from URLs
      \item some fixes and improvements to 'displayRaster'
    }
  }
}

\section{Changes in version 3.99.3}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item replaced 'readImage' and 'writeImage' by the new 'readImage2' and 'writeImage2' functions; moved the previous ones to 'readImageOld' and 'writeImageOld'
    }
  }
}

\section{Changes in version 3.99.2}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item 'readImage2' and 'writeImage2' functions: candidates for replacing the original ImageMagick based I/O functions
      \item 'swapXY' function swapping the first two indices of an image array during I/O operations
      \item 'displayRaster' function displaying images using R graphics
    }
  }
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item bumped version from 4.0.1 to 3.99.2 to prepare the future 4.0.0 version
      \item refactored the functions, not using ImageMagick any longer: 'translate', 'affine', 'rotate', 'resize'
      \item removed 'animate'
      \item deprecated 'blur' and 'equalize'
      \item modified 'getFrame': improved performance
      \item modified 'as.raster'
      \item 'inst/images/lena.gif' is now 'inst/images/lena.png' (preparing for dropping GIF support)
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item 'erode'/'dilate' functions: incorrect range of loop indices caused memory reads from outside the kernel vector
    }
  }
}

\section{Changes in version 3.13.2}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item deprecated 'equalize'
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item occurrences of partial argument matching that 'R CMD check' had complained about
    }
  }
}

\section{Changes in version 3.13.1}{
  \subsection{BUG FIXES}{
    \itemize{
      \item some matrices in 'ImageMagickCall' were not normalized to [0;1] before IM calls
    }
  }
}

\section{Changes in version 3.13.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item greyscale functions for computation of the self-complementary top-hat (I. Kats)
      \item a median filter based on Perreault's constant time median filter (J. Barry)
    }
  }
}
