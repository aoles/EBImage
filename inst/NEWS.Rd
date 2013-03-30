\name{NEWS}

\title{News for Package 'EBImage'}

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




