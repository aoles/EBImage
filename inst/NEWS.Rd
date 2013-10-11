\name{NEWS}

\title{News for Package 'EBImage'}

\section{Changes in version 4.4.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item New \code{colorLabels} function color-coding labels of object masks by a random permutation (Bernd Fisher)
      \item Additional argument \code{inputRange} to \code{normalize} allowing presetting a limited input intensity range
      \item Additional argument \code{thick} to \code{paintObjects} controlling the thickness of boundary contours
    }
  }
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item \code{normalize} and \code{combine} use the generics from BiocGenerics
      \item removed the \code{along} argument from \code{combine}
      \item Re-introduced calculation of 's.radius.sd' (standard deviation of the mean radius) in cell features
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{getFrame}: XY dimensions equal 1 were dropped
    }
  }
}




