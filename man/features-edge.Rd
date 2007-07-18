\name{edge.features}

\alias{edge.features}
\alias{edge.profile}

\concept{feature extraction}

\title{ Extraction of edge profiles and edge features from images of indexed objects }

\description{
  Edge profile is a distance profile from the geometric center of the object
  to all its perimeter points calculated by taking the distance at different
  rotation angles (rotation around the center). The profile is calculated for
  the rotation angle, theta in \code{[-pi,pi]}.
}

\section{ Usage }{
  \preformatted{

  ## S4 methods for x='IndexedImage' and ref='Image'/'missing'

  edge.profile(x, ref, n=32, fft=TRUE, scale=TRUE, rotate=TRUE)

  ## S4 method for x='IndexedImage'

  edge.features(x, ref)
}}

\section{ Arguments }{ \describe{

  \item{\code{x}}{An object of \code{\link{IndexedImage-class}}.}

  \item{\code{ref}}{A reference Grayscale image of the same size as \code{x}.
    See details.}

  \item{\code{n}}{ An integer value giving the number of angle measures. The
    full circle of \code{[-pi,pi]} is divided into \code{n-1} segments, at
    which edges the profile is approximated. }

  \item{\code{fft}}{ A logical value. If \code{TRUE}, the resulting profile is the
    \code{\link{fft}} transformation of the distance profile giving the
    frequences of angular changes in shape. }

  \item{\code{scale}}{ A logical value. If \code{TRUE}, the resulting profile
    is scaled by the effective radius (calcualted as part of
    \code{link{hull.features}}) making the profile scale invariant. }

  \item{\code{rotate}}{ A logical value. If \code{TRUE}, the resulting profile
    is shifted by the object's roation angle (calculated from the
    \code{\link{moments}} on the \code{ref} image, if provided, and on the hull
    otherwise. }

}}

\section{ Value }{
  For a single frame, both functions return a matrix of descriptors with
  objects in rows and ordered profile points (or features) in columns.
  For image stacks, a list of such matrices.

  The matrix columns in \code{edge.profile} correspond, from left to right,
  to the equidistant divisions of the range \code{[-pi,pi]} if \code{fft} is
  not used, otherwise to the frequences of angular changes (lower frequences on
  the left, higher on the right up to the middle of the vector).
}

\section{ Details }{
  The \code{ref} image can be omitted. However, if supplied it affects the
  centers of objects and their angles of rotation as in \code{\link{moments}}.

  The \code{edge.features} runs with \code{n=16} calculating scale and rotation
  invariant features. From the original distance profile, it returns the measure
  of object's irregularity taken by calculating the difference between the
  farthest and the closest to the center edge points. Then it computes the
  \code{fft} and returns 4 of it's lowest frequences, corresponding to
  \code{2*Pi, 2*Pi/2=Pi, 2*Pi/3} and \code{2*Pi/4=Pi/2}.

  The extracted feature names carry a \code{e.} prefix to indicate edge features.
}

\seealso{\code{
  \link{IndexedImage-class}, \link{watershed}, \link{getFeatures},
  \link{hull.features}, \link{moments}
}}

\author{
  Oleg Sklyar <\email{osklyar@ebi.ac.uk}>, 2007
}

\examples{
}

\keyword{file}

