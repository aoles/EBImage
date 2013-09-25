\name{colorLabels}

\alias{colorLabels}

\title{Color Code Labels}

\description{
  Color codes the labels of object masks by a random permutation.
}

\usage{
colorLabels(x, normalize = TRUE)
}

\arguments{
  \item{x}{an \code{Image} object in \code{Grayscale} color mode or an array containing object masks. Object masks are sets of pixels with the same unique integer value}
  \item{normalize}{if TRUE normalizes the resulting color image}
}

\value{
  An \code{Image} object containing color coded objects of \code{x}.
}

\details{
Performs color coding of object masks, which are typically obtained using the \code{bwlabel} function. Each label from \code{x} is assigned an unique color. The colors are distributed among the labels using a random permutation. If \code{normalize} is set to \code{TRUE} the intensity values of the resulting image are mapped to the [0,1] range.
}

\author{
  Bernd Fischer, 2013
}

\seealso{
  \code{\link{bwlabel}},  \code{\link{normalize}}
}

\examples{
  x = readImage(system.file('images', 'shapes.png', package='EBImage'))
  x = x[110:512,1:130]
  y = bwlabel(x)
  z = colorLabels(y)
  if (interactive()) display(z, title='Colored segmentation')
}
