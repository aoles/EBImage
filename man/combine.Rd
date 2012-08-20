\name{Combine}

\alias{combine}

\title{Combining images}

\description{
  Merges images to create image sequences.
}

\usage{
  combine(x, ..., along) 	
}


\arguments{
   \item{x}{An \code{Image} object, an array, or a list of \code{Image} objects and arrays.}
   \item{...}{\code{Image} objects or arrays.}
   \item{along}{an optional numeric. See details.}
}

\value{
  An \code{Image} object or an array.
}

\details{
  The function \code{combine} uses \code{abind} to merge multi-dimensionnal
  arrays along the dimension specified by the value \code{along}.

  If \code{along} is missing, a default value depending on the
  color mode of \code{x} is used. If \code{x} is a \code{Grayscale} image or an array,
  \code{along} is set to 3 and image objects are combined on this dimension. If
  \code{x} is a \code{Color} image, \code{along} is set to 4 and image objects
  are combined on this dimension, leaving room on the third dimension for color
  channels.
}

\seealso{
  \code{\link{Image}}
}

\author{
  Gregoire Pau
}

\examples{
  if (interactive()) {
    ## combination of color images
    lena = readImage(system.file("images", "lena-color.png", package="EBImage"))
    x = combine(lena, flip(lena), flop(lena))
    if (interactive()) display(x)

    ## Blurred lenas
    x = resize(lena, 128, 128)
    xt = list()
    for (t in seq(0.1, 5, len=9)) xt=c(xt, list(gblur(x, s=t)))
    xt = combine(xt)
    if (interactive()) display(xt, title='Blurred Lenas')
  }
}


