\name{combine}

\alias{combine}
\alias{combine,array,array-method}
\alias{combine,matrix,matrix-method}
\alias{combine,Image,Image-method}
\alias{combine,list,missing-method}

\title{Combining images}

\description{
  Merges images to create image sequences.
}

\usage{
combine(x, y, ...)
}


\arguments{
   \item{x}{An \code{Image} object, an array, or a list of \code{Image} objects and arrays.}
   \item{y}{An \code{Image} object or an array.}
   \item{...}{\code{Image} objects or arrays.}
}

\value{
  An \code{Image} object or an array.
}

\details{
  The function \code{combine} uses \code{abind} to merge multi-dimensional
  arrays along the dimension depending on the
  color mode of \code{x}. If \code{x} is a \code{Grayscale} image or an array,
  image objects are combined along the third dimension, whereas when
  \code{x} is a \code{Color} image they are combined along the forth dimension, leaving room on the third dimension for color
  channels.
}

\seealso{
  \code{\link{Image}}
}

\author{
  Gregoire Pau, Andrzej Oles, 2013
}

\examples{
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


