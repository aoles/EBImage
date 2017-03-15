\name{transpose}

\alias{transpose}

\title{Image Transposition}

\description{
  Transposes an image by swapping its spatial dimensions.
}

\usage{
transpose(x)
}

\arguments{
  \item{x}{an \code{Image} object or an array.}
}

\value{
  A transformed version of \code{x} with its first two dimensions transposed.
}

\details{
The transposition of an image is performed by swapping the X and Y indices of its array representation.
}

\note{
The function is implemented using an efficient cash-oblivious algorithm which is typically faster than R's \code{aperm} and \code{t} functions.
}

\author{
  Andrzej Oles, \email{andrzej.oles@embl.de}, 2012-2017
}

\seealso{
  \code{\link{flip}}, \code{\link{flop}}, \code{\link{rotate}}
}

\examples{
  x = readImage(system.file("images", "sample-color.png", package="EBImage"))
  y = transpose(x)
  
  display(x, title='Original')
  display(y, title='Transposed')

  ## performing the transposition of an image twice should result in the original image
  z = transpose(y)
  identical(x, z)
}
