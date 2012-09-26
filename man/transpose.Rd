\name{transpose}

\alias{transpose}

\title{Image Transposition}

\description{
  Transposes an image by swapping its first two, i.e., spatial dimensions.
}

\usage{
  transpose(x, coerce = FALSE)
}

\arguments{
  \item{x}{an \code{Image} object or an array.}
  \item{coerce}{controls the coercion of \code{x}. By default the output is of the same class as the input. If \code{coerce = TRUE} then \code{x} becomes coerced to an array.}
}

\value{
  An \code{Image} object or an array, containing \code{x} with its XY dimensions transposed. When \code{coerce = TRUE} the output is coerced to an array.
}

\details{
The transposition of an image is performed by swapping the X and Y indices of its array representation.
}

\note{
\code{transpose} is particularly useful when converting between different representations of image data in which the X and Y dimensions are swapped. Typically, in such context only the actual pixel data matters. For performance reasons it is best practice to issue the function directly on an \code{Image} object with \code{coerce = TRUE} rather than to extract its image data first and only then perform the transposition, or to transpose the \code{Image} object and coerce it to an array afterwards.
}

\author{
  Andrzej Oles, \email{andrzej.oles@gmail.com}, 2012
}

\seealso{
  \code{\link{flip}}, \code{\link{flop}}, \code{\link{rotate}}
}

\examples{
  x = readImage(system.file("images", "lena-color.png", package="EBImage"))
  y = transpose(x)
  if (interactive()) {
    display(x, title='Lena')
    display(y, title='Transposed Lena')
  }

  ## performing the transposition of an image twice should result in the original image
  z = transpose(y)
  identical(x, z)
}
