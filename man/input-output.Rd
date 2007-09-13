\name{readImage}

\alias{chooseImage}
\alias{readImage}
\alias{writeImage}
\alias{writeImage,Image-method}

\concept{image IO}

\title{ Image I/O }

\description{
  Functions to choose, read and write images from/to files and URL's.
  Supported image formats are determined by the ImageMagick installation.
}

\usage{
  chooseImage(colormode = Grayscale)
  readImage(files, colormode = Grayscale, ...)

  \S4method{writeImage}{Image}(x, files, ...)
}

\arguments{

  \item{x}{An object of \code{\linkS4class{Image}}. }

  \item{files}{ A character vector of files/URL's to read from or to write to. }

  \item{colormode}{ An integer value for the color mode of images after they are
    read. By default all images will be converted to Grayscale on read. }

  \item{...}{ Reserved. }

}

\value{

  For \code{readImage} and \code{chooseImage} a new instance of 
  \code{\linkS4class{Image}}.

  For \code{writeImage} an invisible \code{NULL}.
}

\details{

  If \code{files} is missing when writing images, \code{\link{fileName}(x)}
  method will be used to select a single file to write to. Otherwise,
  the length of this vector must be equal either 1 or the number of 2D images
  in the stack.

  When writing images in formats supporting non-lossless compression (like JPEG),
  the quality can be spcified used a numeric \code{quality} argument with the
  range \code{[1,100]}. Higher values correspond to better quality. Defaults to 95.

  The file format is deduced from the file name extension(s), there is neither
  a need nor a way to specify the format explicitly.

  \code{ImageMagick} is used to perform all image I/O operations. Therefore,
  the package supports all the file types supported by \code{ImageMagick}.

  When reading images, files of different formats can be mixed in any
  consequence, including mixing single 2D images with TIFF image stacks.
  The result will contain a stack of all images and stacks cropped (filled
  with background if images are smaller) at the size of the first image read.

  \code{choose.image} is an interactive function that does not return to R
  until either Ok or Cancel button is pressed in the GUI dialog. It uses
  \code{GTK+2} File Open Dialog to select images. Multiple images can be
  selected and loaded at once. By default this functions reads images as
  \code{\link{TrueColor}}. This function will produce an error message if
  the package was compiled without \code{GTK+} support.

}

\seealso{
  \code{
    \linkS4class{Image}, \link{Image}, \link{channel}
  }
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2005-2006
}

\examples{
    f <- paste( system.file(package="EBImage"), "images/Gene1_G.tif", sep="/" )
    ii = readImage(f)
  if ( interactive() ) {
    url <- c("http://www.google.com/intl/en/images/logo.gif")
    im <- readImage (url, TrueColor)
    \dontrun{writeImage (im, "googlelogo.tif")}
    \dontrun{im1 <- channel ( chooseImage(), "gray")}
  }
}

\keyword{file}

