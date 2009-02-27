\name{readImage}

\alias{readImage}
\alias{writeImage}

\concept{image IO}

\title{Image I/O}

\description{
  Functions to read and write images from/to files and URL's.
  The supported image formats depend on the capabilities of ImageMagick.
}

\usage{
  readImage(files, colormode)
  writeImage(x, files, quality = 100)
}

\arguments{
  \item{files}{A character vector of file names or URLs. If missing, an
  interactive file chooser is displayed.}
  \item{x}{An \code{\linkS4class{Image}} object or an array.} 
  \item{quality}{A numeric, ranging from 1 to 100. Default is 100.} 
  \item{colormode}{Deprecated.}
}

\value{
  For \code{readImage}, a new instance of \code{\linkS4class{Image}}.
  \code{writeImage} returns \code{invisible(files)}.
}

\details{
  When writing images in formats supporting lossy compression (like JPEG),
  the quality can be spcified used a \code{quality} value in the
  range \code{[1,100]}. The best quality is obtained with 100.

  The file format is deduced from the file name extension(s), there is neither
  a need nor a way to specify the format otherwise.

  \code{ImageMagick} is used to perform all image I/O operations. Therefore,
  the package supports all the file types supported by \code{ImageMagick}.

  When reading images, files of different formats can be mixed in any
  sequence, including mixing single 2D images with TIFF image stacks.
  The result will contain a stack with all images and stacks,
  at the size of the first image read. Subsequent images are cropped (if
  larger) or filled with background (if smaller).
}


\references{
    ImageMagick: \url{http://www.imagemagick.org}
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2005-2006
}

\examples{
 ## Reads and display images
 if(interactive()) {
    lena = readImage(system.file("images", "lena-color.png", package="EBImage"))
    display(lena)
    im = readImage("http://www.google.com/intl/en/images/logo.gif")
    display(im)
 }

 ## Converts a TIFF file into JPEG
 f1 = system.file("images", "Gene1_G.tif", package="EBImage")
 f2 = paste(tempfile(), "jpeg", sep=".")
 ii = readImage(f1, colormode=Color)
 writeImage(ii, f2)

}

\keyword{file}

