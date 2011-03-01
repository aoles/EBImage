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
  \code{readImage} returns a new \code{Image} object.
  \code{writeImage} returns \code{invisible(files)}.
}

\details{
  When writing images in formats supporting lossy compression (like JPEG),
  the quality can be specified used a \code{quality} value in the
  range \code{[1,100]}. The best quality is obtained with 100.

  The file format is deduced from the file name extension.

  \code{ImageMagick} is used to perform all image I/O operations. Therefore,
  the package supports all the file types supported by \code{ImageMagick}.

  When reading images, files of different formats can be mixed in any
  sequence, including mixing single 2D images with TIFF image stacks.
  The result will contain a stack with all images and stacks,
  at the size of the first image read. Subsequent images are cropped (if
  larger) or filled with background (if smaller).

  \code{readImage} returns an \code{Image} object, containing an array of
  double values ranging from 0 (black) to 1 (white). Image formats have
  a limited dynamic range (e.g. JPEG: 8 bit, TIFF: 16 bit) and
  \code{writeImage} may cause some loss of accuracy.
}


\references{
    ImageMagick: \url{http://www.imagemagick.org}
}

\author{
  Oleg Sklyar, \email{osklyar@ebi.ac.uk}, 2005-2006
}

\examples{
   ## Reads and display images
   f = system.file("images", "lena-color.png", package="EBImage")
   x = readImage(f)
   if (interactive()) display(x)

   x = readImage(system.file("images", "nuclei.tif", package="EBImage"))
   if (interactive()) display(x)
   
   try({
     im = readImage("http://www.google.com/intl/en/images/logo.gif")
     if (interactive()) display(im)
   })

   ## Converts a TIFF file into JPEG
   f1 = system.file("images", "lena-color.png", package="EBImage")
   x1 = readImage(f1)
   f2 = paste(tempfile(), "jpeg", sep=".")
   writeImage(x1, f2)
   cat("Converted '", f1, "' into '", f2, "'.\n", sep='')
}


