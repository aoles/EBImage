\name{io}

\alias{readImage}
\alias{writeImage}

\concept{image IO}

\title{Image I/O}

\description{
  Read and write images from/to files and URLs.
}

\usage{
readImage(files, type, all = TRUE, \dots)
writeImage(x, files, type, quality = 100, bits.per.sample, compression = "none", \dots)
}

\arguments{
  \item{files}{a character vector of file names or URLs.}
  \item{type}{image type (optional). Supported values are: \code{jpeg}, \code{png}, and \code{tiff}. If missing, file format is automatically determined by filename extension.}
  \item{all}{logical: when the file contains more than one image should all frames be read, or only the first one?}
  \item{x}{an \code{Image} object or an array.} 
  \item{bits.per.sample}{a numeric scalar specifying the number of bits per sample (only for \code{tiff} files). Supported values are 8 and 16.}
  \item{compression}{the desired compression algorithm (only for \code{tiff} files). For a list of supported values consult the documentation of the \code{\link{writeTIFF}} function from the \pkg{tiff} package.}
  \item{quality}{a numeric ranging from 1 to 100 (default) controlling the quality of the JPEG output.}
  \item{\dots}{arguments passed to the corresponding functions from the \pkg{jpeg}, \pkg{png}, and \pkg{tiff} packages.}
}

\value{
  \code{readImage} returns a new \code{Image} object.

  \code{writeImage} returns an invisible vector of filenames.
}

\details{
\code{readImage} loads all images from the \code{files} vector and returns them stacked into a single \code{Image} object containing an array of doubles ranging from 0 (black) to 1 (white). All images need to be of the same \code{type} and have the same dimensions and color mode. If \code{type} is missing, the appropriate file format is determined from filename extension. Color mode is determined automatically based on the number of channels. When the function fails to read an image it skips to the next element of the \code{files} vector issuing a warning message. Non-local files can be read directly from a valid URL.

  \code{writeImage} writes images into files specified in \code{files}, were the number of \code{files} needs to be equal 1 or the number of frames. Given an image containing multiple frames and a single filename either the whole stack is written into a single TIFF file, or each frame is saved to an individual JPEG/PNG file (for \code{files = "image.*"} frames are saved into \code{image-X.*} files, where \code{X} is the frame number less one; for an image containing \code{n} frames this results in filenames numbered from 0 to \code{n-1}). 

  When writing JPEG files the compression quality can be specified using \code{quality}. Valid values range from 100 (highest quality) to 1 (lowest quality). For TIFF files additional information about the desired number of bits per sample (\code{bits.per.sample}) and the compression algorithm (\code{compression}) can be provided. For a complete list of supported values please consult the documentation of the \pkg{tiff} package.
}

\note{
  Image formats have a limited dynamic range (e.g. JPEG: 8 bit, TIFF: 16 bit) and \code{writeImage} may cause some loss of accuracy. In specific, writing 16 bit image data to formats other than TIFF will strip the 8 LSB. When writing TIFF files a dynamic range check is performed and an appropriate value of \code{bits.per.sample} is set automatically.
}

\seealso{
  \code{\link{Image}}, \code{\link{display}}, \code{\link{readJPEG}}/\code{\link{writeJPEG}},  \code{\link{readPNG}}/\code{\link{writePNG}}, \code{\link{readTIFF}}/\code{\link{writeTIFF}}
}

\author{
  Andrzej Oles, \email{andrzej.oles@embl.de}, 2012
}

\examples{
  ## Read and display an image
  f = system.file("images", "lena-color.png", package="EBImage")
  x = readImage(f)
  if (interactive()) display(x)

  ## Read and display a multi-frame TIFF
  y = readImage(system.file("images", "nuclei.tif", package="EBImage"))
  if (interactive()) display(y)

  ## Read an image directly from a remote location by specifying its URL
  try({
    im = readImage("http://www-huber.embl.de/EBImage/ExampleImages/berlin.tif")
    if (interactive()) display(im, title = "Berlin Impressions")
  })

  ## Convert a PNG file into JPEG
  tempfile = tempfile("", , ".jpeg")
  writeImage(x, tempfile, quality = 85)
  cat("Converted '", f, "' into '", tempfile, "'.\n", sep="")

  ## Save a frame sequence
  files = writeImage(y, tempfile("", , ".jpeg"), quality = 85)
  cat("Files created: ", files, sep="\n")
}


