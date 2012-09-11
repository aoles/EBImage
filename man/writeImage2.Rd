\name{writeImage2}

\alias{writeImage2}

\title{Function to Write Images}

\description{
Save \code{Image} object as TIFF, PNG, and JPEG files.
}
\usage{
writeImage2(img, files, quality = 100, ...)
}
\arguments{
  \item{img}{ \code{Image} to write}
  \item{files}{ a single filename or a character vector of file names for stacked multi-frame images }
  \item{quality}{JPEG quality; a numeric ranging from 1 to 100  (default value is 100) }
  \item{\dots}{ additional parameters passed to the \code{writeTIFF} function (see documentation of the \code{tiff} package)}
}

\author{Andrzej Oles, \email{andrzej.oles@gmail.com}, 2012}

\examples{
   ## load stacked TIFF image
   x = readImage2(system.file("images", "nuclei.tif", package="EBImage"))

   ## save individual frames as JPEGs
   writeImage2(x, c("nuclei.1.jpg","nuclei.2.jpg", "nuclei.3.jpg", "nuclei.4.jpg"), quality=75)
}