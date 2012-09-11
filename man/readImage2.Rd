\name{readImage2}

\alias{readImage2}

\title{Function to Read Images}
\description{
Read images from files and URLs. Supports TIFF, PNG, and JPEG file formats.
}
\usage{
readImage2(files, type)
}
\arguments{
  \item{files}{a character vector of file names or URLs}
  \item{type}{optional file format specification, which can be either 'tiff', 'png', or 'jpeg'. If missing, file format will be automatically deduced from filename extensions}
}

\details{
Loads all images from the given list and returns them stacked in a single \code{Image} object, provided that the individual image's dimensions match.
The proper \code{colormode} is determined automatically.
}
\value{a new \code{Image} object}

\author{Andrzej Oles, \email{andrzej.oles@gmail.com}, 2012}

\examples{

   ## RGB image
   x = readImage2(system.file("images", "lena-color.png", package="EBImage"))
   display(x)

   ## stacked Grayscale TIFF
   y = readImage2(system.file("images", "nuclei.tif", package="EBImage"))
   display(y)
}