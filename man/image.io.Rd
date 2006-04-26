\name{Image IO}
\alias{Image IO}

\alias{read.image}
\alias{write.image}
\alias{ping.image}

\concept{image input output}

\title{
    Routines to read and write images
}

\description{
    Read and write operations for images from/to files and URLs (for URLs write to anonymous FTP only).
    Supported image formats are determined by your local installation of ImageMagick.
}

\usage{
    read.image(files, rgb = FALSE)
    write.image(object, files)
    ping.image(files, show.comments = FALSE)
}

\arguments{
    \item{files}{A character vector of files/URLs to be read or written. For writing to URLs, only
    anonymous FTP is supported.}
    \item{rgb}{A logical value specifying if the image(s) read are to be stored as RGB or
        grayscales. A conversion is made automatically if necessary.}
    \item{object}{An image to be saved.}
    \item{show.comments}{A logical value telling the ping routine if detailed image comments should be
        displayed: output can be lengthy.}
}

\value{
    For \code{read.image} a new instance of class Image.

    For \code{write.image} an invisible logical, \code{TRUE} if write was successful,
    \code{FALSE} otherwise.

    For \code{ping.image} an invisible NULL.
}

\details{
    \code{ping.image} enables retrieving image information without loading
    the image. Image summaries such as size, density, compression type and
    quality are printed to the standard output.

    The file format is deduced from the file name extension(s), there is no
    need to specify the format explicitely.

    When reading images, files of different formats can be mixed in any sequence, including
    mixing single 2D images with TIFF etc image stacks. First all files will be read one-by-one
    into a stack, pushing new images to the back of the stack. Then a new instance of class
    Image will be created with the size of the first 2D image in the stack and the third dimension
    set to the number of images. When the data is moved to this new R structure, larger images
    will be cropped and smaller ones will be padded with zeroes on the right
    and the bottom. Therefore it can be useful to load the largest image first.
    The conversion to RGB or grayscale is made on the stage of moving data from ImageMagick structures
    to the instance of \code{Image} direct memory copy routines.

    When writing images, \code{files} can be of length 1, in this
    case if the image is 3D, the target format must support stacks (e.g. TIFF). Otherwise
    \code{files} must be of the same length as the third dimension of the image.
}

\seealso{
   \code{\link{Image}}
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}

\examples{
    urls <- c("http://www.ebi.ac.uk/~osklyar/projects/EBImage/examples/example.tif", "http://www.ebi.ac.uk/~osklyar/projects/EBImage/examples/segmented.tif")
    ping.image(urls)
    im <- read.image(urls)
    write.image(im, "image.io.Rd.example.tif")
}

\keyword{file}

