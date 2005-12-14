\name{Image IO}
\alias{Image IO}

\alias{read.image}
\alias{write.image}
\alias{ping.image}

\concept{image input output}

\title{
    Routines to Read and Write 2D and 3D Images
}

\description{
    A set of tree routines (\code{write.image} is implemented as method) to enable
    read/write operations for images. Multiple images and multiple filenames are supported.
    Also supported read from URLs. Image formats are supported those of your local
    installation of ImageMagick.
}

\usage{
    read.image(files, rgb = FALSE)
    write.image(object, files)
    ping.image(files, show.comments = FALSE)
}

\arguments{
    \item{files}{A character vector of file names to be read, written to.}
    \item{rgb}{A logical value specifying if the image(s) read are to be stored as RGB or
        grayscales. A conversion is made automatically if necessary.}
    \item{object}{A 2D or 3D image to be saved.}
    \item{show.comments}{A logical value telling the ping routine if image comments should be displayed: output can be lengthy.}
}

\value{
    For \code{read.image} a new instance of class Image with data of images read
    from file(s) or URL(s).

    For \code{write.image} an invisible logical. TRUE if write was successful,
    FALSE otherwise.

    For \code{ping.image} an invisible NULL.
}

\details{
    \code{ping.image} enables retrieving image information without loading them. Image summary
    consisting of size, density, compression type and quality etc is displayed on \code{stdout}.

    When reading images, files of different formats can be mixed in any sequence, including
    mixing single 2D images with TIFF etc image stacks. First all files will be read one-by-one
    into a stack, pushing new images to the back of the stack. Then a new instance of class
    Image will be created with the size of the first 2D image in the stack and the third dimension
    set to the number of images. When the data is moved to this new R structure, larger images
    will be croped and smaller will have a black border on their right-bottom side. Therefore it
    might be important to specify first the image with the largest size that might be required.
    The conversion to RGB or grayscal is made on the stage of moving data from ImageMagick structures
    to the instance of Image utlising Magick++ fast direct memory-copy routines.

    When writing images, only 2D and 3D images can be saved. \code{files} can be of length 1, in this
    case if the image is 3D, the target format must support stacks (e.g. TIFF), or \code{files} must
    be of the same length as the thrid dimension on the image. If the image has higher dimensionality,
    use subset operators to save parts of the image with this function.
}

\seealso{
   \code{\link{Image2D}}, \code{\link{Image3D}}
}

\source{
    R sources in 'R/class.Image2D.R' and C++ sources in 'inst/src/io.cpp' of package 'imagine'.
}

\references{
    \emph{ImageMagick}: \url{http://www.imagemagick.org}.
}

\author{
    Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}

\examples{
    # FIXME: load image, increase brightness, save image
    \dontrun{im = read.image("test.tif")}
    \dontrun{im = im + 0.1}
    \dontrun{write.image(im, "test1.tif")}
}

\keyword{file}

