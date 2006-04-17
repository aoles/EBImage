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
  Read and write operations for images from/to files and connections.
% FIXME:
%   is this correction, can "files" be a connection, or does it need to
%   be a character with local filename or URL? Also, URL would only
%   work for reading but not writing, right?
%   Also supported read from URLs.
   The supported image formats are those of your local
    installation of ImageMagick.
}

\usage{
    read.image(files, rgb = FALSE)
    write.image(object, files)
    ping.image(files, show.comments = FALSE)
}

\arguments{
    \item{files}{A character vector of file names to be read or
      written. For read operations, this can also be an URL.}
    \item{rgb}{A logical value specifying if the image(s) read are to be stored as RGB or
        grayscales. A conversion is made automatically if necessary.}
    \item{object}{A 2D or 3D image to be saved.}
    \item{show.comments}{A logical value telling the ping routine if image comments should be displayed: output can be lengthy.}
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
and the bottom.
% FIXME: I think for our type of applications it would also be good to
% be able to emit warnings or an error if the images have different sizes (because
% in an HT screen, we expect them all to be the same size)
Therefore it can be useful to load the largest image first.
The conversion to RGB or grayscale is made on the stage of moving data from ImageMagick structures
% FIXME: Image is currently not a defined class, Image2D and Image3D
% (but as mentioned somewhere else, I find them awkward and would find
% simply a class "Image" better anyway).
to the instance of \code{Image} utlising Magick++ fast direct memory-copy routines.

When writing images, \code{files} can be of length 1, in this
case if the image is 3D, the target format must support stacks (e.g. TIFF), or \code{files} must
be of the same length as the third dimension of \code{object}.
% FIXME:
% This is weird, because you don't talk about objects with more than 3
% dimensions anywhere else (although I don't see why we shouldn't allow
% them, they might well be useful!)
If the image has higher dimensionality,
use subset operators to save parts of the image with this function.
}

\seealso{
   \code{\link{Image2D}}, \code{\link{Image3D}}
}

%\source{
%    R sources in 'R/class.Image2D.R' and C++ sources in 'inst/src/io.cpp' of package 'imagine'.
%}

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

