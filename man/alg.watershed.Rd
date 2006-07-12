\name{wsObjects}

\alias{wsObjects}
\alias{wsPaint}
\alias{wsImages}

\docType{function}

\title{Object detection and marking with watershed algorithm}

\description{
    Modified watershed algorithm is used in \code{wsObjects} to identify objects in an 
    image. Supplying the results of \code{wsObjects} along with a reference image to
    \code{wsPaint} allows to obtain an image with all detected objects marked
    in different colors for quality control. Supplying the results of \code{wsObjects}
    along with a reference image to \code{wsImages} will generate an image stack
    with a single detected object per image positioned in the centre of the image.
    This can be used further for pattern recognition etc.
}

\usage{
    wsObjects(x, mind=15, minr=10, ef=0.2, seeds=NULL, ref=NULL)
    wsPaint(wsres, ref, opac=0.2, col="default", fill=TRUE, brds=TRUE)
    wsImages(wsres, ref)
}
\arguments{
    \item{x}{An instance of class \code{\link{Image}}, grayscale - a distance map!}
    \item{ref}{Reference image, grayscale for \code{wsObjects}, any otherwise}
    \item{mind}{Minimum distance between objects, if two identified objects are 
        closer, they are combined into one}
    \item{minr}{Minimum distance (radius) from the object centre to the background (minimum
        value on the distance map to start an object)}
    \item{ef}{Edge factor - if the ratio of image-edge pixels to perimeter pixels for any
        particular object is larger than \code{ef}, this object is deleted (or combined with
        another one, which is close enough and such that the combined ratio is less than \code{ef}}
    \item{seeds}{A matrix (if \code{dim(x)[[2]] == 1} or a list of matrices of 2 columns and 
        rows being the number of objects, where the object detection can only start from. If 
        this value is given, no new object will be created, but some can still be combined
        together if they are too close)}
    \item{wsres}{A list as returned by \code{wsObjects}. See description of the values below.}
    \item{opac}{A numeric value for the opacity of painted objects on the reference image. 
        The reference image will correspondingly have the opacity equal 1 - opac. Values must
        be between 0 and 1 (both excluding).}
    \item{col}{A character vector of base colors to use the color ramp on specifying the 
        fill color for the objects. Objects are colored in the order of their detection (as in
        the argument \code{wsres}). \code{"default"} uses 5 colors from the Set3 pallet of the
        ColorBrewer (EBImage does not depend on ColorBrewer). Fill color will be derived from
        base colors by scaling it with \code{opac}. Stroke colors are the same with the opacity 
        of 1}
    \item{fill}{Logical, whether objects need to be filled with color}
    \item{brds}{Logical, whether objects need to be outlined}
}

\value{
    \code{wsObjects} returns a named list with elements {objects, pixels, borders} for 
    single images or a list of such lists for image stacks. Item \code{objects} is a 
    matrix with rows specifying detected objects and columns - their properties.
    Cols 1 and 2 - x,y coordinates of the object geometrical center on the image;
    col 3 - object size in pixels, col 4 - total intensity of the reference image for
    the given object (sum of pixel values); col 5 - number of perimeter pixels (pixels 
    shared by two neighboring objects not included); col 6 - number of image-edge pixels.
    Item \code{pixels} a matrix with the same number of rows as \code{object} and with
    number of columns by the maximum size of objects in the image. Each row is filled
    with indecies (withing one image for stacks) of object pixels or NAs to fill the 
    rest. These values are used by \code{wsPaint} and \code{wsImages} to reconstruct the 
    objects! Item \code{borders} has the same structure but depends on the maximum number
    of perimeter pixels for all objetcs in one image; used by \code{wsPaint} to
    draw outlines.
    
    \code{wsPaint}'s value is of the same type as its \code{ref} argument, but is always
    converted to RGB.
    
    \code{wsImages} returns a stack of images of a size that allows to fit the largest 
    object in an image within it with one object per image. The stack depths is determined
    by the number of objects. If input is image stack, the result is a list of image stacks
    described above. Images will be grayscale or RGB depending on the mode of \code{ref}.
}

\details{
    \code{wsObjects} uses distance maps for its \code{x} arguments. It will either fail
    or in the worst case take forever to generate an inadequate result if any 
    other image is supplied! 
}

\seealso{
   \code{\link{Image}}, \code{\link{distMap}}, \code{\link{thresh}}, \code{\link{mOpen}}
}

\author{
    Copyright (c) 2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\examples{
    # create an empty image of 300x200 size
    im1 <- Image(0, c(300,200,1))
    # fill image with data (waves)
    for (i in 1:dim(im1)[[1]])
        for (j in 1:dim(im1)[[2]]) {
            im1[i,j,1] <- abs(sin(i*0.0314)*sin(i*0.0314) + cos(j*0.0314)*cos(j*0.0314))
        }
    # normalize image
    im1 <- normalize(im1)
    \dontrun{display(im1)}
    # threshold
    seg <- thresh(im1, 20, 20, 0)
    # do opening and closing
    seg <- mClose(mOpen(seg))
    \dontrun{display(seg)}
    # calculate distance map and normalize it
    dm <- distMap(seg)
    # detect objects
    x <- wsObjects(dm, ref = im1)
    # paint original image
    painted <- wsPaint(x, im1)
    \dontrun{display(painted)}
    # get a subset of images-per-object
    imgs <- wsImages(x, im1)
    \dontrun{display(imgs)}
}

\keyword{dplot}
\keyword{manip}
\keyword{array}

