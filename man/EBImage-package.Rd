\name{EBImage-package}

\alias{EBImage-package}
\alias{EBImage}

\docType{package}

\title{EBImage package overview}

\description{
   EBImage is an image processing and analysis package for R. The primary goal of the
   package is to enable automated or semiautomated analysis of large sets of images,
   e.g. results of automated microscopy screens.

   Many of EBImage routines are based on ImageMagick library that provides all I/O
   operations as well as some filter routines. The package does not inteface all ImageMagick
   functions and concentrates on those that are essential and usefull for data analysis.
   On top of ImageMagick functions further routines and algorithms are implemented to
   support distance maps, thresholding, object counting etc.

   Images are stored in objects of class \code{\link{Image}}. The class is directly derived from
   \code{\link{array}} and features all properties of those. Image processing and analysis routines
   require objects of class \code{\link{Image}} as input.
}

\details{
    The package provides most of ImageMagick image filters that can be divided into the following
    categories: \code{\link{Segmentation}}, \code{\link{Transformation}},
    \code{\link{Color filters}} and \code{\link{Enhancement}}.

    Further algorithms currently include distance maps (\code{\link{distMap}}), adaptive
    thresholding (\code{\link{thresh}}), feature counting (\code{\link{objectCount}}).

    Images in different formats can be loaded and saved with \code{\link{read.image}} and
    \code{\link{write.image}}.

    To view results of image processing on the screen, \code{\link{display}} function can be used.
}
\author{
    Oleg Sklyar, \email{osklyar@ebi.ac.uk}. 2005-2006. European Bioinformatics Institute. Package
    is a part of Bioconductor release branch since release 1.8.
}
\keyword{package}
