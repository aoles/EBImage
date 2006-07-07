\name{EBImage-package}

\alias{EBImage-package}
\alias{EBImage}

\docType{package}

\title{EBImage package overview and changelog}

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
    thresholding (\code{\link{thresh}}), object detection by a modified watershed algorithm 
    (\code{\link{ws}}), mathematical image morphology with \code{\link{mErode}}, 
    ImageMagick \code{\link{Drawable}}s etc.

    Images in different formats can be loaded and saved with \code{\link{read.image}} and
    \code{\link{write.image}}.

    To view results of image processing on the screen, \code{\link{display}} function can be used.
}

\section{Changelog}{
    \describe{
        \item{\code{06.07.2006:} }{Added morphological operations (erosion, dilation, opening, closing) (\code{\link{Morphology}})}
        \item{\code{05.07.2006:} }{\code{wsImages} added to generate image stack of detected 
            objects/per image. \code{wsPaint} added to paint results of watershed object detection
            (\code{\link{ws}}). Color conversions finished enabling converting colors between three
            different representaions - double intensity values, integer RGB's and X11 strings}
        \item{\code{29.06.2006:} }{Added watershed object detection: \code{\link{ws}}}
        \item{\code{17.06.2006:} }{Added first full vignette!}
        \item{\code{02.05.2006:} }{Added \code{\link{plot.image}} to display images on 
            machines where \code{\link{display}} fails}
        \item{\code{29.04.2006:} }{Added INSTALL file with installation descriptions for Mac etc}
        \item{\code{25.04.2006:} }{Major restructuring deprecating Image2D, Image3D in favor 
            of \code{\link{Image}}}
        \item{\code{24.04.2006:} }{EBImage first time in the Bioconductor release!}
        \item{\code{31.03.2006:} }{Uses autoconf to generate configure script}
    }
}

\author{
    Copyright (c) 2005-2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}. Parts of 
    code by Wolfgang Huber : \email{huber@ebi.ac.uk}
    
    European Bioinformatics Institute, Wellcome Trust Genome Campus, 
    Hinxton, Cambs, CB10 1SD, England

    Released under \code{LGPL}:

    \code{This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public License 
    as published by the Free Software Foundation; either version 2.1
    of the License, or (at your option) any later version.}

    \code{This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.}

    \code{See the GNU Lesser General Public License for more details.
    LGPL license wording: http://www.gnu.org/licenses/lgpl.html}
}
\keyword{package}
