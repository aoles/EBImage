\name{updateEBImage}

\alias{updateEBImage}
\alias{updateEBImageDev}

\docType{function}

\title{Functions to update EBImage}

\description{
    Two functions to update to the latest version of EBImage. Whereas #
    \code{updateEBImageDev} allows to update to the latest development version at
    BioC development branch, \code{updateEBImage} provides a possibility to
    update to the latest stable package release between two BioC releases. It is
    not a development version, but a stable version of the package, which however
    carries new functionality or/and bug fixes.
}

\usage{
    updateEBImage()
    updateEBImageDev()
}
\details{
    \code{updateEBImageDev} uses tarballs from \url{http://www.bioconductor.org/packages/1.9/bioc} (the development version of BioC at the time of writing this documentation).
    
    \code{updateEBImage} uses tarballs from \url{http://www.ebi.ac.uk/huber-srv/data/Rrepos}
    which contains the latest stable package version independently on the BioC release.
    
    To read help on the latest stable version, please visit the package home page 
    \url{http://www.ebi.ac.uk/~osklyar/projects/EBImage}.
}

\value{
    Same as of \code{install.packages}.
}

\author{
    Copyright (c) 2006 Oleg Sklyar : \email{osklyar@ebi.ac.uk}   
}

\keyword{manip}

