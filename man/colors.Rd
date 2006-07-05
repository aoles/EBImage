\name{colRGB}

%\alias{colRGB}
%\alias{colString}

\docType{function}

\title{Color conversion routines}
\description{
%    Routines analogous to \code{\link{rgb}} and \code{\link{col2rgb}},
%    that can however operate with 16-bit color channels, i.e. handling X11
%    color strings in form \verb|#0000FFFF0000| instead of \verb|#00FF00|.
}

\usage{
%    colRGB(col)
%    colString(r, g, b, gray)
}
\arguments{
%    \item{col}{Character vector giving the color string, either in form
%        of 16-bit \verb|#0000FFFF0000| or 8-bit \verb|#00FF00|}
%    \item{r}{Red value between 0 and 1}
%    \item{g}{Green value between 0 and 1}
%    \item{b}{Blue value between 0 and 1}
%    \item{gray}{Gray value between 0 and 1}
}
\details{
    If \code{gray} is specified in call to \code{colString}, values of
    \code{r}, \code{g}, and \code{b} are ommited.
}

\value{
    \code{colRGB} returns a vector of length 3 giving RGB channel values as
    doubles in the range 0..1

    \code{colString} returns a character value in form of 16-bit X11
    color string.
}

\seealso{
   \code{\link{Drawable}}
}

\author{
    Oleg Sklyar, \email{osklyar@ebi.ac.uk}
}

\keyword{dplot}
\keyword{manip}

