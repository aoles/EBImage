\name{zernike.moments}

\alias{zernike.moments}

\concept{feature extraction}

\title{ Extraction of Zernike moments from images of indexed objects }

\description{
  
}

\section{ Usage }{
  \preformatted{

  ## S4 method for x='IndexedImage', ref='Image'

  zernike.moments(x, ref, N = 12, R = 30, apply.Gaussian=TRUE)
}}

\section{ Arguments }{ \describe{

  \item{\code{x}}{An object of \code{\link{IndexedImage-class}}.}
  
  \item{\code{ref}}{An object of \code{\link{Image-class}} in the Grayscale 
    mode.}

  \item{\code{N}}{Integer value defining the degree of the Zernike polynomials, 
    which in turn defines the number of features calculated. Defaults to 12.}
    
  \item{\code{R}}{ Defines the radius of the circle around an object centre 
    from which the features are calculated. It also defines the standard deviation 
    for the 2D Gausian applied at the centre of an object. See details. 
    Defaults to 30. }
    
  \item{\code{apply.Gaussian}}{A logical value that specifies if a local 
    2D Gaussian modification should be applied to every object at its centre. 
    Defaults to \code{TRUE}.}
}}

\section{ Value }{
  For a single frame, a matrix of descriptors with objects in rows and features
  in columns. For image stacks, a list of such matrices. Frames with no objects
  will result in 0-populated a matrix for 1 object. 
}

\section{ Details }{
  Zernike features are calculated as follows:
  
  \code{Z_nl = (n+1) / pi * ABS( sum_x,y(V*nl(x,y) * i(x,y)) )},
  
  \code{0 <= l <= n, n - l} is even and \code{i(x,y)} is the intensity of the
  reference image at the corrdinates \code{(x,y)} that fall withing a circle of 
  radius \code{R} from the object's centre. Coordinates taken relative to the 
  object's centre. The \code{ABS} of the sum gives a real value of the complex 
  feature and makes it rotation invariant.
  
  \code{V*nl} is a complex conjugate of a Zernike polynomial of degree \code{n} 
  and angular dependence \code{l}:
  
  \code{Vnl(x,y) = Qnl(x,y) * exp(j*l*theta)}, where \code{j = sqrt(-1), 
  theta=atan2(y,x)}, and
  
  \code{Qnl(x,y) = sum[_m=0^((n-l)/2)] ((-1)^m * (n-m)! * r^(n-2*m)) / 
  (m! * ((n-2*m+l)/2)! * ((n-2*m-l)/2)!) }, where \code{r = sqrt(x^2+y^2)}.

  The extracted features all carry a \code{"z"}-prefix, for Zernike, and have
  the indexing of the form \code{z.0402} where 04 in this is for \code{n=4} 
  and 02 for \code{l=2}. The number of differnt \code{l}-values is calculated 
  automatically from \code{N}. For a given \code{N} all
  combinations of \code{(n,l)} are calculated. For the default \code{N = 12}
  the resulting number of features is 49. Columns in the results matrix are 
  firstly ordered by increasing value of \code{n} and secondly by increasing 
  value of \code{l}.

  If \code{apply.Gaussian = TRUE} then prior to calculating the Zernike features, 
  a Gaussian is applied to the image centred at the co-ordinates of an objects 
  centre of mass, with the standard deviation defined by \code{sigma = 0.8 * R}. 
  The centres of mass are found by using \code{\link{moments(x, ref)}}.
  The resulting image is then:
  
  \code{i'(x,y) = i(x,y) * exp( -(x^2+y^2) / (2 * sigma^2) )}.
  
  The expected effect: the default value of \code{R = 30} and 
  \code{sigma = 0.8 * R = 24} will penalize the edges of objects of 
  radii more than 20 and intensity is expected to be around 0 at distances from
  the centre more than 40.
  
}

\section{references}{
  F. Zernike. \emph{Beugungstheorie des Schneidenverfahrens und seiner verbesserten 
  Form, der Phasenkontrastmethode (Diffraction theory of the cut procedure and 
  its improved form, the phase contrast method)}. Physica, 1:pp. 689-704, 1934.
  
  Jamie Shutler, \emph{Complex Zernike Moments}: 
  \url{http://homepages.inf.ed.ac.uk/rbf/CVonline/LOCAL_COPIES/SHUTLER3/node11.html}
}

\seealso{\code{
  \link{IndexedImage-class}, \link{watershed}, \link{getFeatures}, 
  \link{moments}
}}



\author{
  Oleg Sklyar <\email{osklyar@ebi.ac.uk}>, Mike Smith <\email{msmith@ebi.ac.uk}>, 2007
}

\keyword{file}

