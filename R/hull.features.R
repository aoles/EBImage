# Extraction of hull and edge features from indexed images

# Copyright (c) 2007 Oleg Sklyar

# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation; either version 2.1
# of the License, or (at your option) any later version.

# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# See the GNU Lesser General Public License for more details.
# LGPL license wording: http://www.gnu.org/licenses/lgpl.html

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("hull.features", signature(x="IndexedImage"),
  function(x, ...) {
    if ( colorMode(x) != Grayscale )
      .stop( "'x' must be Grayscale" )
    ## get basic hull features
    res <- .DoCall( "lib_basic_hull", x )
    if ( is.null(res) ) return( NULL )
    ## get central moments
    moms <- function(m) {
      ## normalized central moments (to size)
      m11 <- m[2,2,] / m[1,1,]
      m02 <- m[1,3,] / m[1,1,]
      m20 <- m[3,1,] / m[1,1,]
      ## semimaj/semiminor = sqrt(eigenvalues) of the covariance matrix
      t1 <- m20 + m02
      t2 <- sqrt( 4 * m11^2 + (m20 - m02)^2 )
      eig1 <- 0.5 * (t1 + t2)
      smaj <- sqrt( eig1 )
      eig2 <- 0.5 * (t1 - t2)
      theta <- 0.5 * atan2( 2 * m11, m20 - m02 )
      ## second division to m[1,1,] to allow for mxx/m00^2 for scale invariants
      m11 <- m11 / m[1,1,]
      m02 <- m02 / m[1,1,]
      m20 <- m20 / m[1,1,]
      I1 <- m20 + m02
      I2 <- (m20 - m02)^2 + 4 * m11^2
      matrix( c(theta, 2*smaj, 2*sqrt(eig2), sqrt(eig1-eig2)/smaj, I1, I2), nrow=dim(m)[3], ncol=6 )
    }
    resm <- smoments(x, pw=2, what="c")
    if ( is.matrix(res) ) res <- cbind( res, moms(resm) )
    else for ( i in seq_along(res) ) res[[i]] <- cbind( res[[i]], moms(resm[[i]]) )
    cn <- c("h.x", "h.y", "h.s", "h.p", "h.pdm", "h.pdsd", "h.effr", "h.acirc", "h.sf", 
            "h.edge", "h.theta", "h.s2maj", "h.s2min", "h.ecc", "h.I1", "h.I2")
    if ( is.matrix(res) ) colnames(res) <- cn
    else res <- lapply(res, function(x) { if ( is.matrix(x) ) colnames(x) <- cn; x } )
    return( res )
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.edge.profile <- function(x, ref=NULL, n=32, fft=FALSE, scale=TRUE, rotate=TRUE, ...) {
  if ( colorMode(x) != Grayscale ) .stop( "'x' must be Grayscale" )
  .dim <- dim(x)
  ## get centres of objects xy, rotation angle theta and if required effective
  ## radius for scaling, s. If a reference image is provided, then xy and t are
  ## affected by the intensity distribution (as per moments)
  if ( .dim[3] == 1 ) {
    if ( is.null(ref) ) xyt <- moments(x)[, c(3,4,8)]
    else xyt <- moments(x, ref)[, c(3,4,8)]
    if ( scale ) s <- hull.features(x)[,7] else s <- NULL
  }
  else {
    if ( is.null(ref) ) xyt <- lapply(moments(x), function(x) x[,c(3,4,8)] )
    else xyt <- lapply(moments(x, ref), function(x) x[,c(3,4,8)] )
    if ( scale ) s <- lapply(hull.features(x), function(x) x[,7] )
    else s <- vector("list", length(xyt))
  }
  ## returns for each image a matrix of border points with for each
  ## object index in the first col, distance from the centre in the second
  ## and theta in third
  res <- .DoCall("lib_edge_profile", x, xyt)
  ## profile will be calculated at these angle coordinates (-2Pi,+2Pi)
  xout <- (2*(1:n)/(n+1) - 1) * pi
  ## this function will compose a profile matrix from the above matrix
  do.profile <- function(m, xyt, s) {
    ## object indexes for each record
    index <- m[,1]
    ## split by object indexes and sort by object index acsending
    m <- split(as.data.frame(m[,2:3]), index)
    m <- m[ order(as.integer(names(m))) ]
    ## shift theta and scale d if rotation and scaling required
    if (rotate || scale) 
      for (i in seq_along(m)) {
        mi <- m[[i]]
        changes <- FALSE
        if ( rotate ) {
          mi[,2] <- mi[,2] - xyt[i,3]
          index <- which( mi[,2] < -pi )
          if ( length(index) > 0 ) {
            mi[index,2] <- mi[index,2] + 2 * pi
            changes <- TRUE
          }
        }
        if ( scale ) {
          mi[,1] <- mi[,1] / s[i]
          changes <- TRUE
        }
        if (changes) m[[i]] <- mi
      }
    ## approximate the data sets for each object by approx at xout, apply fft, return
    if ( fft )
      return( matrix( unlist( lapply(m, function(x) abs(fft(approx(x[,2], x[,1], 
        xout=xout, n=n)$y)) ) ), ncol=n, nrow=length(m), byrow=TRUE) )
    matrix( unlist( lapply(m, function(x) approx(x[,2], x[,1], xout=xout, n=n)$y) ),
      ncol=n, nrow=length(m), byrow=TRUE)
  }
  ## run the above function for all images    
  if ( .dim[3] == 1 ) res <- do.profile(res, xyt, s)
  else for (i in seq_along(res)) res[[i]] <- do.profile(res[[i]], xyt[[i]], s[[i]])
  res
}

setMethod ("edge.profile", signature(x="IndexedImage", ref="missing"),
  function (x, ref, n=32, fft=TRUE, scale=TRUE, rotate=TRUE, ...) {
    .edge.profile(x, NULL, n, fft, scale, rotate, ...)
  }
)

setMethod ("edge.profile", signature(x="IndexedImage", ref="Image"),
  function (x, ref, n=32, fft=TRUE, scale=TRUE, rotate=TRUE, ...) {
    .edge.profile(x, ref, n, fft, scale, rotate, ...)
  }
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod ("edge.features", signature(x="IndexedImage"),
  function (x, ref, ...) {
    .dim <- dim(x)
    res <- if ( missing(ref) ) edge.profile(x, n=16, fft=FALSE, scale=TRUE, rotate=TRUE)
    else edge.profile(x, ref, n=16, fft=FALSE, scale=TRUE, rotate=TRUE)
    do.profile <- function(e) {
      m <- matrix(0, ncol=5, nrow=nrow(e))
      colnames(m) <- c("e.irr", "e.f2Pi", "e.fPi", "e.fPi2", "e.fPi4")
      m[,1] <- apply(e, 1, max, na.rm=TRUE) - apply(e, 1, min, na.rm=TRUE)
      m[,2:5] <- t(apply(e, 1, function(x) { x[which(is.na(x))]=median(x,na.rm=TRUE); abs(fft(x))[1:4] } ))
      return( m )
    }
    if ( dim(x)[3] == 1 ) return( do.profile(res) )
    return( lapply(res, do.profile) )
  }
)

