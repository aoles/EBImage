# Extraction of zernike features from indexed images

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

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
getFeatures = function (x, ref, N = 12, R = 30, apply.Gaussian, nc = 256, pseudo) {
  validImage(x)
  if ( !missing(ref) && is.Image(ref) && (colorMode(ref) == TrueColor) )
    stop("\'ref\' must be an Image not in \'TrueColor\' color mode")
  if (!missing(apply.Gaussian)) warning("'apply.Gaussian' is deprecated.")
  if (!missing(pseudo))  warning("'pseudo' is deprecated.")
  
  .dim <- dim(x)
  hf <- hullFeatures( x )
  if ( !missing(ref) ) {
    ef <- edgeFeatures( x=x, ref=ref )
    tf <- haralickFeatures(x=x, ref=ref, nc=nc)
    zf <- zernikeMoments(x=x, ref=ref, N=N, R=R, apply.Gaussian=apply.Gaussian, pseudo=pseudo)
    ## mf calculation
    mf <- moments(x=x, ref=ref)
    ## distance from COM to geometric centre
    if ( getNumberOfFrames(x,'total')== 1 )
      mf <- cbind(mf, sqrt((mf[,3,drop=FALSE]-hf[,1])^2 +(mf[,4]-hf[,2])^2))
    else {
      for ( i in seq_along(hf) )
        mf[[i]] <- cbind(mf[[i]], sqrt((mf[[i]][,3,drop=FALSE]-hf[[i]][,1])^2 +
                                       (mf[[i]][,4]-hf[[i]][,2])^2))
    }
    do.moms <- function(m) {
      m <- cbind(m[,2,drop=FALSE], m[,2]/m[,1], m[,18], 2*sqrt(m[,9]),
                 2*sqrt(m[,10]), sqrt((m[,9] - m[,10])/m[,9]), m[,11:17,drop=FALSE])
      m[ which(is.na(m)) ] = 0.0
      colnames(m) <- c("i.int", "i.dens", "i.d", "i.s2maj", "i.s2min", "i.ecc",
                       "i.I1", "i.I2", "i.I3", "i.I4", "i.I5", "i.I6", "i.I7")
      m
    }
    if ( getNumberOfFrames(x,'total') == 1 ) mf <- do.moms(mf)
    else mf <- lapply(mf, do.moms)
  }
  else { # missing ref
    ef <- edge.features( x=x )
  }
  
  if (  getNumberOfFrames(x,'total') == 1 ) {
    if ( !missing(ref) )
      features <- list( cbind(hf, ef, tf, mf, zf) )
    else
      features <- list( cbind(hf, ef) )
  }
  else {
    features <- vector("list", length(hf))
    if ( !missing(ref) )
      for ( i in seq_along(hf) ) features[[i]] <- cbind(hf[[i]], ef[[i]], tf[[i]], mf[[i]], zf[[i]])
    else
      for ( i in seq_along(hf) ) features[[i]] <- cbind(hf[[i]], ef[[i]])
  }
  return(features)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
zernikeMoments = function(x, ref, N=12, R=30, apply.Gaussian, pseudo) {
  checkCompatibleImages(x,ref)
  if (!missing(apply.Gaussian))  warning("'apply.Gaussian' is deprecated.")
  else apply.Gaussian=FALSE
  if (!missing(pseudo))  warning("'pseudo' is deprecated.")
  else pseudo=FALSE
 
  if ( getNumberOfFrames(x,'total') == 1 )
    xy <- moments(x=x, ref=ref)[, c(3,4), drop=FALSE]
  else
    xy <- lapply(moments(x=x, ref=ref), function(x) x[,c(3,4), drop=FALSE] )
  
  if ( !pseudo )
    return( .Call("lib_zernike", castImage(x), castImage(ref), xy, as.numeric(R), as.integer(N), as.integer(apply.Gaussian), PACKAGE='EBImage') )
  else
    return( .Call("lib_pseudo_zernike", castImage(x), castImage(ref), xy, as.numeric(R), as.integer(N), as.integer(apply.Gaussian), PACKAGE='EBImage') )
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
hullFeatures = function(x) {
  validImage(x)
  if ( colorMode(x) == TrueColor ) stop("this method doesn't support the \'TrueColor\' color mode")
  ## get basic hull features
  res <- .Call( "lib_basic_hull", castImage(x), PACKAGE='EBImage')
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
  resm <- smoments(x=x, pw=2, what="c")
  if ( is.matrix(res) ) res <- cbind( res, moms(resm) )
  else for ( i in seq_along(res) ) res[[i]] <- cbind( res[[i]], moms(resm[[i]]) )
  cn <- c("h.x", "h.y", "h.s", "h.p", "h.pdm", "h.pdsd", "h.effr", "h.acirc", "h.sf",
          "h.edge", "h.theta", "h.s2maj", "h.s2min", "h.ecc", "h.I1", "h.I2")
  if ( is.matrix(res) ) colnames(res) <- cn
  else res <- lapply(res, function(x) { if ( is.matrix(x) ) colnames(x) <- cn; x } )
  return( res )
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.edgeProfile <- function(x, ref=NULL, n=32, fft=FALSE, scale=TRUE, rotate=TRUE) {
   checkCompatibleImages(x,ref)
   
  .dim <- dim(x)
  ## get centres of objects xy, rotation angle theta and if required effective
  ## radius for scaling, s. If a reference image is provided, then xy and t are
  ## affected by the intensity distribution (as per moments)
  if ( getNumberOfFrames(x,'total')== 1 ) {
    if ( is.null(ref) ) xyt <- moments(x=x)[, c(3,4,8), drop=FALSE]
    else xyt <- moments(x=x, ref=ref)[, c(3,4,8), drop=FALSE]
    if ( scale ) s <- hullFeatures(x=x)[,7] else s <- NULL
  }
  else {
    if ( is.null(ref) ) xyt <- lapply(moments(x), function(x) x[,c(3,4,8), drop=FALSE] )
    else xyt <- lapply(moments(x=x, ref=ref), function(x) x[,c(3,4,8), drop=FALSE] )
    if ( scale ) s <- lapply(hullFeatures(x=x), function(x) x[,7] )
    else s <- vector("list", length(xyt))
  }
  ## returns for each image a matrix of border points with for each
  ## object index in the first col, distance from the centre in the second
  ## and theta in third
  res <- .Call("lib_edge_profile", castImage(x), xyt, PACKAGE='EBImage')
   
  ## profile will be calculated at these angle coordinates (-2Pi,+2Pi)
  xout <- (2*(0:(n-1))/(n-1) - 1) * pi
  ## this function will compose a profile matrix from the above matrix
  do.profile <- function(m, xyt, s) {
    if ( is.null(m) ) { # no objects return 1 line full of zeros
      warning("Image contains no objects");
      return( matrix(0, ncol=n, nrow=1) )
    }
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
    do.approx <- function(x) {
      if (nrow(x)!=1) {
        y <- approx(x[,2], x[,1], xout=xout, n=n)$y
        y[ which(is.na(y)) ] <- median(y, na.rm=TRUE)
      } else y=rep(0,n)
      y
    }
    if ( fft )
      res <- lapply(m, function(x) abs(fft(do.approx(x))))
    else
      res <- lapply(m, do.approx)
    matrix( unlist(res), ncol=n, nrow=length(m), byrow=TRUE)
  }
  ## run the above function for all images
  if ( getNumberOfFrames(x,'total') == 1 ) res <- do.profile(res, xyt, s)
  else for (i in seq_along(res)) res[[i]] <- do.profile(res[[i]], xyt[[i]], s[[i]])
  res
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
edgeProfile = function (x, ref, n=32, fft=TRUE, scale=TRUE, rotate=TRUE) {
  if (missing(ref)) ref = NULL
  .edgeProfile(x, ref, n, fft, scale, rotate)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
edgeFeatures = function (x, ref) {
  validImage(x)
  .dim <- dim(x)
  if ( missing(ref) ) ref <- NULL
  res <- edgeProfile(x=x, ref=ref, n=16, fft=FALSE, scale=TRUE, rotate=TRUE)
  do.profile <- function(e) {
    m <- matrix(0, ncol=5, nrow=nrow(e))
    colnames(m) <- c("e.irr", "e.f2Pi", "e.fPi", "e.f2Pi3", "e.fPi2")
    m[,1] <- apply(e, 1, max, na.rm=TRUE) - apply(e, 1, min, na.rm=TRUE)
    m[,2:5] <- t(apply(e, 1, function(x) { x[which(is.na(x))]=median(x,na.rm=TRUE); abs(fft(x))[2:5] } ))
    return( m )
  }
  if (getNumberOfFrames(x,'total')==1) return( do.profile(res) )
  return( lapply(res, do.profile) )
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
haralickMatrix = function(x, ref, nc=32) {
  checkCompatibleImages(x,ref)
  rref <- range(ref)
  if ( rref[1] < 0 || rref[2] > 1 ) {
    ref[ref<0] = 0
    ref[ref>1] = 1
    warning( "Values in 'ref' have been limited to the range [0,1]" )
  }
  
  res <- .Call( "lib_co_occurrence", castImage(x), castImage(ref), as.integer(nc), PACKAGE='EBImage')
  return( res )
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
haralickFeatures = function(x, ref, nc=32) {
  validImage(x)
  hm <- haralickMatrix(x=x, ref=ref, nc=nc)
  if ( is.null(hm) || !(is.array(hm) || is.list(hm)) ) return( NULL )
  do.features <- function(m) {
    res <- .Call( "lib_haralick", m, PACKAGE='EBImage')
    if ( is.matrix(res) )
      colnames(res) <- c("t.asm", "t.con", "t.cor", "t.var", "t.idm", "t.sav", "t.sva", 
                         "t.sen", "t.ent", "t.dva", "t.den", "t.f12", "t.f13")
    res
  }
  if ( !is.list(hm) ) return( do.features(hm) )
  lapply( hm, do.features )
}   




