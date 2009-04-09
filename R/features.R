## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
getFeatures = function (x, ref, N = 12, R = 30, apply.Gaussian, nc = 32, pseudo) {
  validImage(x)
  if (colorMode(ref) == TrueColor) stop("\'ref\' must not be in \'TrueColor\' color mode")
  if (!missing(apply.Gaussian)) warning("'apply.Gaussian' is deprecated.")
  if (!missing(pseudo))  warning("'pseudo' is deprecated.")
  
  gf = hullFeatures(x)
  mf = moments(x=x, ref=ref)
  ef = edgeFeatures(x=x, ref=ref)
  hf = haralickFeatures(x=x, ref=ref, nc=nc)
  zf = zernikeMoments(x=x, ref=ref, N=N, R=R, apply.Gaussian=apply.Gaussian, pseudo=pseudo)

  ## remove the feature 'm.pxs', which is equal to 'h.s'
  if (getNumberOfFrames(x,'total')==1) {
    impxs = match('m.pxs', colnames(mf)) 
    mf = mf[, -impxs]
  } else {
    impxs = match('m.pxs', colnames(mf[[1]])) 
    for (i in seq_along(hf)) mf[[i]] = mf[[i]][, -impxs]
  }
  
  if (getNumberOfFrames(x,'total')==1) features = list( cbind(gf, mf, ef, hf, zf) )
  else {
    features = vector("list", length(gf))
    for (i in seq_along(gf)) features[[i]] = cbind(gf[[i]], mf[[i]], ef[[i]], hf[[i]], zf[[i]])
  }
  return(features)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
zernikeMoments = function(x, ref, N=12, R=30, apply.Gaussian, pseudo) {
  checkCompatibleImages(x, ref)
  if (!missing(apply.Gaussian))  warning("'apply.Gaussian' is deprecated.")
  else apply.Gaussian=FALSE
  if (!missing(pseudo))  warning("'pseudo' is deprecated.")
  else pseudo=FALSE
 
  if (getNumberOfFrames(x,'total') == 1) xy <- moments(x=x, ref=ref)[, c('m.x','m.y'), drop=FALSE]
  else xy <- lapply(moments(x=x, ref=ref), function(x) x[, c('m.x','m.y'), drop=FALSE] )
  
  if (!pseudo) return(.Call("lib_zernike", castImage(x), castImage(ref), xy, as.numeric(R), as.integer(N), as.integer(apply.Gaussian), PACKAGE='EBImage'))
  else return(.Call("lib_pseudo_zernike", castImage(x), castImage(ref), xy, as.numeric(R), as.integer(N), as.integer(apply.Gaussian), PACKAGE='EBImage'))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
hullFeatures = function(x) {
  validImage(x)
  if (colorMode(x) == TrueColor) stop("this method doesn't support the \'TrueColor\' color mode")

  ## get basic hull features
  res <- .Call( "lib_basic_hull", castImage(x), PACKAGE='EBImage')
  if (is.null(res)) return( NULL )
  
  ## get central moments
  moms <- function(m) {
    ## normalized central moments (to size)
    m11 <- m[2,2,] / m[1,1,]
    m02 <- m[1,3,] / m[1,1,]
    m20 <- m[3,1,] / m[1,1,]

    t1 <- m20 + m02
    t2 <- sqrt( 4 * m11^2 + (m20 - m02)^2 )
    eig1 <- 0.5 * (t1 + t2)
    eig2 <- 0.5 * (t1 - t2)
    theta <- 0.5 * atan2( 2 * m11, m20 - m02 )
    
    ## second division to m[1,1,] to allow for mxx/m00^2 for scale invariants
    m11 <- m11 / m[1,1,]
    m02 <- m02 / m[1,1,]
    m20 <- m20 / m[1,1,]
    I1 <- m20 + m02
    I2 <- (m20 - m02)^2 + 4 * m11^2
    matrix( c(theta, eig1, eig2, sqrt(1-eig2/eig1), I1, I2), nrow=dim(m)[3], ncol=6 )
  }
  white = array(1, dim=dim(x))
  resm <- smoments(x=x, ref=white, pw=2, what="c")
  if ( is.matrix(res) ) res <- cbind( res, moms(resm) )
  else for ( i in seq_along(res) ) res[[i]] <- cbind( res[[i]], moms(resm[[i]]) )
  cn <- c("g.x", "g.y", "g.s", "g.p", "g.pdm", "g.pdsd", "g.effr", "g.acirc", "g.sf",
          "g.edge", "g.theta", "g.l1", "g.l2", "g.ecc", "g.I1", "g.I2")
  
  if ( is.matrix(res) ) colnames(res) <- cn
  else res <- lapply(res, function(x) { if ( is.matrix(x) ) colnames(x) <- cn; x } )
  return( res )
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.edgeProfile <- function(x, ref=NULL, n=32, fft=FALSE, scale=TRUE, rotate=TRUE) {
   checkCompatibleImages(x,ref)
   
  ## get centres of objects xy, rotation angle theta and if required effective
  ## radius for scaling, s. If a reference image is provided, then xy and t are
  ## affected by the intensity distribution (as per moments)
  if ( getNumberOfFrames(x,'total')== 1 ) {
    if ( is.null(ref) ) xyt <- hullFeatures(x=x)[, c('g.x','g.y','g.theta'), drop=FALSE]
    else xyt <- moments(x=x, ref=ref)[, c('m.x','m.y','m.theta'), drop=FALSE]
    if ( scale ) s <- hullFeatures(x=x)[,'g.effr'] else s <- NULL
  }
  else {
    if ( is.null(ref) ) xyt <- lapply(hullFeatures(x), function(x) x[,c('g.x','g.y','g.theta'), drop=FALSE] )
    else xyt <- lapply(moments(x=x, ref=ref), function(x) x[,c('m.x','m.y','m.theta'), drop=FALSE] )
    if ( scale ) s <- lapply(hullFeatures(x=x), function(x) x[,'g.effr'] )
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
  checkCompatibleImages(x, ref)
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
      colnames(res) <- c("h.asm", "h.con", "h.cor", "h.var", "h.idm", "h.sav", "h.sva", 
                         "h.sen", "h.ent", "h.dva", "h.den", "h.f12", "h.f13")
    res
  }
  if ( !is.list(hm) ) return( do.features(hm) )
  lapply( hm, do.features )
}   




