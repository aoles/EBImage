## tests
tests = function() {

  ## build labeled image x and reference y
  y = readImage(system.file("images", "nuclei.tif", package="EBImage"))[,,1]
  x = thresh(y, 10, 10, 0.05)
  x = opening(x, makeBrush(5, shape='disc'))
  x = bwlabel(x)
  display(x)
  display(y)

  ## standard call 88 features
  ft = computeFeatures(x, y)
  pft = computeFeatures(x, y, properties=TRUE)
  stopifnot(all(colnames(ft)==pft$name))
  
  ## call without expandRef 49 features
  ft = computeFeatures(x, y, expandRef=NULL)
  pft = computeFeatures(x, y, expandRef=NULL, properties=TRUE)
  stopifnot(all(colnames(ft)==pft$name))
  
  ## changing parameters
  ft = computeFeatures(x, y, basic.quantiles=c(0.2, 0.3), haralick.scales=c(1, 4, 8), expandRef=NULL)
  pft = computeFeatures(x, y, basic.quantiles=c(0.2, 0.3), haralick.scales=c(1, 4, 8), expandRef=NULL, properties=TRUE)
  stopifnot(all(colnames(ft)==pft$name))
  
  ## call with 3 reference images
  yc = list(d=y, t=flip(y), a=flop(y))
  ft = computeFeatures(x, yc)
  pft = computeFeatures(x, yc, properties=TRUE)
  stopifnot(all(colnames(ft)==pft$name))
  
  ## test standardExpandRef
  z = standardExpandRef(yc)
  str(z)
  display(combine(z))
  
  ## image with one point
  x2 = array(0, dim=dim(x))
  x2[100, 100] = 1
  ft = computeFeatures(x2, y)
}

## main function
computeFeatures = function(x, ref, methods.noref=c("computeFeatures.moment", "computeFeatures.shape"),
  methods.ref=c("computeFeatures.basic", "computeFeatures.moment", "computeFeatures.haralick"),
  xname="x", refnames, properties=FALSE, expandRef=standardExpandRef, ...) {
  ## check arguments
  x = checkx(x)
  ref = convertRef(ref, refnames)
  refnames = names(ref)
  nref = length(ref)
 
  ## expand ref
  if (!is.null(expandRef)) {
    ref = expandRef(ref, refnames)
    refnames = names(ref)
    nref = length(ref)
  }
  
  ## compute features
  if ( !isTRUE(properties) ) {
    ## prepare data
    xs = splitObjects(x)
    if (length(xs)==0) return(NULL)
    
    ## compute features without reference
    features.noref = do.call(cbind, lapply(methods.noref, do.call, list(x=x, properties=FALSE, xs=xs, ...)))
    
    ## compute features with reference, for each channel
    features.ref = lapply(1:nref, function(i) {
      do.call(cbind, lapply(methods.ref, do.call, list(x=x, ref=ref[[i]], properties=FALSE, xs=xs, ...)))
    })
    names(features.ref) = refnames

    ## sum up data into a single matrix
    features = c("0"=list(features.noref), features.ref)
    features = features[!sapply(features, is.null)]
    for (i in 1:length(features)) colnames(features[[i]]) = paste(names(features)[i], colnames(features[[i]]), sep=".")
    features = do.call(cbind, features)
    colnames(features) = paste(xname, colnames(features), sep=".")
    features
  } else {
    ## feature properties
    pfeatures.noref = do.call(rbind, lapply(methods.noref, do.call, list(properties=TRUE, ...)))
    pfeatures.ref = do.call(rbind, lapply(methods.ref, do.call, list(properties=TRUE, ...)))
    pfeatures.ref = lapply(1:length(refnames), function(z) pfeatures.ref)
    names(pfeatures.ref) = refnames

    ## sum up data
    pfeatures = c("0"=list(pfeatures.noref), pfeatures.ref)
    pfeatures = pfeatures[!sapply(pfeatures, is.null)]
    for (i in 1:length(pfeatures)) pfeatures[[i]]$name = paste(names(pfeatures)[i], pfeatures[[i]]$name, sep=".")
    pfeatures = do.call(rbind, pfeatures)
    pfeatures$name = paste(xname, pfeatures$name, sep=".")
    rownames(pfeatures) = NULL
    pfeatures
  }
}

## standard reference expansion
standardExpandRef = function(ref, refnames) {
  ## check arguments
  ref = convertRef(ref, refnames)
  nref = length(ref)
  refnames = names(ref)
    
  ## adding joint channels
  if (nref>1) {
    for (i in 1:(nref-1)) {
      for (j in (i+1):nref) {
        refi = ref[[i]]
        refj = ref[[j]]
        jref0 = list( (refi - mean(refi)) * (refj - mean(refj)) )
        names(jref0) = paste0(refnames[i], refnames[j])
        ref = c(ref, jref0)
      }
    }
  }

  ## adding granulometry by blob transform
  blob = gblob(x0=15, n=49, alpha=0.8, beta=1.2)
  bref = lapply(ref, function(r) filter2(r, blob)/2)
  names(bref) = paste0("B", names(ref))
  c(ref, bref)
}

## basic pixel-independant statistics
computeFeatures.basic = function(x, ref, properties=FALSE, basic.quantiles=c(0.01, 0.05, 0.5, 0.95, 0.99), xs, ...) {
  qnames = paste0('b.q', gsub('\\.', '', as.character(basic.quantiles)))
  if ( !isTRUE(properties) ) {
    ## check arguments
    x = checkx(x)
    if (missing(xs)) xs = splitObjects(x)
    if (length(xs)==0) return(NULL)
    ref = convertRef(ref)[[1]]
    
    ## compute features
    features = do.call(rbind, lapply(xs, function(z) {
      z = ref[z]
      q = quantile(z, basic.quantiles)
      names(q) = qnames
      c(b.mean=mean(z), b.sd=sd(z), b.mad=mad(z), q)
    }))

    ## special processing for single points
    z = sapply(xs, length)==1
    features[,'b.sd'] = ifelse(z, 0, features[,'b.sd'])
    features
  }
  else {
    ## feature properties
    data.frame(name=c("b.mean", "b.sd", "b.mad", qnames),
               translation.invariant=TRUE,
               rotation.invariant=TRUE)
  }
}

## shape features
computeFeatures.shape = function(x, properties=FALSE, xs, ...) {
  if ( !isTRUE(properties) ) {
    ## check arguments
    x = checkx(x)
    if (missing(xs)) xs = splitObjects(x)
    if (length(xs)==0) return(NULL)
    
    contours = ocontour(x)
    ## compute features
    features = do.call(rbind, lapply(contours, function(z) {
      cz = apply(z, 2, mean)
      radius = sqrt(rowSums((z - rep(cz, each=nrow(z)))^2))
      radius.mean = mean(radius)
      radius.sd = sqrt(mean((radius - radius.mean)^2))
      
      c(s.perimeter=nrow(z), s.radius.mean=radius.mean, s.radius.sd=radius.sd, s.radius.min=min(radius),
        s.radius.max=max(radius))
    }))
    cbind(s.area=sapply(xs, length), features)
  } else {
    ## feature properties
    data.frame(name=c("s.area", "s.perimeter", "s.radius.mean", "s.radius.sd", "s.radius.min", "s.radius.max"),
               translation.invariant = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
               rotation.invariant = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))
  }
}

## image moments
computeFeatures.moment = function(x, ref, properties=FALSE, xs, ...) {
  if ( !isTRUE(properties) ) {
    ## check arguments
    x = checkx(x)
    if (missing(xs)) xs = splitObjects(x)
    if (length(xs)==0) return(NULL)
    if (missing(ref)) ref = array(1, dim=dim(x))
    ref = convertRef(ref)[[1]]
    
    ## image moments: computing m{pq} = sum_{x^p*y^q*f(x, y)}
    m00 = sapply(xs, function(z) sum(ref[z]))
    m10 = {w = row(ref)*ref ; sapply(xs, function(z) sum(w[z]))}
    m01 = {w = col(ref)*ref ; sapply(xs, function(z) sum(w[z]))}
    m20 = {w = row(ref)^2*ref ; sapply(xs, function(z) sum(w[z]))}
    m02 = {w = col(ref)^2*ref ; sapply(xs, function(z) sum(w[z]))}
    m11 = {w = col(ref)*row(ref)*ref ; sapply(xs, function(z) sum(w[z]))}
    
    ## derive interesting geometric quantities from image moments
    ## done by diagonalisation of the centered image moment matrix
    ## see http://en.wikipedia.org/wiki/Image_moment
    suppressWarnings({
      cx = m10/m00 ## center of mass x
      cy = m01/m00 ## center of mass y
      mu20 = m20/m00 - cx^2 ## temporary quantity
      mu02 = m02/m00 - cy^2 ## temporary quantity
      mu11 = m11/m00 - cx*cy ## temporary quantity
      det = sqrt(4*mu11^2 + (mu20 - mu02)^2) ## temporary quantity
      theta = atan2(2*mu11, (mu20 - mu02))/2 ## angle
      l1 = sqrt((mu20 + mu02 + det)/2)*4 ## major axis
      l2 = sqrt((mu20 + mu02 - det)/2)*4 ## minor axis
      eccentricity = sqrt(1-l2^2/l1^2) ## eccentricity
    })

    ## special processing for single points
    features = cbind(m.cx=cx, m.cy=cy, m.majoraxis=l1, m.eccentricity=eccentricity, m.theta=theta)
    features[is.na(features) | is.nan(features)] = 0
    features
  } else {
    ## feature properties
    data.frame(name=c("m.cx", "m.cy", "m.majoraxis", "m.eccentricity", "m.theta"),
      translation.invariant = c(FALSE, FALSE, TRUE, TRUE, TRUE),
      rotation.invariant = c(TRUE, TRUE, TRUE, TRUE, FALSE))
  }
}

## haralick features
## h.*: haralick features
computeFeatures.haralick = function(x, ref, properties=FALSE, haralick.nbins=32, haralick.scales=c(1, 2), xs, ...) {
  snames = paste0("s", haralick.scales)
  if ( !isTRUE(properties) ) {
    ## check arguments
    x = checkx(x)
    if (missing(xs)) xs = splitObjects(x)
    if (length(xs)==0) return(NULL)
    ref = convertRef(ref)[[1]]
  
    ## clip data
    ref[ref>1] = 1
    ref[ref<0] = 0
    features = lapply(haralick.scales, function(scale) {
      if (scale>1) {
        combx = seq(1, nrow(x), by=scale)
        comby = seq(1, ncol(x), by=scale)
        xscaled = x[combx, comby]
        refscaled = ref[combx, comby]
      } else {
        xscaled = x
        refscaled = ref
      }
      hf = .haralickFeatures(xscaled, refscaled, nc=haralick.nbins)
      ## after scaling, xscaled may not contain the original object labels
      ## fortunately, .haralickFeatures() leave 0-row for them, therefore, the data
      ## is kept synceda and it is enough to fill up hf with 0-row to level up to
      ## the original number of objects
      rbind(hf, matrix(0, nrow=max(x)-max(xscaled), ncol=ncol(hf)))
    })
    for (i in 1:length(features)) colnames(features[[i]]) = paste(colnames(features[[i]]), snames[i], sep=".")
    do.call(cbind, features)
  } else {
    ## feature properties
    hnames = paste("h", c("asm", "con", "cor", "var", "idm", "sav", "sva", "sen", "ent", "dva", "den", "f12", "f13"), sep=".")
    names = paste(hnames, rep(snames, each=length(hnames)), sep=".")
    data.frame(name=names, translation.invariant=TRUE, rotation.invariant=TRUE)
  }
}

## make a blob
## example: blob = gblob(x0=15, n=49, alpha=0.8, beta=1.2)
gblob = function(x0, n, alpha, beta) {
  xx = seq(-x0, x0, length.out=n)
  xx = matrix(xx, nrow=length(xx), ncol=length(xx))
  xx = sqrt(xx^2+t(xx)^2)
  z = dnorm(xx, mean=0, sd=alpha) -  0.65*dnorm(xx, mean=0, sd=beta)
  z/sum(z)
}

## convert ref into a list of images, for fast processing
convertRef = function(ref, refnames) {
  if (!is.array(ref) && !is.list(ref)) stop("'ref' must be an array or a list containing the reference images")
  if (is.array(ref)) {
    nref = .numberOfFrames(ref, 'total')
    if (class(ref)=="Image") ref = imageData(ref)
    ndim = length(dim(ref))
    if (ndim==2) ref = list(ref)
    else if (ndim==3) ref = lapply(1:nref, function(i) ref[,,i])
    else stop ("'ref' must be a 2D or 3D array")
  }
  else if (is.list(ref)) {
    nref = length(ref)
    if (missing(refnames) && !is.null(names(ref))) refnames = names(ref)
    ## sanity check
    ref = lapply(ref, function(r) {
      ndim = length(dim(r))
      if (class(r)=="Image") r = imageData(r)
      if (ndim<2 || ndim>3) stop ("'ref' must contain only 2D arrays")
      else if (ndim==3) {
        if (dim(r)[3]>1) stop ("'ref' must contain only 2D arrays")
        else r = r[,,1]
      }
      r
    })
  }
  if (missing(refnames)) refnames = letters[1:nref]
  if (length(refnames)!=nref) stop ("'refnames' must have the same length as 'ref'")
  names(ref) = refnames
  ref
}

## check x
checkx = function(x) {
  if (!is.array(x))  stop("'x' must be a 2D array")
  if (class(x)=="Image") x = imageData(x)
  ndim = length(dim(x))
  if (ndim<2||ndim>3)  stop("'x' must be a 2D array")
  if (ndim==3) {
    if (dim(x)[3]>1) stop("'x' must be a 2D array")
    else x = x[,,1]
  }
  x
}

## split an labelled image into list of points
splitObjects = function(x) {
  z = which(as.integer(x)>0L)
  split(z, x[z])
}

.haralickMatrix <- function(x, ref, nc=32) {
  checkCompatibleImages(x, ref)
  rref <- range(ref)
  if ( rref[1] < 0 || rref[2] > 1 ) {
    ref[ref<0] = 0
    ref[ref>1] = 1
    warning( "Values in 'ref' have been limited to the range [0,1]" )
  }
  
  res <- .Call(C_haralickMatrix, castImage(x), castImage(ref), as.integer(nc))
  return( res )
}

.haralickFeatures <- function(x, ref, nc=32) {
  validImage(x)
  hm <- .haralickMatrix(x=x, ref=ref, nc=nc)
  if ( is.null(hm) || !(is.array(hm) || is.list(hm)) ) return( NULL )
  do.features <- function(m) {
    if (dim(m)[3]>0) res <- .Call(C_haralickFeatures, m)
    else res = matrix(0, nrow=0, ncol=13) ## no objects
    if ( is.matrix(res) )
      colnames(res) <- c("h.asm", "h.con", "h.cor", "h.var", "h.idm", "h.sav", "h.sva", 
                         "h.sen", "h.ent", "h.dva", "h.den", "h.f12", "h.f13")
    res
  }
  if ( !is.list(hm) ) return( do.features(hm) )
  lapply( hm, do.features )
} 
