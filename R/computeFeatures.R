## TODO
## feed list
## zero objects

## main function
## x: labelled image
## ref: a list of images
## expandRef: a function to expand ref
computeFeatures = function(x, ref, methods.noref=c("computeFeatures.moment", "computeFeatures.shape"),
  methods.ref=c("computeFeatures.basic", "computeFeatures.moment", "computeFeatures.haralick"),
  xname="x", refnames, properties=FALSE, expandRef=genericExpandRef, ...) {
  ## check arguments
  if (!is.array(x) || getNumberOfFrames(x)>1)  stop("'x' must be a matrix containing a labelled image")
  if (class(x)=="Image") x = imageData(x)
  if (is.list(ref)) {
    if (!is.null(names(ref))) refnames = names(ref)
    ref = do.call(EBImage::combine, ref)
  }
  if (!is.array(ref)) stop("'ref' must be an array containing the reference images")
  if (class(ref)=="Image") ref = imageData(ref)
  nref = getNumberOfFrames(ref)
  dim(ref) = c(dim(ref)[1:2], nref)
  if (missing(refnames)) refnames = letters[1:nref]
  if (length(refnames)!=nref) stop("'refnames' must have the same length as the number of images in 'ref'")
  
  ## transform references
  if (!is.null(expandRef)) {
    ref = expandRef(ref=ref, refnames=refnames)
    refnames = attr(ref, "refnames")
    nref = getNumberOfFrames(ref)
  }

  ## compute features
  if (!properties) {
    xs = splitObjects(x)
    ## compute features without reference
    features.noref = do.call(cbind, lapply(methods.noref, do.call, list(x=x, xs=xs, properties=FALSE, ...)))
    
    ## compute features with reference, for each channel
    features.ref = lapply(1:nref, function(i) {
      ref0 = ref[,,i]
      do.call(cbind, lapply(methods.ref, do.call, list(x=x, ref=ref0, xs=xs, properties=FALSE, ...)))
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

example = function() {
  y = readImage(system.file('images', 'nuclei.tif', package='EBImage'))[,,1]
  x = thresh(y, 10, 10, 0.05)
  x = opening(x, makeBrush(5, shape='disc'))
  x = bwlabel(x)

  system.time({ft = computeFeatures(x, combine(y, flip(y), flop(y)))})
  
}

genericExpandRef = function(ref, refnames) {
  ## compute join channels
  n = getNumberOfFrames(ref)
  if (n>1) {
    comb = combn(n, 2)
    jref = list()
    for (i in 1:ncol(comb)) {
      i1 = comb[1, i]
      i2 = comb[2, i]
      jref0 = list((ref[,,i1] - mean(ref[,,i1])) * (ref[,,i2] - mean(ref[,,i2])))
      names(jref0) = paste(refnames[i1], refnames[i2], sep="")
      jref = c(jref, jref0)
    }
    ref = combine(ref, combine(jref))
    refnames = c(refnames, names(jref))
  }
  
  ## add granulometry by blob transform
  blob = gblob(x0=15, n=49, alpha=0.8, beta=1.2)
  bref = filter2(ref, blob)/2
  ref = combine(ref, bref)
  refnames = c(refnames, paste("B", refnames, sep=""))
  attr(ref, "refnames") = refnames
  ref
}

splitObjects = function(x) {
  z = which(as.integer(x)>=1)
  split(z, x[z])
}



gblob = function(x0, n, alpha, beta) {
  xx = seq(-x0, x0, length.out=n)
  xx = matrix(xx, nrow=length(xx), ncol=length(xx))
  xx = sqrt(xx^2+t(xx)^2)
  z = dnorm(xx, mean=0, sd=alpha) -  0.65*dnorm(xx, mean=0, sd=beta)
  z/sum(z)
}



## basic pixel-independant statistics
computeFeatures.basic = function(x, ref, xs, properties=FALSE, basic.quantiles=c(0.01, 0.05, 0.5, 0.95, 0.99), ...) {
  qnames = paste('b.q', gsub('\\.', '', as.character(basic.quantiles)), sep='')
  if (!properties) {
    ## check arguments
    if (missing(xs)) xs = splitObjects(x)

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

## image moments
computeFeatures.moment = function(x, ref, xs, properties=FALSE, ...) {
  if (!properties) {
    ## check arguments
    if (missing(xs)) xs = splitObjects(x)
    if (missing(ref)) ref = array(1, dim=dim(x))
    
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
      theta = atan(2*mu11/(mu20 - mu02))/2 ## angle
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

## shape features
computeFeatures.shape = function(x, xs, properties=FALSE, ...) {
  if (!properties) {
    ## check arguments
    if (missing(xs)) xs = splitObjects(x)
    contours = ocontour(x)

    ## compute features
    features = do.call(rbind, lapply(contours, function(z) {
      cz = apply(z, 2, mean)
      radius = sqrt(rowSums((z - rep(cz, each=nrow(z)))^2))
      c(s.perimeter=nrow(z), s.radius.mean=mean(radius), s.radius.min=min(radius),
        s.radius.max=max(radius))
    }))
    cbind(s.area=sapply(xs, length), features)
  } else {
    ## feature properties
    data.frame(name=c("s.area", "s.perimeter", "s.radius.mean", "s.radius.min", "s.radius.max"),
               translation.invariant = c(TRUE, TRUE, TRUE, TRUE, TRUE),
               rotation.invariant = c(TRUE, TRUE, TRUE, TRUE, TRUE))
  }
}
  
## haralick features
computeFeatures.haralick = function(x, ref, properties=FALSE, haralick.nbins=32, haralick.scales=c(1, 2), ...) {
  snames = paste("s", haralick.scales, sep="")
  if (!properties) {
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
      hf = haralickFeatures(xscaled, refscaled, nc=haralick.nbins)
      ## after scaling, xscaled may not contain the original object labels
      ## fortunately, haralickFeatures() leave 0-row for them, therefore, the data
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

correlatedFeatures = function(f, threshold=0.99) {
  zc = cor(f)
  zc[upper.tri(zc, diag=TRUE)] = NA
  ztw = which(abs(zc)<threshold, 2)
  za = colnames(f)[ztw[,1]]
  zb = colnames(f)[ztw[,2]]
  cbind(za=za, zb=zb)
}
