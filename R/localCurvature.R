localCurvature = function(x, h=100, maxk=10000) {
 # check that x is of proper type
 if (!is.matrix(x) & !is.data.frame(x)) stop("'x' should be a data frame or a matrix")
 # if data frame convert to matrix to standardize processing
 if (is.data.frame(x)) x = as.matrix(x)
 # check that x has proper dimensions
 if (length(dim(x))!=2L || dim(x)[2L]!=2L) stop("'x' should have dimensions of N x 2")

 # compute arc length
 px = c(0, cumsum(sqrt(rowSums(diff(x)^2))))

 # smooth and calculate derivatives
 poly = lp(px, deg=2, h=h)
 derivatives = apply(x, 2, function(v) {
  d1 = locfit(v ~ poly, deriv=1, maxk=maxk)
  d2 = locfit(v ~ poly, deriv=c(1,1), maxk=maxk)
  list(predict(d1, newdata=data.frame(px=px)),
  predict(d2, newdata=data.frame(px=px)))
 })
 
 # calculate signed curvature
 curvature = function(d) (d[[1]][[1]]*d[[2]][[2]] - d[[2]][[1]]*d[[1]][[2]]) / (d[[1]][[1]]^2+d[[2]][[1]]^2)^1.5

 return(list(contour=x, curvature=curvature(derivatives), length=max(px, na.rm=TRUE)))
}
