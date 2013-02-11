localCurvature = function(x, h=100, maxk=10000) {
 df=as.data.frame(x)
 colnames(df)=c('x', 'y')
 px = c(0, cumsum(sqrt(c(rowSums(sapply(df, diff)^2)))))
 poly = lp(px, deg=2, h=h)
 derivatives = apply(df, 2, function(v) {
  d1 = locfit(v ~ poly, deriv=1, maxk=maxk)
  d2 = locfit(v ~ poly, deriv=c(1,1), maxk=maxk)
  list(predict(d1, newdata=data.frame(px=px)),
  predict(d2, newdata=data.frame(px=px)))
 })
 curvature = function(d) (d[[1]][[1]]*d[[2]][[2]] - d[[2]][[1]]*d[[1]][[2]]) / (d[[1]][[1]]^2+d[[2]][[1]]^2)^1.5
 return(list(contour=x, curvature=curvature(derivatives), length=max(px, na.rm=TRUE)))
}

