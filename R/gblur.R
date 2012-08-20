gblur <- function(x, sigma, radius=2*ceiling(3*sigma)+1) {
  filter = makeBrush(size=radius, shape="gaussian",sigma=sigma)
  filter2(x, filter=filter)
}
