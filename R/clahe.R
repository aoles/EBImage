clahe = function(x, nx = 8, ny = nx, bins = 256, limit = 2, keep.range = FALSE) {
  validImage(x)
  .Call(C_clahe, castImage(x), as.integer(nx), as.integer(ny), as.integer(bins), as.double(limit), isTRUE(keep.range))
}
