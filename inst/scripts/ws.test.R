# this script will test if the following functions are generally working:
# read.image, normalize, mOpen, mClose (mErode, mDilate), gaussFilter, thresh, 
# distMap, ws, wsPaint, display, Image

library(EBImage)

a1 <- read.image("HT10-C04_A03_w1-0.png")
t1 <- thresh(normalize(a1), 100, 100, 0.01, TRUE)
t1 <- mClose(mOpen(t1, 1, mKernel(7)), 1, mKernel(7))

dm1 <- distMap(t1)
x1 <- ws(dm1, 15, 10, 0.2, ref = a1)
img1 <- wsPaint(x1, a1, opac = 0.3)

seeds <- x1$objects[,1:2]

a2 <- read.image("HT10-C04_A03_w3-0.png")
a3 <- sqrt(normalize(normalize(a1) + normalize(a2)))
t2 <- thresh(a3, 300, 300, 0.00, TRUE)
t2 <- mClose(mOpen(t2, 1, mKernel(11)), 1, mKernel(11))
dm2 <- distMap(t2)
x2 <- ws(dm2, 50, 15, 0.2, seeds, ref = a2)
img2 <- wsPaint(x2, a2, opac = 0.3)
img2 <- wsPaint(x1, img2, opac = 0.3)

x3 <- ws(dm2, 45, 20, 0.2, ref = a2) # no seeds
img3 <- wsPaint(x3, a2, opac = 0.3)
img3 <- wsPaint(x1, img3, opac = 0.3)

.dim <- dim(img1)
.dim[[3]] <- 3
img <- Image(as.integer(c(img1, img2, img3)), .dim, rgb=TRUE)

nucl <- normalize(wsImages(x1, a1), independent = TRUE)
cells <- normalize(wsImages(x2, a2), independent = TRUE)

cat("done: display(img), display(nucl), display(cells)\n")

