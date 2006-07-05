# this script will test if the following functions are generally working:
# read.image, normalize, gaussFilter, thresh, distMap, ws, wsPaint, display, Image

library(EBImage)

a1 <-normalize(read.image("HT10-C04_A03_w1-0.png"))
t1 <- thresh(a1, 100, 100, 0.01, TRUE)
dm1 <- sqrt(distMap(t1))
x1 <- ws(dm1, 15, 10, 0.2, ref = a1)
img1 <- wsPaint(x1, a1, opac = 0.3)

seeds <- x1$objects[,1:2]

a2 <- sqrt(normalize(a1 + normalize(read.image("HT10-C04_A03_w3-0.png"))))
t2 <- thresh(a2, 300, 300, 0.00, TRUE)
dm2 <- sqrt(distMap(t2))
x2 <- ws(dm2, 50, 15, 0.2, seeds, ref = a2)
img2 <- wsPaint(x2, a2, opac = 0.3)
img2 <- wsPaint(x1, img2, opac = 0.3)

x3 <- ws(dm2, 45, 20, 0.2, ref = a2) # no seeds
img3 <- wsPaint(x3, a2, opac = 0.3)
img3 <- wsPaint(x1, img3, opac = 0.3)

.dim <- dim(img1)
.dim[[3]] <- 3
img <- Image(as.integer(c(img1, img2, img3)), .dim, rgb=TRUE)
display(img)
