## This scripts makes two figures for the explanation
##   of the distance map transformation that we use in talks
## WH 5.4.2007
##
library("EBImage")
library("RColorBrewer")
library("lattice")

h = matrix(0, nrow=500, ncol=500)

x=140; y=140; r=80
h[ ((col(h)-x)^2+(row(h)-y)^2) < r^2 ] = 1

x=250; y=290; r=150
h[ ((col(h)-x)^2+(row(h)-y)^2) < r^2 ] = 1

dm = distMap(Image(h, dim=dim(h)))

png("tmp/distmap1.png", width=500, heigth=500)
par(mai=rep(0,4))
image(h, col=c("white", "black"))
dev.off()

png("tmp/distmap2.png", width=512, heigth=660)
print(levelplot(matrix(dm, nrow=nrow(h), ncol=ncol(h)),
   col.regions=colorRampPalette(c("white", brewer.pal(9, "YlOrRd")))(256)))
dev.off()
