#script to plot standardized bias
load(paste(getwd(), "/BalancePS.RData", sep=""))

ss<-matrix(0, 4, 6)


ss[1,]<-colMeans(stdbias.0.subclass, na.rm=TRUE)
ss[2,]<-colMeans(stdbias.1.subclass, na.rm=TRUE)
ss[3,]<-colMeans(stdbias.2.subclass, na.rm=TRUE)
ss[4,]<-colMeans(stdbias.3.subclass, na.rm=TRUE)
ymm<-c(0,0.8)

#dev.copy2pdf(file="plot1.pdf")
aa<-c(1:3)
plot(aa, xlim=c(0,3),ylim=c(ymm[1],ymm[2]),type="n",xaxt="n", xlab="", col="red", ylab="Average Standardized Bias", yaxt="n", frame.plot=FALSE, pch=17)
ticks <- seq(ymm[1], ymm[2], by=0.1)       # sequence for ticks and labels
axis(2, at = ticks,         # y-Axis
    labels = ticks, cex.axis=0.6)
xx<-c(0:3)
bt3<-c(ss[1,1], ss[2,1], ss[3,1], ss[4,1])
lines(x=xx, y=bt3, col="red", lwd=3)
points(x=xx, y=bt3, col="red")

bt3<-c(ss[1,2], ss[2,2], ss[3,2], ss[4,2])
lines(x=xx, y=bt3, col="green3", lwd=3)
points(x=xx, y=bt3, col="green3")

bt3<-c(ss[1,3], ss[2,3], ss[3,3], ss[4,3])
lines(x=xx, y=bt3, col="blue", lwd=3)
points(x=xx, y=bt3, col="blue")

bt3<-c(ss[1,4], ss[2,4], ss[3,4], ss[4,4])
lines(x=xx, y=bt3, col="cyan", lwd=3)
points(x=xx, y=bt3, col="cyan")

bt3<-c(ss[1,5], ss[2,5], ss[3,5], ss[4,5])
lines(x=xx, y=bt3, col="magenta", lwd=3)
points(x=xx, y=bt3, col="magenta")

bt3<-c(ss[1,6], ss[2,6], ss[3,6], ss[4,6])
lines(x=xx, y=bt3, col="gray", lwd=3)
points(x=xx, y=bt3, col="gray")
temp<-c(0:3)
axis(1,temp, labels=FALSE)

mtext("No PS",1,line=1,at=0, cex=0.8)
mtext("PS True",1,line=1,at=1, cex=0.8)
mtext("PS EP",1,line=1,at=2, cex=0.8)
mtext("PS Adj",1,line=1,at=3, cex=0.8)
legend("topright", c("X1","X2", "X3", "X4", "X5", "X6"), pch=c(1,1,1,1,1,1), col=c("red", "green3", "blue", "cyan", "magenta", "gray"),cex=0.45, bg="white")
dev.copy2pdf(file="std3c.pdf")
