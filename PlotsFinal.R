#script to generate plot of results. 
library(xtable)
#loading results
aa1=aa2<-matrix(0, 39, 2)
#no PS
load(paste(getwd(), "/NoPS.RData", sep=""))
library(xtable)
aa1=aa2<-matrix(0, 39, 2)
#no PS
load(paste(getwd(), "/NoPS.RData", sep=""))
betas_marg<-betas_margR
betas<-betasR
siz<-500
betas_margT<-betas_marg[1]
betas_margMSE<-c()
betas_mm=betas_tt<-betas_marg
betas_s<-betas
for (i in 1:dim(betas_marg)[2]){
		betas_tt<-betas_mm[which(is.infinite(betas_mm[,i])==FALSE),]
		betas_s<-betas_s[which(is.infinite(betas_mm[,i])==FALSE),]
		betas_mm<-betas_tt
	}

betas_margBias=betas_margBias2<-matrix(0, dim(betas_mm)[1], dim(betas_mm)[2])	
for (i in 1:dim(betas_mm)[2]){

betas_margBias[,i]<-betas_mm[,i]-betas_margT
betas_margMSE[i]<-mean((betas_mm[,i]-betas_margT)^2)
betas_margBias2[,i]<-(betas_mm[,i]-betas_margT)/betas_margT

}

b1<-round((colMeans(betas_margBias)), digits=5) 
b2<-round(sqrt(betas_margMSE), digits=5)
bb1<-matrix(0,nrow=dim(betas_marg)[2], ncol=2)
for (j in 1:dim(betas_marg)[2]){
	bb1[j,]<-c(b1[j], b2[j])

}
xtable(bb1, digits=5)
aa1[1,]<-bb1[2,]
aa1[2,]<-bb1[3,]
aa1[3,]<-bb1[6,]


aa2[1,]<-bb1[4,]
aa2[2,]<-bb1[5,]
aa2[3,]<-bb1[7,]

#weighted adj
load(paste(getwd(), "/WeightPS.RData", sep=""))
betas_marg<-betas_margR
betas<-betasR
siz<-500

betas_margMSE<-c()
betas_mm=betas_tt<-betas_marg
betas_s<-betas
for (i in 1:dim(betas_marg)[2]){
		betas_tt<-betas_mm[which(is.infinite(betas_mm[,i])==FALSE),]
		betas_s<-betas_s[which(is.infinite(betas_mm[,i])==FALSE),]
		betas_mm<-betas_tt
	}

betas_margBias<-matrix(0, dim(betas_mm)[1], dim(betas_mm)[2])	
for (i in 1:dim(betas_mm)[2]){

betas_margBias[,i]<-betas_mm[,i]-betas_margT
betas_margMSE[i]<-mean((betas_mm[,i]-betas_margT)^2, na.rm=TRUE)
}

b1<-round((colMeans(betas_margBias, na.rm=TRUE)), digits=5) 
b2<-round(sqrt(betas_margMSE), digits=5)
bb1<-matrix(0,nrow=dim(betas_marg)[2], ncol=2)
for (j in 1:dim(betas_marg)[2]){
	bb1[j,]<-c(b1[j], b2[j])

}
xtable(bb1, digits=5)
aa1[13,]<-bb1[2,]
aa1[14,]<-bb1[3,]
aa1[15,]<-bb1[4,]
aa1[16,]<-bb1[5,]
aa1[17,]<-bb1[6,]
aa1[18,]<-bb1[7,]
aa1[19,]<-bb1[14,]
aa1[20,]<-bb1[15,]
aa1[21,]<-bb1[16,]


aa2[13,]<-bb1[8,]
aa2[14,]<-bb1[9,]
aa2[15,]<-bb1[10,]
aa2[16,]<-bb1[11,]
aa2[17,]<-bb1[12,]
aa2[18,]<-bb1[13,]
aa2[19,]<-bb1[17,]
aa2[20,]<-bb1[18,]
aa2[21,]<-bb1[19,]

#stratified adj
load(paste(getwd(), "/StrataPS.RData", sep=""))
betas_marg<-betas_margR
betas<-betasR
siz<-500

betas_margMSE<-c()
betas_mm=betas_tt<-betas_marg
betas_s<-betas
for (i in 1:dim(betas_marg)[2]){
		betas_tt<-betas_mm[which(is.infinite(betas_mm[,i])==FALSE),]
		betas_s<-betas_s[which(is.infinite(betas_mm[,i])==FALSE),]
		betas_mm<-betas_tt
	}

betas_margBias<-matrix(0, dim(betas_mm)[1], dim(betas_mm)[2])	
for (i in 1:dim(betas_mm)[2]){

betas_margBias[,i]<-betas_mm[,i]-betas_margT
betas_margMSE[i]<-mean((betas_mm[,i]-betas_margT)^2, na.rm=TRUE)
}

b1<-round((colMeans(betas_margBias, na.rm=TRUE)), digits=5) 
b2<-round(sqrt(betas_margMSE), digits=5)
bb1<-matrix(0,nrow=dim(betas_marg)[2], ncol=2)
for (j in 1:dim(betas_marg)[2]){
	bb1[j,]<-c(b1[j], b2[j])

}
xtable(bb1, digits=5)
aa1[4,]<-bb1[8,]
aa1[5,]<-bb1[9,]
aa1[6,]<-bb1[10,]
aa1[7,]<-bb1[11,]
aa1[8,]<-bb1[12,]
aa1[9,]<-bb1[13,]
aa1[10,]<-bb1[14,]
aa1[11,]<-bb1[15,]
aa1[12,]<-bb1[16,]

aa2[4,]<-bb1[2,]
aa2[5,]<-bb1[3,]
aa2[6,]<-bb1[4,]
aa2[7,]<-bb1[5,]
aa2[8,]<-bb1[6,]
aa2[9,]<-bb1[7,]
aa2[10,]<-bb1[17,]
aa2[11,]<-bb1[18,]
aa2[12,]<-bb1[19,]

#matched
load(paste(getwd(), "/MatchPS.RData", sep=""))
betas_marg<-betas_margR
betas<-betasR
siz<-500

betas_margMSE<-c()
betas_mm=betas_tt<-betas_marg
betas_s<-betas
for (i in 1:dim(betas_marg)[2]){
		betas_tt<-betas_mm[which(is.infinite(betas_mm[,i])==FALSE),]
		betas_s<-betas_s[which(is.infinite(betas_mm[,i])==FALSE),]
		betas_mm<-betas_tt
	}

betas_margBias<-matrix(0, dim(betas_mm)[1], dim(betas_mm)[2])	
for (i in 1:dim(betas_mm)[2]){

betas_margBias[,i]<-betas_mm[,i]-betas_margT
betas_margMSE[i]<-mean((betas_mm[,i]-betas_margT)^2, na.rm=TRUE)
}

b1<-round((colMeans(betas_margBias, na.rm=TRUE)), digits=5) 
b2<-round(sqrt(betas_margMSE), digits=5)
bb1<-matrix(0,nrow=dim(betas_marg)[2], ncol=2)
for (j in 1:dim(betas_marg)[2]){
	bb1[j,]<-c(b1[j], b2[j])

}
xtable(bb1, digits=5)
aa1[31,]<-bb1[2,]
aa1[32,]<-bb1[3,]
aa1[33,]<-bb1[4,]
aa1[34,]<-bb1[5,]
aa1[35,]<-bb1[6,]
aa1[36,]<-bb1[7,]
aa1[37,]<-bb1[14,]
aa1[38,]<-bb1[15,]
aa1[39,]<-bb1[16,]

aa2[31,]<-bb1[8,]
aa2[32,]<-bb1[9,]
aa2[33,]<-bb1[10,]
aa2[34,]<-bb1[11,]
aa2[35,]<-bb1[12,]
aa2[36,]<-bb1[13,]
aa2[37,]<-bb1[17,]
aa2[38,]<-bb1[18,]
aa2[39,]<-bb1[19,]

#creating plot
par(mfrow=c(1,2), pty='s')

#Bias
ymm<-c(-0.6,0.6)
plot(aa2[,1], xlim=c(0,17),ylim=c(ymm[1],ymm[2]),type="n",xaxt="n", xlab="", col="red", ylab="Bias", yaxt="n", frame.plot=FALSE, pch=17)
ticks <- seq(ymm[1], ymm[2], by=0.1)       # sequence for ticks and labels
axis(2, at = ticks,         # y-Axis
    labels = ticks, cex.axis=0.3)

xx<-c(1:3)
bt3<-c(4,13,31)
points(xx, aa2[bt3,1],   xlim=c(0,17), ylim=ymm, pch=c(0,1,2),col=c("blue", "red", "green"))

xx<-c(7:9)
bt<-c(8,17,35)
points(xx, aa2[bt,1],   xlim=c(0,17), ylim=ymm, pch=c(0,1,2),col=c("blue", "red", "green"))

xx<-c(13:15)
bt2<-c(12,21,39)

points(xx, aa2[bt2,1],   xlim=c(0,17), ylim=ymm, pch=c(0,1,2),col=c("blue", "red", "green"))

abline(v=5, lty=2)
abline(v=11, lty=2)
abline(h=0)
temp<-c(0,5,11,17)
axis(1,temp, labels=FALSE)

mtext("Gold Standard",1,line=1,at=2, cex=0.5)
mtext("No Adj",1,line=1,at=8, cex=0.5)
mtext("2-Step Adj",1,line=1,at=14, cex=0.5)
legend("topleft", c("Subclassification by PS", "IPTW using PS", "Matching by PS"), pch=c(0,1,2), col=c("blue", "red", "green"), bg="white", cex=0.6)

#MSE
aa2[,1]<-aa2[,2]

ymm<-c(0,0.6)
plot(aa2[,1], xlim=c(0,17),ylim=ymm,type="n",xaxt="n", yaxt="n", xlab="", col="red", ylab=expression(sqrt(MSE)), frame.plot=FALSE, pch=17)

ticks <- seq(ymm[1], ymm[2], by=0.1)       # sequence for ticks and labels
axis(2, at = ticks,         # y-Axis
    labels = ticks, cex.axis=0.3)

xx<-c(1:3)
bt3<-c(4,13,31)
points(xx, aa2[bt3,1],   xlim=c(0,17), ylim=ymm, pch=c(0,1,2),col=c("blue", "red", "green"))

xx<-c(7:9)
bt<-c(8,17,35)
points(xx, aa2[bt,1],   xlim=c(0,17), ylim=ymm, pch=c(0,1,2),col=c("blue", "red", "green"))

xx<-c(13:15)
bt2<-c(12,21,39)

points(xx, aa2[bt2,1],   xlim=c(0,17), ylim=ymm, pch=c(0,1,2),col=c("blue", "red", "green"))

abline(v=5, lty=2)
abline(v=11,lty=2)
abline(h=0)
temp<-c(0,5,11,17)
axis(1,temp, labels=FALSE)

mtext("Gold Standard",1,line=1,at=2, cex=0.5)
mtext("No Adj",1,line=1,at=8, cex=0.5)
mtext("2-Step Adj",1,line=1,at=14, cex=0.5)
legend("topleft", c("Subclassification by PS", "IPTW using PS", "Matching by PS"), pch=c(0,1,2), col=c("blue", "red", "green"),cex=0.6, bg="white")

dev.copy2pdf(file="plot1_final3c.pdf")
