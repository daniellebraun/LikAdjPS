#script to estimate balance across different models.
stdbias=matrix(0,4,6)
unlink(".RData*")
unlink(".RData")
library(xtable)
library(maxLik)
library(MatchIt)
library(twang)
set.seed(12498)

#siz<-100
siz=1
betas<-matrix(nrow=siz,ncol=200)
gammas<-matrix(nrow=siz,ncol=28)
betas_marg<-matrix(nrow=siz, ncol=25)

 load(paste(getwd(), "/params.RData", sep=""))

gammas[,1]=gamma0F=gamma_f[1]
gammas[,2]=gamma1F=gamma_f[2]
gammas[,3]=gamma2F=gamma_f[3]
gammas[,4]=gamma3F=gamma_f[4]
gammas[,5]=gamma4F=gamma_f[5]
gammas[,6]=gamma5F=gamma_f[6]
gammas[,7]=gamma6F=gamma_f[7]

betas[,1]=beta0F=beta_f[1]
betas[,2]=beta1F=beta_f[2]
betas[,3]=beta2F=beta_f[3]
betas[,4]=beta3F=beta_f[4]
betas[,5]=beta4F=beta_f[5]
betas[,6]=beta5F=beta_f[6]
betas[,7]=beta6F=beta_f[7]
betas[,8]=beta7F=beta_f[8]

for (s in 1:500){
load(paste(getwd(), "/results/data",s,".RData", sep=""))

#BALANCE
#stratification
#based on true
ps.1.subcl<-matchit(trt~c1+c2+c3+c4+c5+c6, data=data1, method="subclass",   sub.by="all")
summary.1.subcl<-summary(ps.1.subcl, standardize=TRUE)


stdbias[2,]<-abs(summary.1.subcl$sum.subclass[2:7,3])
stdbias[1,]<-abs(summary.1.subcl$sum.all[2:7,3])


#based on error-prone
ps.2.subcl<-matchit(trt~c1+c2+c3+c4+c5+c6, data=data1, distance=data1$ps_ep, method="subclass",   sub.by="all")
summary.2.subcl<-summary(ps.2.subcl, standardize=TRUE)
stdbias[3,]<-abs(summary.2.subcl$sum.subclass[2:7,3])

#based on adjusted
ps.3.subcl<-matchit(trt~c1+c2+c3+c4+c5+c6, data=data1, distance=data1$ps_adj, method="subclass",  sub.by="all")
summary.3.subcl<-summary(ps.3.subcl, standardize=TRUE)

stdbias[4,]<-abs(summary.3.subcl$sum.subclass[2:7,3])


save(stdbias, file = paste(getwd(),"/results/balance",s,".RData", sep=""))
}