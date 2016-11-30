#script to merge results across 500 replications
#no PS
siz<-500
betasR<-matrix(nrow=siz,ncol=70)
#gammasR<-matrix(nrow=siz,ncol=28)
betas_margR<-matrix(nrow=siz, ncol=8)
for (i in 1:500){
load(paste(getwd(), "/results/NoPS",i,".RData", sep=""))
betasR[i,]<-betas[1,]
#gammasR[i,]<-gammas[1,]
betas_margR[i,]<-betas_marg[1,]
}
save(betasR, betas_margR, file = paste(getwd(),"/NoPS.RData", sep=""))

#weighted PS
siz<-500
betasR<-matrix(nrow=siz,ncol=200)
#gammasR<-matrix(nrow=siz,ncol=28)
betas_margR<-matrix(nrow=siz, ncol=25)
for (i in 1:500){
load(paste(getwd(), "/results/WeightPS",i,".RData", sep=""))
betasR[i,]<-betas[1,]
#gammasR[i,]<-gammas[1,]
betas_margR[i,]<-betas_marg[1,]
}
save(betasR, betas_margR, file = paste(getwd(),"/WeightPS.RData", sep=""))

#Strata PS
siz<-500
betasR<-matrix(nrow=siz,ncol=250)
#gammasR<-matrix(nrow=siz,ncol=28)
betas_margR<-matrix(nrow=siz, ncol=25)

for (i in 1:500){
load(paste(getwd(), "/results/StrataPS",i,".RData", sep=""))
betasR[i,]<-betas[1,]
#gammasR[i,]<-gammas[1,]
betas_margR[i,]<-betas_marg[1,]}
save(betasR, betas_margR, file = paste(getwd(),"/StrataPS.RData", sep=""))

#match PS
siz<-500
betasR<-matrix(nrow=siz,ncol=250)
#gammasR<-matrix(nrow=siz,ncol=28)
betas_margR<-matrix(nrow=siz, ncol=25)
for (i in 1:500){
load(paste(getwd(), "/results/MatchPS",i,".RData", sep=""))
betasR[i,1:200]<-betas[1,]
#gammasR[i,1:28]<-gammas[1,]
betas_margR[i,1:25]<-betas_marg[1,]

}
save(betasR, betas_margR, file = paste(getwd(),"/MatchPS.RData", sep=""))


#balance PS
stdbias.0.subclass=stdbias.1.subclass=stdbias.2.subclass=stdbias.3.subclass=matrix(0, 500, 6)
for (i in 1:500){
load(paste(getwd(), "/results/balance",i,".RData", sep=""))
stdbias.0.subclass[i,]<-stdbias[1,]
stdbias.1.subclass[i,]<-stdbias[2,]
stdbias.2.subclass[i,]<-stdbias[3,]
stdbias.3.subclass[i,]<-stdbias[4,]

}
save(stdbias.0.subclass, stdbias.1.subclass,stdbias.2.subclass,stdbias.3.subclass, file = paste(getwd(),"/BalancePS.RData", sep=""))