#script to set simulation parameters 
gamma_f<-c(0,0.3,-0.3,-0.3,0.3,-0.3,0.3)

beta_f<-c(0,-2,-1,1,1,-1,1,1)

nu0<-0.5
nu1<--0.4
nu2<--0.4
nu3<--0.4

n1<-3000
n2<-1500

save(gamma_f, beta_f, nu0, nu1, nu2, nu3, n1, n2, file = paste(getwd(),"/params.RData", sep=""))