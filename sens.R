#script to estimate sensitivity/specificity of generated data
sens=spec=ppv=npv<-c()
for (i in 1:100){
  load(paste(getwd(), "/results/data",i,".RData", sep=""))

sens[i]<-length(which(data1$trt==1 & data1$trte==1))/length(which(data1$trt==1))
spec[i]<-length(which(data1$trt==0 & data1$trte==0))/length(which(data1$trt==0))
ppv[i]<-length(which(data1$trt==1 & data1$trte==1))/length(which(data1$trte==1))
npv[i]<-length(which(data1$trt==0 & data1$trte==0))/length(which(data1$trte==0))

}

save(sens, spec, ppv, npv, file = paste(getwd(),"/error_est.RData", sep=""))
