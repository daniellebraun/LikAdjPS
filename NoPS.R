#script to generate data and perform analysis under no propensity score adjustment
unlink(".RData*")
unlink(".RData")
library(xtable)
library(maxLik)

if(file.exists(paste(getwd(), "/results/NoPS",s,".RData", sep=""))==FALSE) 
{  
set.seed(12498+s)
load(paste(getwd(), "/params.RData", sep=""))
 
siz=1
betas<-matrix(nrow=siz,ncol=70)
gammas<-matrix(nrow=siz,ncol=28)
betas_marg<-matrix(nrow=siz, ncol=8)

   
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

for (m in 1:siz) {
#generating main study
#entire study 
n<-n1
#confounder
c1=rnorm(n, 0, 1)
c2=rnorm(n, 0, 2)
c3=rnorm(n, 0, 3)
c4=rbinom(n, 1, 0.5)
c5=rbinom(n, 1, 0.5)
c6=rbinom(n,1,0.5)

#True Exposure
ps_gs<-1/(1+exp(-(gamma0F+gamma1F*c1+gamma2F*c2+gamma3F*c3+gamma4F*c4+gamma5F*c5+gamma6F*c6)))
trt<-rbinom(n,1,ps_gs)
#Error in exposure
p_trte<-1/(1+exp(-(nu0+nu1*trt+nu2*c1+nu3*c4)))
trte<-rbinom(n, 1, p_trte)
#Outcome
p_y<-1/(1+exp(-(beta0F+beta1F*trt+beta2F*c1+beta3F*c2+beta4F*c3+beta5F*c4+beta6F*c5+beta7F*c6)))
y = rbinom(n,1, p_y)
 
data1<-data.frame(y,trt, trte, c1, c2, c3, c4, c5, c6)
colnames(data1)<-c("y","trt", "trte", "c1", "c2", "c3", "c4", "c5", "c6")
#generating validation study
#validation study
n<-n2
#confounder
c1=rnorm(n, 0, 1)
c2=rnorm(n, 0, 2)
c3=rnorm(n, 0, 3)
c4=rbinom(n, 1, 0.5)
c5=rbinom(n, 1, 0.5)
c6=rbinom(n,1,0.5)

#True Exposure
ps_gs<-1/(1+exp(-(gamma0F+gamma1F*c1+gamma2F*c2+gamma3F*c3+gamma4F*c4+gamma5F*c5+gamma6F*c6)))
trt<-rbinom(n,1,ps_gs)
#Error in exposure
p_trte<-1/(1+exp(-(nu0+nu1*trt+nu2*c1+nu3*c4)))
trte<-rbinom(n, 1, p_trte)
#Outcome
p_y<-1/(1+exp(-(beta0F+beta1F*trt+beta2F*c1+beta3F*c2+beta4F*c3+beta5F*c4+beta6F*c5+beta7F*c6)))
y = rbinom(n,1, p_y)
 
data_val<-data.frame(y,trt, trte, c1, c2, c3, c4, c5, c6)
colnames(data_val)<-c("y","trt", "trte", "c1", "c2", "c3", "c4", "c5", "c6")
  
#Calculating 3 different PS
#PS based on true data

  gammas[m,8:14]<-glm(trt ~ c1+c2+c3+c4+c5+c6, family=binomial(logit), data=data1)$coefficients
  
  data1$ps_true<-1/(1+exp(-(gammas[m,8]+gammas[m,9]*data1$c1+gammas[m,10]*data1$c2+gammas[m,11]*data1$c3+gammas[m,12]*data1$c4+gammas[m,13]*data1$c5+gammas[m,14]*data1$c6)))  

#PS based on the error-prone data
  gammas[m,15:21]<-glm(trte ~ c1+c2+c3+c4+c5+c6, family=binomial(logit), data=data1)$coefficients
 
   data1$ps_ep<-1/(1+exp(-(gammas[m,15]+gammas[m,16]*data1$c1+gammas[m,17]*data1$c2+gammas[m,18]*data1$c3+gammas[m,19]*data1$c4+gammas[m,20]*data1$c5+gammas[m,21]*data1$c6)))

#PS based on correction
 psc<-c()
 psc[1:4]<-glm(trte~trt+c1+c4, family=binomial(logit), data=data_val)$coefficients

z<-data1$trte
pz1x1<-exp(psc[1]+psc[2]+psc[3]*data1$c1+psc[4]*data1$c4)/(1+exp(psc[1]+psc[2]+psc[3]*data1$c1+psc[4]*data1$c4))
pz1x0<-exp(psc[1]+psc[3]*data1$c1+psc[4]*data1$c4)/(1+exp(psc[1]+psc[3]*data1$c1+psc[4]*data1$c4))

pz0x1<-1-pz1x1
pz0x0<-1-pz1x0

  xx<-data_val$trt
  cc1<-data_val$c1
  cc2<-data_val$c2
  cc3<-data_val$c3
  cc4<-data_val$c4
  cc5<-data_val$c5
  cc6<-data_val$c6

  lik = function(gamma){ 
  	gamma0<-gamma[1]
  	gamma1<-gamma[2]
  	gamma2<-gamma[3]
  	gamma3<-gamma[4]
  	gamma4<-gamma[5]
  	gamma5<-gamma[6]
  	gamma6<-gamma[7]

  	
  	a1<-sum(z *log (( pz1x1 *(1/(1+exp(-(gamma0+c1*gamma1+c2*gamma2+c3*gamma3+c4*gamma4+c5*gamma5+c6*gamma6))))+pz1x0*(1-1/(1+exp(-(gamma0+c1*gamma1+c2*gamma2+c3*gamma3+c4*gamma4+c5*gamma5+c6*gamma6)))))))
a2<-sum((1-z) *log(pz0x1*(1/(1+exp( -(gamma0+c1*gamma1+c2*gamma2+c3*gamma3+c4*gamma4+c5*gamma5+c6*gamma6)))) +pz0x0*(1-1/(1+exp( -(gamma0+c1*gamma1+c2*gamma2+c3*gamma3+c4*gamma4+c5*gamma5+c6*gamma6))))))
a3<-sum(xx*log(1/(1+exp(-(gamma0+cc1*gamma1+cc2*gamma2+cc3*gamma3+cc4*gamma4+cc5*gamma5+cc6*gamma6)))))
a4<-sum((1-xx)*log((1-1/(1+exp(-(gamma0+cc1*gamma1+cc2*gamma2+cc3*gamma3+cc4*gamma4+cc5*gamma5+cc6*gamma6))))))
   return(a1+a2+a3+a4)   
      }

  res <- tryCatch(maxLik(lik, start=c(0,0, 0, 0, 0, 0, 0)))


gammas[m,22:28]<-coef(res)

data1$ps_adj<-1/(1+exp(-(gammas[m,22]+gammas[m,23]*data1$c1+gammas[m,24]*data1$c2+gammas[m,25]*data1$c3+gammas[m,26]*data1$c4+gammas[m,27]*data1$c5+gammas[m,28]*data1$c6)))
save(gammas, file = paste(getwd(),"/results/gammas",s,".RData", sep=""))

#Calculating 3 different PS in validation data
#PS based on true data

  gammas[m,8:14]<-glm(trt ~ c1+c2+c3+c4+c5+c6, family=binomial(logit), data=data_val)$coefficients
  
  data_val$ps_true<-1/(1+exp(-(gammas[m,8]+gammas[m,9]*data_val$c1+gammas[m,10]*data_val$c2+gammas[m,11]*data_val$c3+gammas[m,12]*data_val$c4+gammas[m,13]*data_val$c5+gammas[m,14]*data_val$c6)))  

#PS based on the error-prone data
  gammas[m,15:21]<-glm(trte ~ c1+c2+c3+c4+c5+c6, family=binomial(logit), data=data_val)$coefficients
 
   data_val$ps_ep<-1/(1+exp(-(gammas[m,15]+gammas[m,16]*data_val$c1+gammas[m,17]*data_val$c2+gammas[m,18]*data_val$c3+gammas[m,19]*data_val$c4+gammas[m,20]*data_val$c5+gammas[m,21]*data_val$c6)))

#PS based on correction
 psc<-c()
 psc[1:4]<-glm(trte~trt+c1+c4, family=binomial(logit), data=data_val)$coefficients

z<-data_val$trte
pz1x1<-exp(psc[1]+psc[2]+psc[3]*data_val$c1+psc[4]*data_val$c4)/(1+exp(psc[1]+psc[2]+psc[3]*data_val$c1+psc[4]*data_val$c4))
pz1x0<-exp(psc[1]+psc[3]*data_val$c1+psc[4]*data_val$c4)/(1+exp(psc[1]+psc[3]*data_val$c1+psc[4]*data_val$c4))

pz0x1<-1-pz1x1
pz0x0<-1-pz1x0

  c1<-data_val$c1
  c2<-data_val$c2
  c3<-data_val$c3
  c4<-data_val$c4
  c5<-data_val$c5
  c6<-data_val$c6

  lik = function(gamma){ 
  	gamma0<-gamma[1]
  	gamma1<-gamma[2]
  	gamma2<-gamma[3]
  	gamma3<-gamma[4]
  	gamma4<-gamma[5]
  	gamma5<-gamma[6]
  	gamma6<-gamma[7]

  	
  	a1<-sum(z *log (( pz1x1 *(1/(1+exp(-(gamma0+c1*gamma1+c2*gamma2+c3*gamma3+c4*gamma4+c5*gamma5+c6*gamma6))))+pz1x0*(1-1/(1+exp(-(gamma0+c1*gamma1+c2*gamma2+c3*gamma3+c4*gamma4+c5*gamma5+c6*gamma6)))))))
a2<-sum((1-z) *log(pz0x1*(1/(1+exp( -(gamma0+c1*gamma1+c2*gamma2+c3*gamma3+c4*gamma4+c5*gamma5+c6*gamma6)))) +pz0x0*(1-1/(1+exp( -(gamma0+c1*gamma1+c2*gamma2+c3*gamma3+c4*gamma4+c5*gamma5+c6*gamma6))))))

   return(a1+a2)   
      }

  res <- tryCatch(maxLik(lik, start=c(0,0, 0, 0, 0, 0, 0)))


gammas[m,22:28]<-coef(res)

data_val$ps_adj<-1/(1+exp(-(gammas[m,22]+gammas[m,23]*data_val$c1+gammas[m,24]*data_val$c2+gammas[m,25]*data_val$c3+gammas[m,26]*data_val$c4+gammas[m,27]*data_val$c5+gammas[m,28]*data_val$c6)))

save(data1, data_val, file = paste(getwd(),"/results/data",s,".RData", sep=""))


#model 1
#evaluating the betas
#model 1, y~trt+c
countF<-9
countL<-countF+7
count<-1
mm1<-count+1

betas[m,countF:countL]<-glm(y~trt+c1+c2+c3+c4+c5+c6, family=binomial(logit), data=data1)$coefficients

#marginal beta
a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+1])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
b<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
betas_marg[m,mm1]<-mean(a-b)

#model 2, y~trte+c
countF<-countF+8
countL<-countF+7
mm1<-mm1+1
betas[m,countF:countL]<-glm(y~trte+c1+c2+c3+c4+c5+c6, family=binomial(logit), data=data1)$coefficients

#marginal beta
a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+1])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
b<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
betas_marg[m,mm1]<-mean(a-b)

#model 3, y~trt
countF<-countF+8
countL<-countF+7
mm1<-mm1+1
betas[m,countF:(countF+1)]<-glm(y~trt, family=binomial(logit), data=data1)$coefficients
betas[m,(countF+2):countL]<-'NA'

#marginal beta
a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,(countF+1)])))
b<-1/(1+exp(-as.numeric(betas[m,countF])))
betas_marg[m,mm1]<-mean(a-b) 
 
#model 4, y~trte
countF<-countF+8
countL<-countF+7
mm1<-mm1+1
betas[m,countF:(countF+1)]<-glm(y~trte, family=binomial(logit), data=data1)$coefficients
betas[m,(countF+2):countL]<-'NA'

#marginal beta
a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,(countF+1)])))
b<-1/(1+exp(-as.numeric(betas[m,countF])))
betas_marg[m,mm1]<-mean(a-b) 

#Likelihood corrections

#correct the model for x
#model 5, y~trte+c
countF<-countF+8
countL<-countF+7
mm1<-mm1+1
c1<-data1$c1
c2<-data1$c2
c3<-data1$c3
c4<-data1$c4
c5<-data1$c5
c6<-data1$c6
z<-data1$trte
y<-data1$y
yy<-data_val$y

psc<-c()
psc[1:4]<-glm(trt~trte+c1+c4, family=binomial(logit), data=data_val)$coefficients

data1$px1z1=px1z1<-exp(psc[1]+psc[2]+psc[3]*data1$c1+psc[4]*data1$c4)/(1+exp(psc[1]+psc[2]+psc[3]*data1$c1+psc[4]*data1$c4))

data1$px1z0= px1z0<-exp(psc[1]+psc[3]*data1$c1+psc[4]*data1$c4)/(1+exp(psc[1]+psc[3]*data1$c1+psc[4]*data1$c4))

 px0z1<-1-px1z1
 px0z0<-1-px1z0



  lik = function(beta){ 
  	beta0<-beta[1]
  	beta1<-beta[2]
  	beta2<-beta[3]
	beta3<-beta[4]
	beta4<-beta[5]
	beta5<-beta[6]
	beta6<-beta[7]
	beta7<-beta[8]
	
	
  	a1<-sum(y*z*log ( (1-px0z1) * (1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+px0z1 * (1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))
a2<-sum(y*(1-z) *log(px1z0  * (1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+ (1-px1z0) * (1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))
a3<-sum((1-y)*z *log( (1-px0z1) * (1-1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+px0z1 * (1-1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6)))))) 
a4<-sum((1-y)*(1-z)*log(px1z0  * (1-1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+ (1-px1z0) * (1-1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6)))))) 
a5<-sum(yy*log(1/(1+exp(-(beta0+beta1*xx+beta2*cc1+beta3*cc2+beta4*cc3+beta5*cc4+beta6*cc5+beta7*cc6)))))
a6<-sum((1-yy)*(log(1-1/(1+exp(-(beta0+beta1*xx+beta2*cc1+beta3*cc2+beta4*cc3+beta5*cc4+beta6*cc5+beta7*cc6))))))
return(a1+a2+a3+a4+a5+a6)
  	   	
  	}
  res <- maxLik(lik, start=c(0,0,0,0,0,0,0,0)) 
  betas[m,countF:countL]<-coef(res)
    
a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+1])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
b<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
betas_marg[m,mm1]<-mean(a-b)


#model 6, y~trte
countF<-countF+8
countL<-countF+7
mm1<-mm1+1

y<-data1$y
z<-data1$trte

  lik = function(beta){ 
  	beta0<-beta[1]
  	beta1<-beta[2]
	
  	a1<-sum(y*z *log ( (1-px0z1) * (1/(1+exp(-(beta0+beta1))))+px0z1 * (1/(1+exp(-(beta0)))))) 
  	a2<-sum(y*(1-z) *log (px1z0  * (1/(1+exp(-(beta0+beta1))))+ (1-px1z0) * (1/(1+exp(-(beta0))))))
  	a3<-sum((1-y)*z * log( (1-px0z1) * (1-1/(1+exp(-(beta0+beta1))))+px0z1 * (1-1/(1+exp(-(beta0))))))
  	a4<-sum((1-y)*(1-z) * (px1z0  * (1-1/(1+exp(-(beta0+beta1))))+ (1-px1z0) * (1-1/(1+exp(-(beta0)))))) 	
  	a5<-sum(yy*log(1/(1+exp(-(beta0+beta1*xx)))))
  	a6<-sum((1-yy)*(log(1-1/(1+exp(-(beta0+beta1*xx))))))
      return(a1+a2+a3+a4+a5+a6)
  	
  	}
  res <- maxLik(lik, start=c(0,0)) 
  betas[m,countF:(countF+1)]<-coef(res)
  betas[m,(countF+2): countL]<-'NA'
  
  a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,(countF+1)])))
b<-1/(1+exp(-as.numeric(betas[m,countF])))
betas_marg[m,mm1]<-mean(a-b) 

}

#estimating "true" marginal beta

n<-1000000
#confounder
c1=rnorm(n, 0, 1)
c2=rnorm(n, 0, 2)
c3=rnorm(n, 0, 3)
c4=rbinom(n, 1, 0.5)
c5=rbinom(n, 1, 0.5)
c6=rbinom(n,1,0.5)

#True Exposure
ps_gs<-1/(1+exp(-(gamma0F+gamma1F*c1+gamma2F*c2+gamma3F*c3+gamma4F*c4+gamma5F*c5+gamma6F*c6)))
trt<-rbinom(n,1,ps_gs)
#Error in exposure
p_trte<-1/(1+exp(-(nu0+nu1*trt+nu2*c1+nu3*c4)))
trte<-rbinom(n, 1, p_trte)
#Outcome
p_y<-1/(1+exp(-(beta0F+beta1F*trt+beta2F*c1+beta3F*c2+beta4F*c3+beta5F*c4+beta6F*c5+beta7F*c6)))
y = rbinom(n,1, p_y)
 
data_marg<-data.frame(y,trt, trte, c1, c2, c3, c4, c5, c6)
colnames(data_marg)<-c("y","trt", "trte", "c1", "c2", "c3", "c4", "c5", "c6")

m<-c()
m[1:8]<-glm(y~trt+c1+c2+c3+c4+c5+c6, family=binomial(logit), data=data_marg)$coefficients

#marginal beta
data1<-data_marg
a<-1/(1+exp(-as.numeric(m[1])-as.numeric(m[2])-as.numeric(m[3])*data1$c1-as.numeric(m[4])*data1$c2-as.numeric(m[5])*data1$c3-as.numeric(m[6])*data1$c4-as.numeric(m[7])*data1$c5-as.numeric(m[8])*data1$c6))
b<-1/(1+exp(-as.numeric(m[1])-as.numeric(m[3])*data1$c1-as.numeric(m[4])*data1$c2-as.numeric(m[5])*data1$c3-as.numeric(m[6])*data1$c4-as.numeric(m[7])*data1$c5-as.numeric(m[8])*data1$c6))
betas_marg[,1]<-mean(a-b)


save(betas, betas_marg, file = paste(getwd(),"/results/NoPS",s,".RData", sep=""))
}