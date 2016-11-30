#script for proposed adjustment under IPTW 
unlink(".RData*")
unlink(".RData")
library(xtable)
library(maxLik)
#library(MatchIt)
#library(twang)
if(file.exists(paste(getwd(), "/results/WeightPS",s,".RData", sep=""))==FALSE) 
{
set.seed(12498+s)

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

for (m in 1:siz) {

load(paste(getwd(), "/results/data",s,".RData", sep=""))

#adjusting only by correcting PS
#model 1,y~trt+c, ps_true
countF<-9
countL<-countF+7
count<-1
mm1<-count+1
data1$ps<-data1$ps_true

data1$w[data1$trt==0]<-mean((1-data1$ps[data1$trt==0]))/(1-data1$ps[data1$trt==0])
data1$w[data1$trt==1]<-mean((data1$ps[data1$trt==1]))/(data1$ps[data1$trt==1])

betas[m,countF:countL]<-glm(y~trt+c1+c2+c3+c4+c5+c6, family=binomial(logit), data=data1, weights=w)$coefficients

#marginal
a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+1])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
b<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
betas_marg[m,mm1]<-mean(a-b)

# #mode2 1,y~trt+c, ps_ep
countF<-countF+8
countL<-countF+7
mm1<-mm1+1

data1$ps<-data1$ps_ep

data1$w[data1$trt==0]<-mean((1-data1$ps[data1$trt==0]))/(1-data1$ps[data1$trt==0])
data1$w[data1$trt==1]<-mean((data1$ps[data1$trt==1]))/(data1$ps[data1$trt==1])

betas[m,countF:countL]<-glm(y~trt+c1+c2+c3+c4+c5+c6, family=binomial(logit), data=data1, weights=w)$coefficients

#marginal
a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+1])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
b<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
betas_marg[m,mm1]<-mean(a-b)

# #mode3 1,y~trt+c, ps_adj
countF<-countF+8
countL<-countF+7
mm1<-mm1+1

data1$ps<-data1$ps_adj

data1$w[data1$trt==0]<-mean((1-data1$ps[data1$trt==0]))/(1-data1$ps[data1$trt==0])
data1$w[data1$trt==1]<-mean((data1$ps[data1$trt==1]))/(data1$ps[data1$trt==1])

betas[m,countF:countL]<-glm(y~trt+c1+c2+c3+c4+c5+c6, family=binomial(logit), data=data1, weights=w)$coefficients

#marginal
a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+1])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
b<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
betas_marg[m,mm1]<-mean(a-b)

# #model 4, y~trte+c, w=ps_true
countF<-countF+8
countL<-countF+7
mm1<-mm1+1

data1$ps<-data1$ps_true

data1$w[data1$trte==0]<-mean((1-data1$ps[data1$trte==0]))/(1-data1$ps[data1$trte==0])
data1$w[data1$trte==1]<-mean((data1$ps[data1$trte==1]))/(data1$ps[data1$trte==1])

betas[m,countF:countL]<-glm(y~trte+c1+c2+c3+c4+c5+c6, family=binomial(logit), data=data1, weights=w)$coefficients

#marginal
a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+1])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
b<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
betas_marg[m,mm1]<-mean(a-b)

# #model 5, y~trte+c, w=ps_ep
countF<-countF+8
countL<-countF+7
mm1<-mm1+1

data1$ps<-data1$ps_ep


data1$w[data1$trte==0]<-mean((1-data1$ps[data1$trte==0]))/(1-data1$ps[data1$trte==0])
data1$w[data1$trte==1]<-mean((data1$ps[data1$trte==1]))/(data1$ps[data1$trte==1])

betas[m,countF:countL]<-glm(y~trte+c1+c2+c3+c4+c5+c6, family=binomial(logit), data=data1, weights=w)$coefficients

#marginal
a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+1])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
b<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
betas_marg[m,mm1]<-mean(a-b)

# #model 6, y~trte+c, w=ps_adj
countF<-countF+8
countL<-countF+7
mm1<-mm1+1

data1$ps<-data1$ps_adj


data1$w[data1$trte==0]<-mean((1-data1$ps[data1$trte==0]))/(1-data1$ps[data1$trte==0])
data1$w[data1$trte==1]<-mean((data1$ps[data1$trte==1]))/(data1$ps[data1$trte==1])

betas[m,countF:countL]<-glm(y~trte+c1+c2+c3+c4+c5+c6, family=binomial(logit), data=data1, weights=w)$coefficients

#marginal
a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+1])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
b<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
betas_marg[m,mm1]<-mean(a-b)


#model 7, y~trt, w=ps_true
countF<-countF+8
countL<-countF+7
mm1<-mm1+1

data1$ps<-data1$ps_true

data1$w[data1$trt==0]<-mean((1-data1$ps[data1$trt==0]))/(1-data1$ps[data1$trt==0])
data1$w[data1$trt==1]<-mean((data1$ps[data1$trt==1]))/(data1$ps[data1$trt==1])

betas[m,countF:(countF+1)]<-glm(y~trt, family=binomial(logit), data=data1, weights=w)$coefficients
betas[m,(countF+2):countL]<-'NA'

#marginal beta
a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,(countF+1)])))
b<-1/(1+exp(-as.numeric(betas[m,countF])))
betas_marg[m,mm1]<-mean(a-b) 


#model 8, y~trt, w=ps_ep
countF<-countF+8
countL<-countF+7
mm1<-mm1+1

data1$ps<-data1$ps_ep

data1$w[data1$trt==0]<-mean((1-data1$ps[data1$trt==0]))/(1-data1$ps[data1$trt==0])
data1$w[data1$trt==1]<-mean((data1$ps[data1$trt==1]))/(data1$ps[data1$trt==1])

betas[m,countF:(countF+1)]<-glm(y~trt, family=binomial(logit), data=data1, weights=w)$coefficients
betas[m,(countF+2):countL]<-'NA'

#marginal beta
a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,(countF+1)])))
b<-1/(1+exp(-as.numeric(betas[m,countF])))
betas_marg[m,mm1]<-mean(a-b) 

#model 9, y~trt, w=ps_adj
countF<-countF+8
countL<-countF+7
mm1<-mm1+1

data1$ps<-data1$ps_adj

data1$w[data1$trt==0]<-mean((1-data1$ps[data1$trt==0]))/(1-data1$ps[data1$trt==0])
data1$w[data1$trt==1]<-mean((data1$ps[data1$trt==1]))/(data1$ps[data1$trt==1])

betas[m,countF:(countF+1)]<-glm(y~trt, family=binomial(logit), data=data1, weights=w)$coefficients
betas[m,(countF+2):countL]<-'NA'

#marginal beta
a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,(countF+1)])))
b<-1/(1+exp(-as.numeric(betas[m,countF])))
betas_marg[m,mm1]<-mean(a-b) 

#model 10, y~trte, w=ps_true
countF<-countF+8
countL<-countF+7
mm1<-mm1+1

data1$ps<-data1$ps_true
data1$w[data1$trte==0]<-mean((1-data1$ps[data1$trte==0]))/(1-data1$ps[data1$trte==0])
data1$w[data1$trte==1]<-mean((data1$ps[data1$trte==1]))/(data1$ps[data1$trte==1])


betas[m,countF:(countF+1)]<-glm(y~trte, family=binomial(logit), data=data1, weights=w)$coefficients
betas[m,(countF+2):countL]<-'NA'

#marginal beta
a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,(countF+1)])))
b<-1/(1+exp(-as.numeric(betas[m,countF])))
betas_marg[m,mm1]<-mean(a-b) 

#model 11, y~trte, w=ps_ep
countF<-countF+8
countL<-countF+7
mm1<-mm1+1

data1$ps<-data1$ps_ep
data1$w[data1$trte==0]<-mean((1-data1$ps[data1$trte==0]))/(1-data1$ps[data1$trte==0])
data1$w[data1$trte==1]<-mean((data1$ps[data1$trte==1]))/(data1$ps[data1$trte==1])


betas[m,countF:(countF+1)]<-glm(y~trte, family=binomial(logit), data=data1, weights=w)$coefficients
betas[m,(countF+2):countL]<-'NA'

#marginal beta
a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,(countF+1)])))
b<-1/(1+exp(-as.numeric(betas[m,countF])))
betas_marg[m,mm1]<-mean(a-b) 

#model 12, y~trte, w=ps_adj
countF<-countF+8
countL<-countF+7
mm1<-mm1+1

data1$ps<-data1$ps_adj
data1$w[data1$trte==0]<-mean((1-data1$ps[data1$trte==0]))/(1-data1$ps[data1$trte==0])
data1$w[data1$trte==1]<-mean((data1$ps[data1$trte==1]))/(data1$ps[data1$trte==1])


betas[m,countF:(countF+1)]<-glm(y~trte, family=binomial(logit), data=data1, weights=w)$coefficients
betas[m,(countF+2):countL]<-'NA'

#marginal beta
a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,(countF+1)])))
b<-1/(1+exp(-as.numeric(betas[m,countF])))
betas_marg[m,mm1]<-mean(a-b) 

#likelihood correction for x
 #model 13, y~trte+c, ps_true
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

  xx<-data_val$trt
  cc1<-data_val$c1
  cc2<-data_val$c2
  cc3<-data_val$c3
  cc4<-data_val$c4
  cc5<-data_val$c5
  cc6<-data_val$c6
  yy<-data_val$y
  gammasV<-c()
  gammasV[1:7]<-glm(trt ~ c1+c2+c3+c4+c5+c6, family=binomial(logit), data=data_val)$coefficients
  
 data_val$pval<-1/(1+exp(-(gammasV[1]+gammasV[2]*data_val$c1+gammasV[3]*data_val$c2+gammasV[4]*data_val$c3+gammasV[5]*data_val$c4+gammasV[6]*data_val$c5+gammasV[7]*data_val$c6))) 
psc<-c()
psc[1:4]<-glm(trt~trte+c1+c4, family=binomial(logit), data=data_val)$coefficients
data1$px1z1=px1z1<-exp(psc[1]+psc[2]+psc[3]*data1$c1+psc[4]*data1$c4)/(1+exp(psc[1]+psc[2]+psc[3]*data1$c1+psc[4]*data1$c4))
data1$px1z0= px1z0<-exp(psc[1]+psc[3]*data1$c1+psc[4]*data1$c4)/(1+exp(psc[1]+psc[3]*data1$c1+psc[4]*data1$c4))
data1$px0z1= px0z1<-1-px1z1
data1$px0z0= px0z0<-1-px1z0
 
data1$ps=ps_true<-data1$ps_true
data1$w[data1$trte==0]<-mean((1-data1$ps[data1$trte==0]))/(1-data1$ps[data1$trte==0])
data1$w[data1$trte==1]<-mean((data1$ps[data1$trte==1]))/(data1$ps[data1$trte==1])

data_val$w[data_val$trt==0]<-mean((1-data_val$pval[data_val$trt==0]))/(1-data_val$pval[data_val$trt==0])
data_val$w[data_val$trt==1]<-mean((data_val$pval[data_val$trt==1]))/(data_val$pval[data_val$trt==1])

  lik = function(beta){ 
  	beta0<-beta[1]
  	beta1<-beta[2]
  	beta2<-beta[3]
	beta3<-beta[4]
	beta4<-beta[5]
	beta5<-beta[6]
	beta6<-beta[7]
	beta7<-beta[8]
	
	
   	a1<-sum(data1$w*y*z *log ( (1-px0z1) * (1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+px0z1 * (1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))
  	 
	a2<-sum(y*(1-z)*data1$w*log (px1z0  * (1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+(1-px1z0) * (1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))
  	
  	a3<-sum((1-y)*z *data1$w*log ((1-px0z1) * (1-1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+px0z1 * (1-1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))
  	
  	a4<-sum((1-y)*(1-z)*data1$w*log (px1z0  * (1-1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+(1-px1z0) * (1-1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))

  	a5<-sum(yy*data_val$w*log(1/(1+exp(-(beta0+beta1*xx+cc1*beta2+cc2*beta3+cc3*beta4+cc4*beta5+cc5*beta6+cc6*beta7)))))  	
  	a6<-sum((1-yy)*data_val$w*log(1-1/(1+exp(-(beta0+beta1*xx+cc1*beta2+cc2*beta3+cc3*beta4+cc4*beta5+cc5*beta6+cc6*beta7)))))
  	return(a1+a2+a3+a4+a5+a6)
  	}
  	
  	
  	
  res <- maxLik(lik, start=c(0,0,0,0,0,0,0,0)) 
  betas[m,countF:countL]<-coef(res)
    
a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+1])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
b<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
betas_marg[m,mm1]<-mean(a-b)


# #model 14, y~trte+c, ps_ep
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


data1$ps=ps_true<-data1$ps_ep
data1$w[data1$trte==0]<-mean((1-data1$ps[data1$trte==0]))/(1-data1$ps[data1$trte==0])
data1$w[data1$trte==1]<-mean((data1$ps[data1$trte==1]))/(data1$ps[data1$trte==1])


   lik = function(beta){ 
  	beta0<-beta[1]
  	beta1<-beta[2]
  	beta2<-beta[3]
	beta3<-beta[4]
	beta4<-beta[5]
	beta5<-beta[6]
	beta6<-beta[7]
	beta7<-beta[8]
	
	
   	a1<-sum(data1$w*y*z *log ( (1-px0z1) * (1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+px0z1 * (1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))
  	 
	a2<-sum(y*(1-z)*data1$w*log (px1z0  * (1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+(1-px1z0) * (1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))
  	
  	a3<-sum((1-y)*z *data1$w*log ((1-px0z1) * (1-1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+px0z1 * (1-1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))
  	
  	a4<-sum((1-y)*(1-z)*data1$w*log (px1z0  * (1-1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+(1-px1z0) * (1-1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))

  	a5<-sum(yy*data_val$w*log(1/(1+exp(-(beta0+beta1*xx+cc1*beta2+cc2*beta3+cc3*beta4+cc4*beta5+cc5*beta6+cc6*beta7)))))  	
  	a6<-sum((1-yy)*data_val$w*log(1-1/(1+exp(-(beta0+beta1*xx+cc1*beta2+cc2*beta3+cc3*beta4+cc4*beta5+cc5*beta6+cc6*beta7)))))
  	return(a1+a2+a3+a4+a5+a6)
  	}
  res <- maxLik(lik, start=c(0,0,0,0,0,0,0,0)) 
  betas[m,countF:countL]<-coef(res) 

    
a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+1])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
b<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
betas_marg[m,mm1]<-mean(a-b)

# #model 15, y~trte+c, ps_adj
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


data1$ps=ps_true<-data1$ps_adj
data1$w[data1$trte==0]<-mean((1-data1$ps[data1$trte==0]))/(1-data1$ps[data1$trte==0])
data1$w[data1$trte==1]<-mean((data1$ps[data1$trte==1]))/(data1$ps[data1$trte==1])


  lik = function(beta){ 
  	beta0<-beta[1]
  	beta1<-beta[2]
  	beta2<-beta[3]
	beta3<-beta[4]
	beta4<-beta[5]
	beta5<-beta[6]
	beta6<-beta[7]
	beta7<-beta[8]
	
	
  	   	a1<-sum(data1$w*y*z *log ( (1-px0z1) * (1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+px0z1 * (1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))
  	 
	a2<-sum(y*(1-z)*data1$w*log (px1z0  * (1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+(1-px1z0) * (1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))
  	
  	a3<-sum((1-y)*z *data1$w*log ((1-px0z1) * (1-1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+px0z1 * (1-1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))
  	
  	a4<-sum((1-y)*(1-z)*data1$w*log (px1z0  * (1-1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+(1-px1z0) * (1-1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))

  	a5<-sum(yy*data_val$w*log(1/(1+exp(-(beta0+beta1*xx+cc1*beta2+cc2*beta3+cc3*beta4+cc4*beta5+cc5*beta6+cc6*beta7)))))  	
  	a6<-sum((1-yy)*data_val$w*log(1-1/(1+exp(-(beta0+beta1*xx+cc1*beta2+cc2*beta3+cc3*beta4+cc4*beta5+cc5*beta6+cc6*beta7)))))
  	return(a1+a2+a3+a4+a5+a6)
  	}
  res <- maxLik(lik, start=c(0,0,0,0,0,0,0,0)) 

  betas[m,countF:countL]<-coef(res)
    
a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+1])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
b<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,countF+2])*data1$c1-as.numeric(betas[m,countF+3])*data1$c2-as.numeric(betas[m,countF+4])*data1$c3-as.numeric(betas[m,countF+5])*data1$c4-as.numeric(betas[m,countF+6])*data1$c5-as.numeric(betas[m,countF+7])*data1$c6))
betas_marg[m,mm1]<-mean(a-b)




#model 16, y~trte, ps_true, 
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


data1$ps=ps_true<-data1$ps_true
data1$w[data1$trte==0]<-mean((1-data1$ps[data1$trte==0]))/(1-data1$ps[data1$trte==0])
data1$w[data1$trte==1]<-mean((data1$ps[data1$trte==1]))/(data1$ps[data1$trte==1])


  lik = function(beta){ 
  	beta0<-beta[1]
  	beta1<-beta[2]

	
	
	
  	 	a1<-sum(data1$w*y*z*log ( (1-px0z1) * (1/(1+exp(-(beta0+beta1))))+px0z1 * (1/(1+exp(-(beta0))))))
  	a2<-sum(data1$w*y*(1-z) *log (px1z0  * (1/(1+exp(-(beta0+beta1))))+ (1-px1z0) * (1/(1+exp(-(beta0))))))
  	a3<-sum(data1$w*(1-y)*z *log ( (1-px0z1) * (1-1/(1+exp(-(beta0+beta1))))+px0z1 * (1-1/(1+exp(-(beta0))))))
  	a4<-sum(data1$w*(1-y)*(1-z) *log (px1z0  * (1-1/(1+exp(-(beta0+beta1))))+ (1-px1z0) * (1-1/(1+exp(-(beta0))))))
   	a5<-sum(data_val$w*yy*log(1/(1+exp(-(beta0+beta1*xx)))))
   	a6<-sum(data_val$w*(1-yy)*log(1-1/(1+exp(-(beta0+beta1*xx)))))
   	return(a1+a2+a3+a4+a5+a6)
   	

  	}
  res <- maxLik(lik, start=c(0,0)) 
  betas[m,countF:(countF+1)]<-coef(res)
  betas[m,(countF+2): countL]<-'NA'
  
  a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,(countF+1)])))
b<-1/(1+exp(-as.numeric(betas[m,countF])))
betas_marg[m,mm1]<-mean(a-b) 

#model 17, y~trte, ps_ep
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


data1$ps=ps_true<-data1$ps_ep
data1$w[data1$trte==0]<-mean((1-data1$ps[data1$trte==0]))/(1-data1$ps[data1$trte==0])
data1$w[data1$trte==1]<-mean((data1$ps[data1$trte==1]))/(data1$ps[data1$trte==1])


  lik = function(beta){ 
  	beta0<-beta[1]
  	beta1<-beta[2]

	
	
 	 	a1<-sum(data1$w*y*z*log ( (1-px0z1) * (1/(1+exp(-(beta0+beta1))))+px0z1 * (1/(1+exp(-(beta0))))))
  	a2<-sum(data1$w*y*(1-z) *log (px1z0  * (1/(1+exp(-(beta0+beta1))))+ (1-px1z0) * (1/(1+exp(-(beta0))))))
  	a3<-sum(data1$w*(1-y)*z *log ( (1-px0z1) * (1-1/(1+exp(-(beta0+beta1))))+px0z1 * (1-1/(1+exp(-(beta0))))))
  	a4<-sum(data1$w*(1-y)*(1-z) *log (px1z0  * (1-1/(1+exp(-(beta0+beta1))))+ (1-px1z0) * (1-1/(1+exp(-(beta0))))))
   	a5<-sum(data_val$w*yy*log(1/(1+exp(-(beta0+beta1*xx)))))
   	a6<-sum(data_val$w*(1-yy)*log(1-1/(1+exp(-(beta0+beta1*xx)))))
   	return(a1+a2+a3+a4+a5+a6)
   	

  	}
  res <- maxLik(lik, start=c(0,0)) 
  betas[m,countF:(countF+1)]<-coef(res)
  betas[m,(countF+2): countL]<-'NA'
  
  a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,(countF+1)])))
b<-1/(1+exp(-as.numeric(betas[m,countF])))
betas_marg[m,mm1]<-mean(a-b) 


#model 18, y~trte, ps_adj
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


data1$ps=ps_true<-data1$ps_adj
data1$w[data1$trte==0]<-mean((1-data1$ps[data1$trte==0]))/(1-data1$ps[data1$trte==0])
data1$w[data1$trte==1]<-mean((data1$ps[data1$trte==1]))/(data1$ps[data1$trte==1])


  lik = function(beta){ 
  	beta0<-beta[1]
  	beta1<-beta[2]

	
	
 	 	a1<-sum(data1$w*y*z*log ( (1-px0z1) * (1/(1+exp(-(beta0+beta1))))+px0z1 * (1/(1+exp(-(beta0))))))
  	a2<-sum(data1$w*y*(1-z) *log (px1z0  * (1/(1+exp(-(beta0+beta1))))+ (1-px1z0) * (1/(1+exp(-(beta0))))))
  	a3<-sum(data1$w*(1-y)*z *log ( (1-px0z1) * (1-1/(1+exp(-(beta0+beta1))))+px0z1 * (1-1/(1+exp(-(beta0))))))
  	a4<-sum(data1$w*(1-y)*(1-z) *log (px1z0  * (1-1/(1+exp(-(beta0+beta1))))+ (1-px1z0) * (1-1/(1+exp(-(beta0))))))
   	a5<-sum(data_val$w*yy*log(1/(1+exp(-(beta0+beta1*xx)))))
   	a6<-sum(data_val$w*(1-yy)*log(1-1/(1+exp(-(beta0+beta1*xx)))))
   	return(a1+a2+a3+a4+a5+a6)
   	

  	}
  res <- maxLik(lik, start=c(0,0)) 
  betas[m,countF:(countF+1)]<-coef(res)
  betas[m,(countF+2): countL]<-'NA'
  
  a<-1/(1+exp(-as.numeric(betas[m,countF])-as.numeric(betas[m,(countF+1)])))
b<-1/(1+exp(-as.numeric(betas[m,countF])))
betas_marg[m,mm1]<-mean(a-b) 

}


save(betas, betas_marg, file = paste(getwd(),"/results/WeightPS",s,".RData", sep=""))
}