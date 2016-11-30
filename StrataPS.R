#script for proposed adjustment under subclassification
unlink(".RData*")
unlink(".RData")
library(xtable)
library(maxLik)
library(MatchIt)
library(twang)
if(file.exists(paste(getwd(), "/results/StrataPS",s,".RData", sep=""))==FALSE) 
{
set.seed(12498+s)

#siz<-100
siz=1
betas<-matrix(nrow=siz,ncol=250)
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

#model 1,y~trt, stratified by ps_true
countF<-9
countL<-countF+7
count<-1
mm1<-count+1
data2<-match.data((matchit(trt~c1+c2+c3+c4+c5+c6, data=data1, method="subclass", subclass=4, sub.by="all")))

bb<-matrix(nrow=4, ncol=10)
for (k in 1:4){
	dat<-data2[which(data2$subclass==k),]
	bb[k,1:2]<-glm(y~trt, family=binomial(logit), data=dat)$coefficients
	a<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,2])))
	b<-1/(1+exp(-as.numeric(bb[k,1])))
	bb[k,3]<-mean(a-b)
	
}

 	
betas[m,countF]<-mean(as.numeric(bb[,1]))
betas[m,countF+1]<-mean(as.numeric(bb[,2]))
betas[m,(countF+2):countL]<-'NA'

#marginal beta

betas_marg[m,mm1]<-mean(bb[,3])

#model 2,y~trt, stratified by ps_ep
countF<-countF+8
countL<-countF+7
mm1<-mm1+1
data2<-match.data(matchit(trt~c1+c2+c3+c4+c5+c6, data=data1, distance=data1$ps_ep, method="subclass", subclass=4,  sub.by="all"))

bb<-matrix(nrow=4, ncol=10)
for (k in 1:4){
	dat<-data2[which(data2$subclass==k),]
	bb[k,1:2]<-glm(y~trt, family=binomial(logit), data=dat)$coefficients
	a<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,2])))
	b<-1/(1+exp(-as.numeric(bb[k,1])))
	bb[k,3]<-mean(a-b)
	
}


 	
betas[m,countF]<-mean(as.numeric(bb[,1]))
betas[m,countF+1]<-mean(as.numeric(bb[,2]))
betas[m,(countF+2):countL]<-'NA'

#marginal beta

betas_marg[m,mm1]<-mean(bb[,3])

#model 3,y~trt, stratified by ps_adj
countF<-countF+8
countL<-countF+7
mm1<-mm1+1
data2<-match.data((matchit(trt~c1+c2+c3+c4+c5+c6, data=data1, distance=data1$ps_adj, method="subclass", subclass=4,  sub.by="all")))

bb<-matrix(nrow=4, ncol=10)
for (k in 1:4){
	dat<-data2[which(data2$subclass==k),]
	bb[k,1:2]<-glm(y~trt, family=binomial(logit), data=dat)$coefficients
	a<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,2])))
	b<-1/(1+exp(-as.numeric(bb[k,1])))
	bb[k,3]<-mean(a-b)
	
}
 	
betas[m,countF]<-mean(as.numeric(bb[,1]))
betas[m,countF+1]<-mean(as.numeric(bb[,2]))
betas[m,(countF+2):countL]<-'NA'

#marginal beta

betas_marg[m,mm1]<-mean(bb[,3])

#model 4,y~trte, stratified by ps_true
countF<-countF+8
countL<-countF+7
mm1<-mm1+1
data2<-match.data((matchit(trte~c1+c2+c3+c4+c5+c6, data=data1, distance=data1$ps_true, method="subclass", subclass=4,  sub.by="all")))

bb<-matrix(nrow=4, ncol=10)
for (k in 1:4){
	dat<-data2[which(data2$subclass==k),]
	bb[k,1:2]<-glm(y~trte, family=binomial(logit), data=dat)$coefficients
	a<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,2])))
	b<-1/(1+exp(-as.numeric(bb[k,1])))
	bb[k,3]<-mean(a-b)
	
}


 	
betas[m,countF]<-mean(as.numeric(bb[,1]))
betas[m,countF+1]<-mean(as.numeric(bb[,2]))
betas[m,(countF+2):countL]<-'NA'

#marginal beta

betas_marg[m,mm1]<-mean(bb[,3])


#model 5,y~trte, stratified by ps_ep
countF<-countF+8
countL<-countF+7
mm1<-mm1+1
data2<-match.data(matchit(trte~c1+c2+c3+c4+c5+c6, data=data1,  method="subclass", subclass=4,  sub.by="all"))

bb<-matrix(nrow=4, ncol=10)
for (k in 1:4){
	dat<-data2[which(data2$subclass==k),]
	bb[k,1:2]<-glm(y~trte, family=binomial(logit), data=dat)$coefficients
	a<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,2])))
	b<-1/(1+exp(-as.numeric(bb[k,1])))
	bb[k,3]<-mean(a-b)
	
}
 	
betas[m,countF]<-mean(as.numeric(bb[,1]))
betas[m,countF+1]<-mean(as.numeric(bb[,2]))
betas[m,(countF+2):countL]<-'NA'

#marginal beta

betas_marg[m,mm1]<-mean(bb[,3])

#model 6,y~trte, stratified by ps_adj
countF<-countF+8
countL<-countF+7
mm1<-mm1+1
data2<-match.data(matchit(trte~c1+c2+c3+c4+c5+c6, data=data1, distance=data1$ps_adj, method="subclass", subclass=4,  sub.by="all"))

bb<-matrix(nrow=4, ncol=10)
for (k in 1:4){
	dat<-data2[which(data2$subclass==k),]
	bb[k,1:2]<-glm(y~trte, family=binomial(logit), data=dat)$coefficients
	a<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,2])))
	b<-1/(1+exp(-as.numeric(bb[k,1])))
	bb[k,3]<-mean(a-b)
	
}
 	
betas[m,countF]<-mean(as.numeric(bb[,1]))
betas[m,countF+1]<-mean(as.numeric(bb[,2]))
betas[m,(countF+2):countL]<-'NA'

#marginal beta

betas_marg[m,mm1]<-mean(bb[,3])


# #model 7,y~trt+c, stratified by ps_true
countF<-countF+8
countL<-countF+7
mm1<-mm1+1
data2<-match.data((matchit(trt~c1+c2+c3+c4+c5+c6, data=data1, method="subclass", subclass=4,  sub.by="all")))
bb<-matrix(nrow=4, ncol=10)
for (k in 1:4){
	dat<-data2[which(data2$subclass==k),]
	bb[k,1:8]<-glm(y~trt+c1+c2+c3+c4+c5+c6, family=binomial(logit), data=dat)$coefficients
	a<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,2])-as.numeric(bb[k,3])*dat$c1-as.numeric(bb[k,4])*dat$c2-as.numeric(bb[k,5])*dat$c3-as.numeric(bb[k,6])*dat$c4-as.numeric(bb[k,7])*dat$c5-as.numeric(bb[k,8])*dat$c6))
b<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,3])*dat$c1-as.numeric(bb[k,4])*dat$c2-as.numeric(bb[k,5])*dat$c3-as.numeric(bb[k,6])*dat$c4-as.numeric(bb[k,7])*dat$c5-as.numeric(bb[k,8])*dat$c6))
bb[k,9]<-mean(a-b)
	
}

 	
betas[m,countF]<-mean(as.numeric(bb[,1]))
betas[m,countF+1]<-mean(as.numeric(bb[,2]))
betas[m,countF+2]<-mean(as.numeric(bb[,3]))
betas[m,countF+3]<-mean(as.numeric(bb[,4]))
betas[m,countF+4]<-mean(as.numeric(bb[,5]))
betas[m,countF+5]<-mean(as.numeric(bb[,6]))
betas[m,countF+6]<-mean(as.numeric(bb[,7]))
betas[m,countF+7]<-mean(as.numeric(bb[,8]))


#marginal beta

betas_marg[m,mm1]<-mean(bb[,9])


# #model 8,y~trt+c, stratified by ps_ep
countF<-countF+8
countL<-countF+7
mm1<-mm1+1
data2<-match.data(matchit(trt~c1+c2+c3+c4+c5+c6, data=data1, distance=data1$ps_ep, method="subclass", subclass=4,  sub.by="all"))

bb<-matrix(nrow=4, ncol=10)
for (k in 1:4){
	dat<-data2[which(data2$subclass==k),]
	bb[k,1:8]<-glm(y~trt+c1+c2+c3+c4+c5+c6, family=binomial(logit), data=dat)$coefficients
	a<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,2])-as.numeric(bb[k,3])*dat$c1-as.numeric(bb[k,4])*dat$c2-as.numeric(bb[k,5])*dat$c3-as.numeric(bb[k,6])*dat$c4-as.numeric(bb[k,7])*dat$c5-as.numeric(bb[k,8])*dat$c6))
b<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,3])*dat$c1-as.numeric(bb[k,4])*dat$c2-as.numeric(bb[k,5])*dat$c3-as.numeric(bb[k,6])*dat$c4-as.numeric(bb[k,7])*dat$c5-as.numeric(bb[k,8])*dat$c6))
bb[k,9]<-mean(a-b)
	
}

 	
betas[m,countF]<-mean(as.numeric(bb[,1]))
betas[m,countF+1]<-mean(as.numeric(bb[,2]))
betas[m,countF+2]<-mean(as.numeric(bb[,3]))
betas[m,countF+3]<-mean(as.numeric(bb[,4]))
betas[m,countF+4]<-mean(as.numeric(bb[,5]))
betas[m,countF+5]<-mean(as.numeric(bb[,6]))
betas[m,countF+6]<-mean(as.numeric(bb[,7]))
betas[m,countF+7]<-mean(as.numeric(bb[,8]))


#marginal beta

betas_marg[m,mm1]<-mean(bb[,9])

# #model 9,y~trt+c, stratified by ps_adj
countF<-countF+8
countL<-countF+7
mm1<-mm1+1
data2<-match.data(matchit(trt~c1+c2+c3+c4+c5+c6, data=data1, distance=data1$ps_adj, method="subclass", subclass=4,  sub.by="all"))
bb<-matrix(nrow=4, ncol=10)
for (k in 1:4){
dat<-data2[which(data2$subclass==k),]

	bb[k,1:8]<-glm(y~trt+c1+c2+c3+c4+c5+c6, family=binomial(logit), data=dat)$coefficients
		a<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,2])-as.numeric(bb[k,3])*dat$c1-as.numeric(bb[k,4])*dat$c2-as.numeric(bb[k,5])*dat$c3-as.numeric(bb[k,6])*dat$c4-as.numeric(bb[k,7])*dat$c5-as.numeric(bb[k,8])*dat$c6))
b<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,3])*dat$c1-as.numeric(bb[k,4])*dat$c2-as.numeric(bb[k,5])*dat$c3-as.numeric(bb[k,6])*dat$c4-as.numeric(bb[k,7])*dat$c5-as.numeric(bb[k,8])*dat$c6))
bb[k,9]<-mean(a-b)
	
}

 	
betas[m,countF]<-mean(as.numeric(bb[,1]))
betas[m,countF+1]<-mean(as.numeric(bb[,2]))
betas[m,countF+2]<-mean(as.numeric(bb[,3]))
betas[m,countF+3]<-mean(as.numeric(bb[,4]))
betas[m,countF+4]<-mean(as.numeric(bb[,5]))
betas[m,countF+5]<-mean(as.numeric(bb[,6]))
betas[m,countF+6]<-mean(as.numeric(bb[,7]))
betas[m,countF+7]<-mean(as.numeric(bb[,8]))


#marginal beta

betas_marg[m,mm1]<-mean(bb[,9])

# #model 10,y~trte+c, stratified by ps_true
countF<-countF+8
countL<-countF+7
mm1<-mm1+1


bb<-matrix(nrow=4, ncol=10)
data2<-match.data((matchit(trte~c1+c2+c3+c4+c5+c6, data=data1, distance=data1$ps_true, method="subclass", subclass=4,  sub.by="all")))
for (k in 1:4){
dat<-data2[which(data2$subclass==k),]

	bb[k,1:8]<-glm(y~trte+c1+c2+c3+c4+c5+c6, family=binomial(logit), data=dat)$coefficients
		a<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,2])-as.numeric(bb[k,3])*dat$c1-as.numeric(bb[k,4])*dat$c2-as.numeric(bb[k,5])*dat$c3-as.numeric(bb[k,6])*dat$c4-as.numeric(bb[k,7])*dat$c5-as.numeric(bb[k,8])*dat$c6))
b<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,3])*dat$c1-as.numeric(bb[k,4])*dat$c2-as.numeric(bb[k,5])*dat$c3-as.numeric(bb[k,6])*dat$c4-as.numeric(bb[k,7])*dat$c5-as.numeric(bb[k,8])*dat$c6))
bb[k,9]<-mean(a-b)
	
}

 	
betas[m,countF]<-mean(as.numeric(bb[,1]))
betas[m,countF+1]<-mean(as.numeric(bb[,2]))
betas[m,countF+2]<-mean(as.numeric(bb[,3]))
betas[m,countF+3]<-mean(as.numeric(bb[,4]))
betas[m,countF+4]<-mean(as.numeric(bb[,5]))
betas[m,countF+5]<-mean(as.numeric(bb[,6]))
betas[m,countF+6]<-mean(as.numeric(bb[,7]))
betas[m,countF+7]<-mean(as.numeric(bb[,8]))


#marginal beta

betas_marg[m,mm1]<-mean(bb[,9])

# #model 11,y~trte+c, stratified by ps_ep
countF<-countF+8
countL<-countF+7
mm1<-mm1+1
data2<-match.data(matchit(trte~c1+c2+c3+c4+c5+c6, data=data1, method="subclass", subclass=4, sub.by="all"))

bb<-matrix(nrow=4, ncol=10)

for (k in 1:4){
dat<-data2[which(data2$subclass==k),]

	bb[k,1:8]<-glm(y~trte+c1+c2+c3+c4+c5+c6, family=binomial(logit), data=dat)$coefficients
		a<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,2])-as.numeric(bb[k,3])*dat$c1-as.numeric(bb[k,4])*dat$c2-as.numeric(bb[k,5])*dat$c3-as.numeric(bb[k,6])*dat$c4-as.numeric(bb[k,7])*dat$c5-as.numeric(bb[k,8])*dat$c6))
b<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,3])*dat$c1-as.numeric(bb[k,4])*dat$c2-as.numeric(bb[k,5])*dat$c3-as.numeric(bb[k,6])*dat$c4-as.numeric(bb[k,7])*dat$c5-as.numeric(bb[k,8])*dat$c6))
bb[k,9]<-mean(a-b)
	
}

 	
betas[m,countF]<-mean(as.numeric(bb[,1]))
betas[m,countF+1]<-mean(as.numeric(bb[,2]))
betas[m,countF+2]<-mean(as.numeric(bb[,3]))
betas[m,countF+3]<-mean(as.numeric(bb[,4]))
betas[m,countF+4]<-mean(as.numeric(bb[,5]))
betas[m,countF+5]<-mean(as.numeric(bb[,6]))
betas[m,countF+6]<-mean(as.numeric(bb[,7]))
betas[m,countF+7]<-mean(as.numeric(bb[,8]))


#marginal beta

betas_marg[m,mm1]<-mean(bb[,9])
# #model 12,y~trte+c, stratified by ps_adj
countF<-countF+8
countL<-countF+7
mm1<-mm1+1
data2<-match.data(matchit(trte~c1+c2+c3+c4+c5+c6, data=data1, distance=data1$ps_adj, method="subclass",  sub.by="all", subclass=4))


bb<-matrix(nrow=4, ncol=10)

for (k in 1:4){
dat<-data2[which(data2$subclass==k),]

	bb[k,1:8]<-glm(y~trte+c1+c2+c3+c4+c5+c6, family=binomial(logit), data=dat)$coefficients
		a<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,2])-as.numeric(bb[k,3])*dat$c1-as.numeric(bb[k,4])*dat$c2-as.numeric(bb[k,5])*dat$c3-as.numeric(bb[k,6])*dat$c4-as.numeric(bb[k,7])*dat$c5-as.numeric(bb[k,8])*dat$c6))
b<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,3])*dat$c1-as.numeric(bb[k,4])*dat$c2-as.numeric(bb[k,5])*dat$c3-as.numeric(bb[k,6])*dat$c4-as.numeric(bb[k,7])*dat$c5-as.numeric(bb[k,8])*dat$c6))
bb[k,9]<-mean(a-b)
	
}

 	
betas[m,countF]<-mean(as.numeric(bb[,1]))
betas[m,countF+1]<-mean(as.numeric(bb[,2]))
betas[m,countF+2]<-mean(as.numeric(bb[,3]))
betas[m,countF+3]<-mean(as.numeric(bb[,4]))
betas[m,countF+4]<-mean(as.numeric(bb[,5]))
betas[m,countF+5]<-mean(as.numeric(bb[,6]))
betas[m,countF+6]<-mean(as.numeric(bb[,7]))
betas[m,countF+7]<-mean(as.numeric(bb[,8]))


#marginal beta

betas_marg[m,mm1]<-mean(bb[,9])


#likelihood correction for x
# #model 13, y~trte+c, ps_true
countF<-countF+8
countL<-countF+7
mm1<-mm1+1

psc<-c()
psc[1:4]<-glm(trt~trte+c1+c4, family=binomial(logit), data=data_val)$coefficients


data2<-match.data((matchit(trte~c1+c2+c3+c4+c5+c6, data=data1, method="subclass", distance=data1$ps_true, subclass=4,  sub.by="all"))) 
 bb<-matrix(nrow=4, ncol=10)
 data2val<-match.data((matchit(trt~c1+c2+c3+c4+c5+c6, data=data_val, method="subclass", subclass=4,  sub.by="all")))
 for (k in 1:4){
	 dat<-data2[which(data2$subclass==k),]
c1<-dat$c1
c2<-dat$c2
c3<-dat$c3
c4<-dat$c4
c5<-dat$c5
c6<-dat$c6
z<-dat$trte
y<-dat$y
 dat_val<-data2val[which(data2val$subclass==k),]
  xx<-dat_val$trt
  cc1<-dat_val$c1
  cc2<-dat_val$c2
  cc3<-dat_val$c3
  cc4<-dat_val$c4
  cc5<-dat_val$c5
  cc6<-dat_val$c6
  yy<-dat_val$y

px1z1<-exp(psc[1]+psc[2]+psc[3]*dat$c1+psc[4]*dat$c4)/(1+exp(psc[1]+psc[2]+psc[3]*dat$c1+psc[4]*dat$c4))
 px1z0<-exp(psc[1]+psc[3]*dat$c1+psc[4]*dat$c4)/(1+exp(psc[1]+psc[3]*dat$c1+psc[4]*dat$c4))
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
  	a2<-sum(y*(1-z) *log (px1z0  * (1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+ (1-px1z0) * (1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))
  	a3<-sum((1-y)*z *log ( (1-px0z1) * (1-1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+px0z1 * (1-1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))
  	a4<-sum((1-y)*(1-z) *log (px1z0  * (1-1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+ (1-px1z0) * (1-1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))
   	 a5<-sum(yy*log(1/(1+exp(-(beta0+beta1*xx+cc1*beta2+cc2*beta3+cc3*beta4+cc4*beta5+cc5*beta6+cc6*beta7)))))
   	 a6<-sum((1-yy)*log(1-1/(1+exp(-(beta0+beta1*xx+cc1*beta2+cc2*beta3+cc3*beta4+cc4*beta5+cc5*beta6+cc6*beta7)))))
      return(a1+a2+a3+a4+a5+a6)
  	
  	}
  res <- maxLik(lik, start=c(0,0,0,0,0,0,0,0)) 
  bb[k,1:8]<-coef(res)

	a<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,2])-as.numeric(bb[k,3])*dat$c1-as.numeric(bb[k,4])*dat$c2-as.numeric(bb[k,5])*dat$c3-as.numeric(bb[k,6])*dat$c4-as.numeric(bb[k,7])*dat$c5-as.numeric(bb[k,8])*dat$c6))
b<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,3])*dat$c1-as.numeric(bb[k,4])*dat$c2-as.numeric(bb[k,5])*dat$c3-as.numeric(bb[k,6])*dat$c4-as.numeric(bb[k,7])*dat$c5-as.numeric(bb[k,8])*dat$c6))
bb[k,9]<-mean(a-b)
	
}

 	
betas[m,countF]<-mean(as.numeric(bb[,1]))
betas[m,countF+1]<-mean(as.numeric(bb[,2]))
betas[m,countF+2]<-mean(as.numeric(bb[,3]))
betas[m,countF+3]<-mean(as.numeric(bb[,4]))
betas[m,countF+4]<-mean(as.numeric(bb[,5]))
betas[m,countF+5]<-mean(as.numeric(bb[,6]))
betas[m,countF+6]<-mean(as.numeric(bb[,7]))
betas[m,countF+7]<-mean(as.numeric(bb[,8]))


#marginal beta

betas_marg[m,mm1]<-mean(bb[,9])



# #model 14, y~trte+c, ps_ep
countF<-countF+8
countL<-countF+7
mm1<-mm1+1
data2<-match.data(matchit(trte~c1+c2+c3+c4+c5+c6, data=data1,  method="subclass", subclass=4,  sub.by="all")) 
 bb<-matrix(nrow=4, ncol=10)
 data2val<-match.data((matchit(trt~c1+c2+c3+c4+c5+c6, data=data_val, method="subclass", subclass=4,  sub.by="all")))
 for (k in 1:4){
	 dat<-data2[which(data2$subclass==k),]
c1<-dat$c1
c2<-dat$c2
c3<-dat$c3
c4<-dat$c4
c5<-dat$c5
c6<-dat$c6
z<-dat$trte
y<-dat$y
 dat_val<-data2val[which(data2val$subclass==k),]
    xx<-dat_val$trt
  cc1<-dat_val$c1
  cc2<-dat_val$c2
  cc3<-dat_val$c3
  cc4<-dat_val$c4
  cc5<-dat_val$c5
  cc6<-dat_val$c6
  yy<-dat_val$y

px1z1<-exp(psc[1]+psc[2]+psc[3]*dat$c1+psc[4]*dat$c4)/(1+exp(psc[1]+psc[2]+psc[3]*dat$c1+psc[4]*dat$c4))
 px1z0<-exp(psc[1]+psc[3]*dat$c1+psc[4]*dat$c4)/(1+exp(psc[1]+psc[3]*dat$c1+psc[4]*dat$c4))
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
  	a2<-sum(y*(1-z) *log (px1z0  * (1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+ (1-px1z0) * (1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))
  	a3<-sum((1-y)*z *log ( (1-px0z1) * (1-1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+px0z1 * (1-1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))
  	a4<-sum((1-y)*(1-z) *log (px1z0  * (1-1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+ (1-px1z0) * (1-1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))
   	 a5<-sum(yy*log(1/(1+exp(-(beta0+beta1*xx+cc1*beta2+cc2*beta3+cc3*beta4+cc4*beta5+cc5*beta6+cc6*beta7)))))
   	 a6<-sum((1-yy)*log(1-1/(1+exp(-(beta0+beta1*xx+cc1*beta2+cc2*beta3+cc3*beta4+cc4*beta5+cc5*beta6+cc6*beta7)))))
      return(a1+a2+a3+a4+a5+a6)
  	

  	}
  res <- maxLik(lik, start=c(0,0,0,0,0,0,0,0)) 
  bb[k,1:8]<-coef(res)

	a<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,2])-as.numeric(bb[k,3])*dat$c1-as.numeric(bb[k,4])*dat$c2-as.numeric(bb[k,5])*dat$c3-as.numeric(bb[k,6])*dat$c4-as.numeric(bb[k,7])*dat$c5-as.numeric(bb[k,8])*dat$c6))
b<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,3])*dat$c1-as.numeric(bb[k,4])*dat$c2-as.numeric(bb[k,5])*dat$c3-as.numeric(bb[k,6])*dat$c4-as.numeric(bb[k,7])*dat$c5-as.numeric(bb[k,8])*dat$c6))
bb[k,9]<-mean(a-b)
	
}

 	
betas[m,countF]<-mean(as.numeric(bb[,1]))
betas[m,countF+1]<-mean(as.numeric(bb[,2]))
betas[m,countF+2]<-mean(as.numeric(bb[,3]))
betas[m,countF+3]<-mean(as.numeric(bb[,4]))
betas[m,countF+4]<-mean(as.numeric(bb[,5]))
betas[m,countF+5]<-mean(as.numeric(bb[,6]))
betas[m,countF+6]<-mean(as.numeric(bb[,7]))
betas[m,countF+7]<-mean(as.numeric(bb[,8]))


#marginal beta

betas_marg[m,mm1]<-mean(bb[,9])


# #model 15, y~trte+c, ps_adj
countF<-countF+8
countL<-countF+7
mm1<-mm1+1
data2<-match.data(matchit(trte~c1+c2+c3+c4+c5+c6, data=data1, distance=data1$ps_adj, method="subclass", subclass=4,  sub.by="all")) 
 bb<-matrix(nrow=4, ncol=10)
 data2val<-match.data((matchit(trt~c1+c2+c3+c4+c5+c6, data=data_val, method="subclass", subclass=4)))
 for (k in 1:4){
	 dat<-data2[which(data2$subclass==k),]
c1<-dat$c1
c2<-dat$c2
c3<-dat$c3
c4<-dat$c4
c5<-dat$c5
c6<-dat$c6
z<-dat$trte
y<-dat$y
 dat_val<-data2val[which(data2val$subclass==k),]
  xx<-dat_val$trt
  cc1<-dat_val$c1
  cc2<-dat_val$c2
  cc3<-dat_val$c3
  cc4<-dat_val$c4
  cc5<-dat_val$c5
  cc6<-dat_val$c6
  yy<-dat_val$y

px1z1<-exp(psc[1]+psc[2]+psc[3]*dat$c1+psc[4]*dat$c4)/(1+exp(psc[1]+psc[2]+psc[3]*dat$c1+psc[4]*dat$c4))
 px1z0<-exp(psc[1]+psc[3]*dat$c1+psc[4]*dat$c4)/(1+exp(psc[1]+psc[3]*dat$c1+psc[4]*dat$c4))
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
  	a2<-sum(y*(1-z) *log (px1z0  * (1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+ (1-px1z0) * (1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))
  	a3<-sum((1-y)*z *log ( (1-px0z1) * (1-1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+px0z1 * (1-1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))
  	a4<-sum((1-y)*(1-z) *log (px1z0  * (1-1/(1+exp(-(beta0+beta1+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))+ (1-px1z0) * (1-1/(1+exp(-(beta0+beta2*c1+beta3*c2+beta4*c3+beta5*c4+beta6*c5+beta7*c6))))))
   	 a5<-sum(yy*log(1/(1+exp(-(beta0+beta1*xx+cc1*beta2+cc2*beta3+cc3*beta4+cc4*beta5+cc5*beta6+cc6*beta7)))))
   	 a6<-sum((1-yy)*log(1-1/(1+exp(-(beta0+beta1*xx+cc1*beta2+cc2*beta3+cc3*beta4+cc4*beta5+cc5*beta6+cc6*beta7)))))
      return(a1+a2+a3+a4+a5+a6)
  	

  	}
  res <- maxLik(lik, start=c(0,0,0,0,0,0,0,0)) 
  bb[k,1:8]<-coef(res)

	a<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,2])-as.numeric(bb[k,3])*dat$c1-as.numeric(bb[k,4])*dat$c2-as.numeric(bb[k,5])*dat$c3-as.numeric(bb[k,6])*dat$c4-as.numeric(bb[k,7])*dat$c5-as.numeric(bb[k,8])*dat$c6))
b<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,3])*dat$c1-as.numeric(bb[k,4])*dat$c2-as.numeric(bb[k,5])*dat$c3-as.numeric(bb[k,6])*dat$c4-as.numeric(bb[k,7])*dat$c5-as.numeric(bb[k,8])*dat$c6))
bb[k,9]<-mean(a-b)
	
}

 	
betas[m,countF]<-mean(as.numeric(bb[,1]))
betas[m,countF+1]<-mean(as.numeric(bb[,2]))
betas[m,countF+2]<-mean(as.numeric(bb[,3]))
betas[m,countF+3]<-mean(as.numeric(bb[,4]))
betas[m,countF+4]<-mean(as.numeric(bb[,5]))
betas[m,countF+5]<-mean(as.numeric(bb[,6]))
betas[m,countF+6]<-mean(as.numeric(bb[,7]))
betas[m,countF+7]<-mean(as.numeric(bb[,8]))


#marginal beta

betas_marg[m,mm1]<-mean(bb[,9])


#model 16, y~trte, ps_true
countF<-countF+8
countL<-countF+7
mm1<-mm1+1
data2<-match.data(matchit(trte~c1+c2+c3+c4+c5+c6, data=data1, method="subclass", distance=data1$ps_true, subclass=4, sub.by="all")) 
 bb<-matrix(nrow=4, ncol=10)
 data2val<-match.data((matchit(trt~c1+c2+c3+c4+c5+c6, data=data_val, method="subclass", subclass=4,  sub.by="all")))
 for (k in 1:4){
	 dat<-data2[which(data2$subclass==k),]
c1<-dat$c1
c2<-dat$c2
c3<-dat$c3
c4<-dat$c4
c5<-dat$c5
c6<-dat$c6
z<-dat$trte
y<-dat$y
 dat_val<-data2val[which(data2val$subclass==k),]

  xx<-dat_val$trt
  cc1<-dat_val$c1
  cc2<-dat_val$c2
  cc3<-dat_val$c3
  cc4<-dat_val$c4
  cc5<-dat_val$c5
  cc6<-dat_val$c6
  yy<-dat_val$y

px1z1<-exp(psc[1]+psc[2]+psc[3]*dat$c1+psc[4]*dat$c4)/(1+exp(psc[1]+psc[2]+psc[3]*dat$c1+psc[4]*dat$c4))
 px1z0<-exp(psc[1]+psc[3]*dat$c1+psc[4]*dat$c4)/(1+exp(psc[1]+psc[3]*dat$c1+psc[4]*dat$c4))
 px0z1<-1-px1z1
 px0z0<-1-px1z0




lik = function(beta){ 
  	beta0<-beta[1]
  	beta1<-beta[2]
	
  	a1<-sum(y*z*log ( (1-px0z1) * (1/(1+exp(-(beta0+beta1))))+px0z1 * (1/(1+exp(-(beta0))))))
  	a2<-sum(y*(1-z) *log (px1z0  * (1/(1+exp(-(beta0+beta1))))+ (1-px1z0) * (1/(1+exp(-(beta0))))))
  	a3<-sum((1-y)*z *log ( (1-px0z1) * (1-1/(1+exp(-(beta0+beta1))))+px0z1 * (1-1/(1+exp(-(beta0))))))
  	a4<-sum((1-y)*(1-z) *log (px1z0  * (1-1/(1+exp(-(beta0+beta1))))+ (1-px1z0) * (1-1/(1+exp(-(beta0))))))
   a5<-sum(yy*log(1/(1+exp(-(beta0+beta1*xx)))))
   a6<-sum((1-yy)*log(1-1/(1+exp(-(beta0+beta1*xx)))))
      
 return(a1+a2+a3+a4+a5+a6)


  	}
  res <- maxLik(lik, start=c(0,0)) 
   bb[k,1:2]<-coef(res)
  
  	a<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,2])))
	b<-1/(1+exp(-as.numeric(bb[k,1])))
	bb[k,3]<-mean(a-b)
	
}

 	
betas[m,countF]<-mean(as.numeric(bb[,1]))
betas[m,countF+1]<-mean(as.numeric(bb[,2]))
betas[m,(countF+2): countL]<-'NA'


#marginal beta

betas_marg[m,mm1]<-mean(bb[,3])



#model 17, y~trte, ps_ep
countF<-countF+8
countL<-countF+7
mm1<-mm1+1
data2<-match.data(matchit(trte~c1+c2+c3+c4+c5+c6, data=data1, method="subclass",  sub.by="all", subclass=4)) 
 bb<-matrix(nrow=4, ncol=10)
 data2val<-match.data((matchit(trt~c1+c2+c3+c4+c5+c6, data=data_val, method="subclass", subclass=4,  sub.by="all")))
 for (k in 1:4){
	 dat<-data2[which(data2$subclass==k),]
c1<-dat$c1
c2<-dat$c2
c3<-dat$c3
c4<-dat$c4
c5<-dat$c5
c6<-dat$c6
z<-dat$trte
y<-dat$y
 dat_val<-data2val[which(data2val$subclass==k),]

  xx<-dat_val$trt
  cc1<-dat_val$c1
  cc2<-dat_val$c2
  cc3<-dat_val$c3
  cc4<-dat_val$c4
  cc5<-dat_val$c5
  cc6<-dat_val$c6
  yy<-dat_val$y

px1z1<-exp(psc[1]+psc[2]+psc[3]*dat$c1+psc[4]*dat$c4)/(1+exp(psc[1]+psc[2]+psc[3]*dat$c1+psc[4]*dat$c4))
 px1z0<-exp(psc[1]+psc[3]*dat$c1+psc[4]*dat$c4)/(1+exp(psc[1]+psc[3]*dat$c1+psc[4]*dat$c4))
 px0z1<-1-px1z1
 px0z0<-1-px1z0




lik = function(beta){ 
  	beta0<-beta[1]
  	beta1<-beta[2]
	
  		
  	a1<-sum(y*z*log ( (1-px0z1) * (1/(1+exp(-(beta0+beta1))))+px0z1 * (1/(1+exp(-(beta0))))))
  	a2<-sum(y*(1-z) *log (px1z0  * (1/(1+exp(-(beta0+beta1))))+ (1-px1z0) * (1/(1+exp(-(beta0))))))
  	a3<-sum((1-y)*z *log ( (1-px0z1) * (1-1/(1+exp(-(beta0+beta1))))+px0z1 * (1-1/(1+exp(-(beta0))))))
  	a4<-sum((1-y)*(1-z) *log (px1z0  * (1-1/(1+exp(-(beta0+beta1))))+ (1-px1z0) * (1-1/(1+exp(-(beta0))))))
   a5<-sum(yy*log(1/(1+exp(-(beta0+beta1*xx)))))
   a6<-sum((1-yy)*log(1-1/(1+exp(-(beta0+beta1*xx)))))
      
 return(a1+a2+a3+a4+a5+a6)
  	}
  res <- maxLik(lik, start=c(0,0)) 
   bb[k,1:2]<-coef(res)
  
  	a<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,2])))
	b<-1/(1+exp(-as.numeric(bb[k,1])))
	bb[k,3]<-mean(a-b)
	
}

 	
betas[m,countF]<-mean(as.numeric(bb[,1]))
betas[m,countF+1]<-mean(as.numeric(bb[,2]))
betas[m,(countF+2): countL]<-'NA'


#marginal beta

betas_marg[m,mm1]<-mean(bb[,3])

#model 18, y~trte, ps_adj
countF<-countF+8
countL<-countF+7
mm1<-mm1+1
data2<-match.data(matchit(trte~c1+c2+c3+c4+c5+c6, data=data1, distance=data1$ps_adj, method="subclass", subclass=4,  sub.by="all")) 
 bb<-matrix(nrow=4, ncol=10)
 data2val<-match.data((matchit(trt~c1+c2+c3+c4+c5+c6, data=data_val, method="subclass", subclass=4,  sub.by="all")))
 for (k in 1:4){
	 dat<-data2[which(data2$subclass==k),]
c1<-dat$c1
c2<-dat$c2
c3<-dat$c3
c4<-dat$c4
c5<-dat$c5
c6<-dat$c6
z<-dat$trte
y<-dat$y
 dat_val<-data2val[which(data2val$subclass==k),]
  xx<-dat_val$trt
  cc1<-dat_val$c1
  cc2<-dat_val$c2
  cc3<-dat_val$c3
  cc4<-dat_val$c4
  cc5<-dat_val$c5
  cc6<-dat_val$c6
  yy<-dat_val$y

px1z1<-exp(psc[1]+psc[2]+psc[3]*dat$c1+psc[4]*dat$c4)/(1+exp(psc[1]+psc[2]+psc[3]*dat$c1+psc[4]*dat$c4))
 px1z0<-exp(psc[1]+psc[3]*dat$c1+psc[4]*dat$c4)/(1+exp(psc[1]+psc[3]*dat$c1+psc[4]*dat$c4))
 px0z1<-1-px1z1
 px0z0<-1-px1z0




lik = function(beta){ 
  	beta0<-beta[1]
  	beta1<-beta[2]
	
  		
  	a1<-sum(y*z*log ( (1-px0z1) * (1/(1+exp(-(beta0+beta1))))+px0z1 * (1/(1+exp(-(beta0))))))
  	a2<-sum(y*(1-z) *log (px1z0  * (1/(1+exp(-(beta0+beta1))))+ (1-px1z0) * (1/(1+exp(-(beta0))))))
  	a3<-sum((1-y)*z *log ( (1-px0z1) * (1-1/(1+exp(-(beta0+beta1))))+px0z1 * (1-1/(1+exp(-(beta0))))))
  	a4<-sum((1-y)*(1-z) *log (px1z0  * (1-1/(1+exp(-(beta0+beta1))))+ (1-px1z0) * (1-1/(1+exp(-(beta0))))))
   a5<-sum(yy*log(1/(1+exp(-(beta0+beta1*xx)))))
   a6<-sum((1-yy)*log(1-1/(1+exp(-(beta0+beta1*xx)))))
      
 return(a1+a2+a3+a4+a5+a6)

  	}
  res <- maxLik(lik, start=c(0,0)) 
   bb[k,1:2]<-coef(res)
  
  	a<-1/(1+exp(-as.numeric(bb[k,1])-as.numeric(bb[k,2])))
	b<-1/(1+exp(-as.numeric(bb[k,1])))
	bb[k,3]<-mean(a-b)
	
}

 	
betas[m,countF]<-mean(as.numeric(bb[,1]))
betas[m,countF+1]<-mean(as.numeric(bb[,2]))
betas[m,(countF+2): countL]<-'NA'


#marginal beta

betas_marg[m,mm1]<-mean(bb[,3])

}

save(betas, betas_marg, file = paste(getwd(),"/results/StrataPS",s,".RData", sep=""))
}