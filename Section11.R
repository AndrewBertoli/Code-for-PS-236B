Section 11 b-Experimental Design

# Bias when clustering

# We will have eight classes of different sizes

Class=c(rep(1,6),rep(2,8),rep(3,10),rep(4,12),rep(5,14),rep(6,16),rep(7,18),rep(8,20))
Student=c(1:6,1:8,1:10,1:12,1:14,1:16,1:18,1:20)

# Students will get better grades in general and experience larger treatment effects if they are in smaller classes

Yit=c(
99,97,96,98,90,95,    
95,94,93,93,91,92,89,91,   
89,88,85,87,88,86,83,84,86,85,  
82,81,82,81,80,82,79,78,77,82,81,80,
76,77,75,76,78,79,72,73,71,77,78,76,77,79,
72,71,70,72,71,72,72,73,71,71,68,76,70,70,71,72,
68,67,69,68,62,63,62,63,61,60,61,59,60,68,69,70,68,67,
59,58,57,62,61,63,57,53,51,54,52,59,60,61,57,57,58,57,58,59
)

TreatmentEffects=Classes=c(rep(10,6),rep(9,8),rep(8,10),rep(7,12),rep(6,14),rep(5,16),rep(4,18),rep(3,20))

ATE=mean(TreatmentEffects)

ATE

Yic=Yit-TreatmentEffects

data=data.frame(Class,Student,Yit,Yic)

DifInMeans=function(T,C){
return(mean(T)-mean(C))}	

# In the following function, N is the total number of units, M is the number of clusters, mt is the number of clusters 
# to be assigned to treatment, nj.t is a vector containing the number of units in each treated cluster, and nj.c is the 
# number of units in each control cluster. 

DesRajEstimator=function(T,C,N,M,mt,nj.t,nj.c,k){ 
mc=M-mt
bar.Y.t=rep(0,length(nj.t))
bar.Y.c=rep(0,length(nj.t))
for(j in 1:length(nj.t)){bar.Y.t[j]=mean(T[1:nj.t[j]]);T=T[-c(1:nj.t[j])]}
for(j in 1:length(nj.c)){bar.Y.c[j]=mean(C[1:nj.c[j]]);C=C[-c(1:nj.c[j])]}
estimator=1/N*(M/mt*sum(bar.Y.t*nj.t-k*(nj.t-N/mt))-M/mc*sum(bar.Y.c*nj.c-k*(nj.c-N/mc)))
return(estimator)
}

diff.in.means.ests=rep(0,10000)

Des.Raj.ests=rep(0,10000)

for(i in 1:10000){
treated.classes=sort(sample(c(1:8),4))
Treatment=data$Yit[data$Class%in%treated.classes]
Control=	data$Yic[!data$Class%in%treated.classes]
diff.in.means.ests[i]=DifInMeans(Treatment,Control)
nj.t=rep(0,4);nj.c=rep(0,4)
for(k in 1:4){nj.t[k]=length(which(data$Class==treated.classes[k]))}
for(k in 1:4){nj.c[k]=length(which(data$Class==c(1:8)[-treated.classes][k]))}
Des.Raj.ests[i]=DesRajEstimator(Treatment,Control,N=104,M=8,mt=4,nj.t=nj.t,nj.c=nj.c,k=75)
}

Ests=data.frame(Estimate=c(diff.in.means.ests,Des.Raj.ests))
Ests$Method=c(rep("Dif In Means",1000),rep("Des Raj",1000))

ggplot(Ests, aes(Estimate, fill = Method)) + geom_density(alpha = 0.2)+geom_vline(xintercept=ATE, lty=2) + 
annotate("text", x=ATE, y=0.0015, angle=90, label="True Effect", size=4.2) + ylab("Density") + xlab("Estimated Treatment Effect") 
+ labs(title="Comparing Difference in Means and Des Raj")








# Rerandomization

covariates=matrix(rnorm(250,0,1),nrow=50,ncol=5)

# Let's define our balance criterian as the mean of the p-values being above 0.7.

balance.check=function(pvalues){
	if(mean(pvalues)<=0.7){return(0)}
	if(mean(pvalues)>0.7){return(1)}
	}

# The following set of nested functions will test whether any treatment assignment meets the balance criterian.

getpvalue=function(cov){
	return(t.test(cov[1:(length(cov)/2)],cov[(length(cov)/2+1):length(cov)])$p.value)
	}

# This function takes two matricies of covariates and returns their mean p-value

pvalfun=function(newtcovariates,newccovariates){
	
	covariate=rbind(newtcovariates,newccovariates)
	pvalues=apply(covariate,2,getpvalue)
	return(balance.check(pvalues))
	
	}

pfun=function(vector){
	
newtcovariates=covariates[vector==1,]
newccovariates=covariates[vector==0,]	
pvalueloss=pvalfun(newtcovariates,newccovariates)
	return(pvalueloss)
	
	}

possible.treatment.assignments=matrix(0,nrow=10000,ncol=50)

for(i in 1:10000){
possible.treatment.assignments[i,]=sample(c(rep(0,25),rep(1,25)),50)}

good.balance.index=apply(possible.treatment.assignments,1,pfun)

sum(good.balance.index)

# If this number is too small, increase i and run the for loop again. 

balanced.t.assign=possible.treatment.assignments[which(good.balance.index==1),]

realassignment=balanced.t.assign[1,]

outcomes=apply(covariates[1:length(covariates[,1]),],1,sum)+rnorm(50,0,0.5)+realassignment

realtstat=mean(outcomes[which(realassignment==1)])-mean(outcomes[which(realassignment==0)])
	
# To get the p-value

fake.t.stats=rep(0,length(balanced.t.assign[,1])-1)

for(i in 2:length(balanced.t.assign[,1])){
fake.assignment=balanced.t.assign[i,]
fake.t.stats[i]=mean(outcomes[which(fake.assignment==1)])-mean(outcomes[which(fake.assignment==0)])}

p.val=length(which(abs(fake.t.stats)>=abs(realtstat)))/length(fake.t.stats)

p.val

# Now let's see what would happen if we did a normal experiment. We will just use the entire possible.treatment.assignments 
matrix instead of the balanced.t.assign matrix 

realassignment2=possible.treatment.assignments[1,]

outcomes2=apply(covariates[1:length(covariates[,1]),],1,sum)+rnorm(50,0,0.5)+realassignment2

realtstat2=mean(outcomes2[which(realassignment2==1)])-mean(outcomes2[which(realassignment2==0)])
	
# To get the p-value

fake.t.stats2=rep(0,length(possible.treatment.assignments[,1])-1)

for(i in 2:length(possible.treatment.assignments[,1])){
fake.assignment2=possible.treatment.assignments[i,]
fake.t.stats2[i]=mean(outcomes2[which(fake.assignment2==1)])-mean(outcomes2[which(fake.assignment2==0)])}

p.val=length(which(abs(fake.t.stats2)>=abs(realtstat2)))/length(fake.t.stats2)

p.val


Dists=data.frame(Distribution=c(fake.t.stats,fake.t.stats2))
Dists$Method=c(rep("Rerandomization",length(fake.t.stats)),rep("Normal Experiment",length(fake.t.stats2)))

ggplot(Dists, aes(Distribution, fill = Method)) + geom_density(alpha = 0.2)+geom_vline(xintercept=1, lty=1) 
+ geom_vline(xintercept=0, lty=1) + geom_vline(xintercept=realtstat, lty=2, col="blue") + geom_vline(xintercept=realtstat2, lty=2, col="pink") 
+ annotate("text", x=0.8, y=0.6, angle=90, label="True Effect", size=4.2) + ylab("Density") + xlab("Randomization Distribution") 
+ labs(title="Comparing Rerandomization to a Normal Experiment")
