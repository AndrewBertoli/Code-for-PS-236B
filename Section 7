# Section 7-Regression Techniques

library(Rlab)
library(ggplot2)

# Dealing with a binary outcome

# Let's say we have 1000 students in a class. We randomly give 500 of them unlimited free coffee from Starbucks. 
# The outcome of interest is whether students pass the class. There is a constant treatment effect, where everyone
# has a 10% increase in their chance of passing the class if they get the free Starbucks. People's probability of 
# passing will be determined by the following function.

Age=runif(100,25,30)

IQ=runif(100,135,150)

Treat=sample(c(0,1),100,replace=TRUE)

Prob.Of.Passing=0.005*Age+0.005*IQ+0.1*Treat

Pass=rbern(100,Prob.Of.Passing)

data=data.frame(Pass,Treat,Age,IQ,Prob.Of.Passing)

# So first note that the all we have to do in this example is run a t-test

t.test(data[Treat==1,]$Pass,data[Treat==0,]$Pass)

# However, let's say that we want to control for age and noise as a robustness check. We will pretend here that 
# we have measurements of both of these covariates with no noise. Since we don't have any interactions, we won't 
# include them in this model, although we normally should. So our model is

model=lm(Pass~Treat+Age+IQ,data)

summary(model)

# So this estimate is not guarenteed to be unbiased. This model will be unbaised (since it is the correct model)

model=lm(Prob.Of.Passing~Treat+Age+IQ,data)

summary(model)

# But unfortunately we don't observe the underlying probabilies. We just see who passes and doesn't pass. So let's go 
# back to this model.

# So some of our predicted probabilities fall outside [0,1], which is a concern (you may need to rerun the code to see this)

model=lm(Pass~Treat+Age+IQ,data)

# The problem is that some of the y_hats (underlying probabilities) fall outside [0,1]

min(model$fitted.values)

max(model$fitted.values)



# Now let's try probit model

model=glm(Pass~Treat+Age+IQ,family=binomial(link="probit"),data)
summary(model)
min(model$fitted.values)
max(model$fitted.values)

# To estimate the marginal effect of the treatment at the mean of Age and IQ, 

variables.T0=matrix(colMeans(data[c("Treat", "Age", "IQ")]),nrow=1)
variables.T0=as.data.frame(variables.T0)
colnames(variables.T0)=c("Treat","Age","IQ")
variables.T0[1]=0

variables.T1=variables.T0
variables.T1[1]=1

y0 = predict(model, newdata=variables.T0, type="response")

y1 = predict(model, newdata=variables.T1, type="response")

y1-y0




# Now we can try the logit model

model=glm(Pass~Treat+Age+IQ,family=binomial(link="logit"),data)
summary(model)
min(model$fitted.values)
max(model$fitted.values)

# To estimate the marginal effect of the treatment at the mean of Age and IQ, 

variables.T0=matrix(colMeans(data[c("Treat", "Age", "IQ")]),nrow=1)
variables.T0=as.data.frame(variables.T0)
colnames(variables.T0)=c("Treat","Age","IQ")
variables.T0[1]=0

variables.T1=variables.T0
variables.T1[1]=1

y0 = predict(model, newdata=variables.T0, type="response")

y1 = predict(model, newdata=variables.T1, type="response")

y1-y0



# Let's do a quick Monte Carlo simulation to see how our Beta_hat_1's compare for the LPM, Probit, and Logit


LPM.Estimates=rep(0,10000)
LPM.P.Vals=rep(0,10000)
for(i in 1:10000){
Age=runif(1000,27,30)
IQ=runif(1000,140,150)
Treat=sample(c(0,1),1000,replace=TRUE)
Prob.Of.Passing=0.005*Age+0.005*IQ+0.1*Treat
Pass=rbern(100,Prob.Of.Passing)
data=data.frame(Pass,Treat,Age,IQ,Prob.Of.Passing)
model=lm(Pass~Treat+Age+IQ,data)
LPM.Estimates[i]=summary(model)$coefficients[2,1]
LPM.P.Vals[i]=summary(model)$coefficients[2,4]
}

mean(LPM.Estimates)

plot(density(LPM.Estimates))

sum(LPM.P.Vals<0.05)/length(LPM.P.Vals)

# Now let's try running it with no treatment effect

LPM.Estimates=rep(0,10000)
LPM.P.Vals=rep(0,10000)
for(i in 1:10000){
Age=runif(1000,27,30)
IQ=runif(1000,140,150)
Treat=sample(c(0,1),1000,replace=TRUE)
Prob.Of.Passing=0.005*Age+0.005*IQ+0*Treat
Pass=rbern(100,Prob.Of.Passing)
data=data.frame(Pass,Treat,Age,IQ,Prob.Of.Passing)
model=lm(Pass~Treat+Age+IQ,data)
LPM.Estimates[i]=summary(model)$coefficients[2,1]
LPM.P.Vals[i]=summary(model)$coefficients[2,4]
}

mean(LPM.Estimates)

plot(density(LPM.Estimates))

sum(LPM.P.Vals<0.05)/length(LPM.P.Vals)



# Now lets do the same two tests for probit

Probit.Estimates=rep(0,1000)
Probit.P.Vals=rep(0,1000)

for(i in 1:1000){
Age=runif(1000,27,30)
IQ=runif(1000,140,150)
Treat=sample(c(0,1),1000,replace=TRUE)
Prob.Of.Passing=0.005*Age+0.005*IQ+0.1*Treat
Pass=rbern(1000,Prob.Of.Passing)
data=data.frame(Pass,Treat,Age,IQ,Prob.Of.Passing)
model=glm(Pass~Treat+Age+IQ,family=binomial(link="probit"),data)
variables.T0=matrix(colMeans(data[c("Treat", "Age", "IQ")]),nrow=1)
variables.T0=as.data.frame(variables.T0)
colnames(variables.T0)=c("Treat","Age","IQ")
variables.T0[1]=0
variables.T1=variables.T0
variables.T1[1]=1
y0 = predict(model, newdata=variables.T0, type="response")
y1 = predict(model, newdata=variables.T1, type="response")
Probit.Estimates[i]=y1-y0
Probit.P.Vals[i]=summary(model)$coefficients[2,4]
}

mean(Probit.Estimates)

plot(density(Probit.Estimates))

sum(Probit.P.Vals<0.05)/length(Probit.P.Vals)


# Probit with no treatment effect

Probit.Estimates=rep(0,1000)
Probit.P.Vals=rep(0,1000)

for(i in 1:1000){
Age=runif(1000,27,30)
IQ=runif(1000,140,150)
Treat=sample(c(0,1),1000,replace=TRUE)
Prob.Of.Passing=0.005*Age+0.005*IQ+0*Treat
Pass=rbern(1000,Prob.Of.Passing)
data=data.frame(Pass,Treat,Age,IQ,Prob.Of.Passing)
model=glm(Pass~Treat+Age+IQ,family=binomial(link="probit"),data)
variables.T0=matrix(colMeans(data[c("Treat", "Age", "IQ")]),nrow=1)
variables.T0=as.data.frame(variables.T0)
colnames(variables.T0)=c("Treat","Age","IQ")
variables.T0[1]=0
variables.T1=variables.T0
variables.T1[1]=1
y0 = predict(model, newdata=variables.T0, type="response")
y1 = predict(model, newdata=variables.T1, type="response")
Probit.Estimates[i]=y1-y0
Probit.P.Vals[i]=summary(model)$coefficients[2,4]
}

mean(Probit.Estimates)

plot(density(Probit.Estimates))

sum(Probit.P.Vals<0.05)/length(Probit.P.Vals)



# Now let's do logit with a treatment effect

Logit.Estimates=rep(0,1000)
Logit.P.Vals=rep(0,1000)

for(i in 1:1000){
Age=runif(1000,27,30)
IQ=runif(1000,140,150)
Treat=sample(c(0,1),1000,replace=TRUE)
Prob.Of.Passing=0.005*Age+0.005*IQ+0.1*Treat
Pass=rbern(1000,Prob.Of.Passing)
data=data.frame(Pass,Treat,Age,IQ,Prob.Of.Passing)
model=glm(Pass~Treat+Age+IQ,family=binomial(link="logit"),data)
variables.T0=matrix(colMeans(data[c("Treat", "Age", "IQ")]),nrow=1)
variables.T0=as.data.frame(variables.T0)
colnames(variables.T0)=c("Treat","Age","IQ")
variables.T0[1]=0
variables.T1=variables.T0
variables.T1[1]=1
y0 = predict(model, newdata=variables.T0, type="response")
y1 = predict(model, newdata=variables.T1, type="response")
Logit.Estimates[i]=y1-y0
Logit.P.Vals[i]=summary(model)$coefficients[2,4]
}

mean(Logit.Estimates)

plot(density(Logit.Estimates))

sum(Logit.P.Vals<0.05)/length(Logit.P.Vals)

# And logit without a treatment effect

Logit.Estimates=rep(0,1000)
Logit.P.Vals=rep(0,1000)

for(i in 1:1000){
Age=runif(1000,27,30)
IQ=runif(1000,140,150)
Treat=sample(c(0,1),1000,replace=TRUE)
Prob.Of.Passing=0.005*Age+0.005*IQ+0*Treat
Pass=rbern(1000,Prob.Of.Passing)
data=data.frame(Pass,Treat,Age,IQ,Prob.Of.Passing)
model=glm(Pass~Treat+Age+IQ,family=binomial(link="logit"),data)
variables.T0=matrix(colMeans(data[c("Treat", "Age", "IQ")]),nrow=1)
variables.T0=as.data.frame(variables.T0)
colnames(variables.T0)=c("Treat","Age","IQ")
variables.T0[1]=0
variables.T1=variables.T0
variables.T1[1]=1
y0 = predict(model, newdata=variables.T0, type="response")
y1 = predict(model, newdata=variables.T1, type="response")
Logit.Estimates[i]=y1-y0
Logit.P.Vals[i]=summary(model)$coefficients[2,4]
}

mean(Logit.Estimates)

plot(density(Logit.Estimates))

sum(Logit.P.Vals<0.05)/length(Logit.P.Vals)





# Now let's make the histograms for the simulations with no treatment effect

Ests=data.frame(Estimate=c(LPM.Estimates,Probit.Estimates,Logit.Estimates))
Ests$Method=c(rep("LPM",1000),rep("Probit",1000),rep("Logit",1000))

ggplot(Ests, aes(Estimate, fill = Method)) + geom_density(alpha = 0.2)+geom_vline(xintercept=0, lty=2) + 
annotate("text", x=-0.005, y=3, angle=90, label="True Effect", size=4.2) + ylab("Density") + xlab("Estimated Treatment Effect") 
+ labs(title="Comparing LPM, Probit, and Logit")

# Now let's see what happens when we add fixed effects. For these simulations, we will fix the treatment effect 
# at 0 and that the fixed effects are all 0.


# Probit with no treatment effect

LPM.Estimates=rep(0,1000)
LPM.P.Vals=rep(0,1000)

for(i in 1:1000){
Age=runif(100,27,30)
IQ=runif(100,140,150)
C1=c(rep(1,4),rep(0,96));C2=c(rep(0,4),rep(1,4),rep(0,92));C3=c(rep(0,8),rep(1,4),rep(0,88));C4=c(rep(0,12),rep(1,4),
rep(0,84));C5=c(rep(0,16),rep(1,4),rep(0,80));C6=c(rep(0,20),rep(1,4),rep(0,76));C7=c(rep(0,24),rep(1,4),rep(0,72));
C8=c(rep(0,28),rep(1,4),rep(0,68));C9=c(rep(0,32),rep(1,4),rep(0,64));C10=c(rep(0,36),rep(1,4),rep(0,60));
C11=c(rep(0,40),rep(1,4),rep(0,56));C12=c(rep(0,44),rep(1,4),rep(0,52));C13=c(rep(0,48),rep(1,4),rep(0,48));C14=c(rep(0,52),
rep(1,4),rep(0,44));C15=c(rep(0,56),rep(1,4),rep(0,40));C16=c(rep(0,60),rep(1,4),rep(0,36));C17=c(rep(0,64),rep(1,4),rep(0,32));
C18=c(rep(0,68),rep(1,4),rep(0,28));C19=c(rep(0,72),rep(1,4),rep(0,24));C20=c(rep(0,76),rep(1,4),rep(0,20));C21=c(rep(0,80),
rep(1,4),rep(0,16));C22=c(rep(0,84),rep(1,4),rep(0,12));C23=c(rep(0,88),rep(1,4),rep(0,8));C24=c(rep(0,92),rep(1,4),rep(0,4))
Treat=sample(c(0,1),100,replace=TRUE)
Prob.Of.Passing=0.005*Age+0.005*IQ+0*Treat
Pass=rbern(100,Prob.Of.Passing)
data=data.frame(Pass,Treat,Age,IQ,C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,C15,C16,C17,C18,C19,C20,C21,C22,C23,C24,
Prob.Of.Passing)
model=lm(Pass~Treat+Age+IQ+C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13+C14+C15+C16+C17+C18+C19+C20+C21+C22+C23+C24,data)
LPM.Estimates[i]=summary(model)$coefficients[2,1]
LPM.P.Vals[i]=summary(model)$coefficients[2,4]
}

mean(LPM.Estimates)

plot(density(LPM.Estimates))

sum(LPM.P.Vals<0.05)/length(LPM.P.Vals)

# So this looks good


# No let's try probit with no treatment effect

Probit.Estimates=rep(0,1000)
Probit.P.Vals=rep(0,1000)

for(i in 1:1000){
Age=runif(100,27,30)
IQ=runif(100,140,150)
C1=c(rep(1,4),rep(0,96));C2=c(rep(0,4),rep(1,4),rep(0,92));C3=c(rep(0,8),rep(1,4),rep(0,88));C4=c(rep(0,12),rep(1,4),rep(0,84));
C5=c(rep(0,16),rep(1,4),rep(0,80));C6=c(rep(0,20),rep(1,4),rep(0,76));C7=c(rep(0,24),rep(1,4),rep(0,72));C8=c(rep(0,28),rep(1,4),
rep(0,68));C9=c(rep(0,32),rep(1,4),rep(0,64));C10=c(rep(0,36),rep(1,4),rep(0,60));C11=c(rep(0,40),rep(1,4),rep(0,56));
C12=c(rep(0,44),rep(1,4),rep(0,52));C13=c(rep(0,48),rep(1,4),rep(0,48));C14=c(rep(0,52),rep(1,4),rep(0,44));C15=c(rep(0,56),
rep(1,4),rep(0,40));C16=c(rep(0,60),rep(1,4),rep(0,36));C17=c(rep(0,64),rep(1,4),rep(0,32));C18=c(rep(0,68),rep(1,4),rep(0,28));
C19=c(rep(0,72),rep(1,4),rep(0,24));C20=c(rep(0,76),rep(1,4),rep(0,20));C21=c(rep(0,80),rep(1,4),rep(0,16));C22=c(rep(0,84),
rep(1,4),rep(0,12));C23=c(rep(0,88),rep(1,4),rep(0,8));C24=c(rep(0,92),rep(1,4),rep(0,4))
Treat=sample(c(0,1),100,replace=TRUE)
Prob.Of.Passing=0.005*Age+0.005*IQ+0*Treat
Pass=rbern(100,Prob.Of.Passing)
data=data.frame(Pass,Treat,Age,IQ,C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,C15,C16,C17,C18,C19,C20,C21,C22,C23,C24,
Prob.Of.Passing)
model=glm(Pass~Treat+Age+IQ+C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13+C14+C15+C16+C17+C18+C19+C20+C21+C22+C23+C24,
family=binomial(link="probit"),data)
variables.T0=matrix(colMeans(data[,c("Treat", "Age", "IQ", "C1", "C2", "C3","C4","C5","C6","C7","C8","C9","C10","C11","C12",
"C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24")]),nrow=1)
variables.T0=as.data.frame(variables.T0)
colnames(variables.T0)=c("Treat","Age","IQ", "C1", "C2", "C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14",
"C15","C16","C17","C18","C19","C20","C21","C22","C23","C24")
variables.T0[1]=0
variables.T1=variables.T0
variables.T1[1]=1
y0 = predict(model, newdata=variables.T0, type="response")
y1 = predict(model, newdata=variables.T1, type="response")
Probit.Estimates[i]=y1-y0
Probit.P.Vals[i]=summary(model)$coefficients[2,4]
Probit.P.Vals[i]=summary(model)$coefficients[2,4]
}

mean(Probit.Estimates)

plot(density(Probit.Estimates))

sum(Probit.P.Vals<0.05)/length(Probit.P.Vals)

# This value will consistently be much larger than 0.05, which is a problem.





Logit.Estimates=rep(0,1000)
Logit.P.Vals=rep(0,1000)

for(i in 1:1000){
Age=runif(100,27,30)
IQ=runif(100,140,150)
C1=c(rep(1,4),rep(0,96));C2=c(rep(0,4),rep(1,4),rep(0,92));C3=c(rep(0,8),rep(1,4),rep(0,88));C4=c(rep(0,12),rep(1,4),rep(0,84));
C5=c(rep(0,16),rep(1,4),rep(0,80));C6=c(rep(0,20),rep(1,4),rep(0,76));C7=c(rep(0,24),rep(1,4),rep(0,72));C8=c(rep(0,28),rep(1,4),
rep(0,68));C9=c(rep(0,32),rep(1,4),rep(0,64));C10=c(rep(0,36),rep(1,4),rep(0,60));C11=c(rep(0,40),rep(1,4),rep(0,56));
C12=c(rep(0,44),rep(1,4),rep(0,52));C13=c(rep(0,48),rep(1,4),rep(0,48));C14=c(rep(0,52),rep(1,4),rep(0,44));C15=c(rep(0,56),
rep(1,4),rep(0,40));C16=c(rep(0,60),rep(1,4),rep(0,36));C17=c(rep(0,64),rep(1,4),rep(0,32));C18=c(rep(0,68),rep(1,4),rep(0,28));
C19=c(rep(0,72),rep(1,4),rep(0,24));C20=c(rep(0,76),rep(1,4),rep(0,20));C21=c(rep(0,80),rep(1,4),rep(0,16));C22=c(rep(0,84),
rep(1,4),rep(0,12));C23=c(rep(0,88),rep(1,4),rep(0,8));C24=c(rep(0,92),rep(1,4),rep(0,4))
Treat=sample(c(0,1),100,replace=TRUE)
Prob.Of.Passing=0.005*Age+0.005*IQ+0*Treat
Pass=rbern(100,Prob.Of.Passing)
data=data.frame(Pass,Treat,Age,IQ,C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,C15,C16,C17,C18,C19,C20,C21,C22,C23,C24,
Prob.Of.Passing)
model=glm(Pass~Treat+Age+IQ+C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13+C14+C15+C16+C17+C18+C19+C20+C21+C22+C23+C24,
family=binomial(link="logit"),data)
variables.T0=matrix(colMeans(data[,c("Treat", "Age", "IQ", "C1", "C2", "C3","C4","C5","C6","C7","C8","C9","C10","C11","C12",
"C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24")]),nrow=1)
variables.T0=as.data.frame(variables.T0)
colnames(variables.T0)=c("Treat","Age","IQ", "C1", "C2", "C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15",
"C16","C17","C18","C19","C20","C21","C22","C23","C24")
variables.T0[1]=0
variables.T1=variables.T0
variables.T1[1]=1
y0 = predict(model, newdata=variables.T0, type="response")
y1 = predict(model, newdata=variables.T1, type="response")
Logit.Estimates[i]=y1-y0
Logit.P.Vals[i]=summary(model)$coefficients[2,4]
}

mean(Logit.Estimates)

plot(density(Logit.Estimates))

sum(Logit.P.Vals<0.05)/length(Logit.P.Vals)

# We have the same problem here as with the probit.





# Lastly, let's try the three models with a treatment effect of 10% increase in the likelihood of passing

LPM.Estimates=rep(0,1000)
LPM.P.Vals=rep(0,1000)

for(i in 1:1000){
Age=runif(100,27,30)
IQ=runif(100,140,150)
C1=c(rep(1,4),rep(0,96));C2=c(rep(0,4),rep(1,4),rep(0,92));C3=c(rep(0,8),rep(1,4),rep(0,88));C4=c(rep(0,12),rep(1,4),rep(0,84));
C5=c(rep(0,16),rep(1,4),rep(0,80));C6=c(rep(0,20),rep(1,4),rep(0,76));C7=c(rep(0,24),rep(1,4),rep(0,72));C8=c(rep(0,28),rep(1,4),
rep(0,68));C9=c(rep(0,32),rep(1,4),rep(0,64));C10=c(rep(0,36),rep(1,4),rep(0,60));C11=c(rep(0,40),rep(1,4),rep(0,56));
C12=c(rep(0,44),rep(1,4),rep(0,52));C13=c(rep(0,48),rep(1,4),rep(0,48));C14=c(rep(0,52),rep(1,4),rep(0,44));C15=c(rep(0,56),
rep(1,4),rep(0,40));C16=c(rep(0,60),rep(1,4),rep(0,36));C17=c(rep(0,64),rep(1,4),rep(0,32));C18=c(rep(0,68),rep(1,4),rep(0,28));
C19=c(rep(0,72),rep(1,4),rep(0,24));C20=c(rep(0,76),rep(1,4),rep(0,20));C21=c(rep(0,80),rep(1,4),rep(0,16));C22=c(rep(0,84),
rep(1,4),rep(0,12));C23=c(rep(0,88),rep(1,4),rep(0,8));C24=c(rep(0,92),rep(1,4),rep(0,4))
Treat=sample(c(0,1),100,replace=TRUE)
Prob.Of.Passing=0.005*Age+0.005*IQ+0.1*Treat
Pass=rbern(100,Prob.Of.Passing)
data=data.frame(Pass,Treat,Age,IQ,C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,C15,C16,C17,C18,C19,C20,C21,C22,C23,C24,
Prob.Of.Passing)
model=lm(Pass~Treat+Age+IQ+C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13+C14+C15+C16+C17+C18+C19+C20+C21+C22+C23+C24,data)
LPM.Estimates[i]=summary(model)$coefficients[2,1]
LPM.P.Vals[i]=summary(model)$coefficients[2,4]
}

mean(LPM.Estimates)

plot(density(LPM.Estimates))

sum(LPM.P.Vals<0.05)/length(LPM.P.Vals)






Probit.Estimates=rep(0,1000)
Probit.P.Vals=rep(0,1000)

for(i in 1:1000){
Age=runif(100,27,30)
IQ=runif(100,140,150)
C1=c(rep(1,4),rep(0,96));C2=c(rep(0,4),rep(1,4),rep(0,92));C3=c(rep(0,8),rep(1,4),rep(0,88));C4=c(rep(0,12),rep(1,4),rep(0,84));
C5=c(rep(0,16),rep(1,4),rep(0,80));C6=c(rep(0,20),rep(1,4),rep(0,76));C7=c(rep(0,24),rep(1,4),rep(0,72));C8=c(rep(0,28),rep(1,4),
rep(0,68));C9=c(rep(0,32),rep(1,4),rep(0,64));C10=c(rep(0,36),rep(1,4),rep(0,60));C11=c(rep(0,40),rep(1,4),rep(0,56));
C12=c(rep(0,44),rep(1,4),rep(0,52));C13=c(rep(0,48),rep(1,4),rep(0,48));C14=c(rep(0,52),rep(1,4),rep(0,44));C15=c(rep(0,56),
rep(1,4),rep(0,40));C16=c(rep(0,60),rep(1,4),rep(0,36));C17=c(rep(0,64),rep(1,4),rep(0,32));C18=c(rep(0,68),rep(1,4),rep(0,28));
C19=c(rep(0,72),rep(1,4),rep(0,24));C20=c(rep(0,76),rep(1,4),rep(0,20));C21=c(rep(0,80),rep(1,4),rep(0,16));C22=c(rep(0,84),
rep(1,4),rep(0,12));C23=c(rep(0,88),rep(1,4),rep(0,8));C24=c(rep(0,92),rep(1,4),rep(0,4))
Treat=sample(c(0,1),100,replace=TRUE)
Prob.Of.Passing=0.005*Age+0.005*IQ+0.1*Treat
Pass=rbern(100,Prob.Of.Passing)
data=data.frame(Pass,Treat,Age,IQ,C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,C15,C16,C17,C18,C19,C20,C21,C22,C23,C24,
Prob.Of.Passing)
model=glm(Pass~Treat+Age+IQ+C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13+C14+C15+C16+C17+C18+C19+C20+C21+C22+C23+C24,
family=binomial(link="probit"),data)
variables.T0=matrix(colMeans(data[,c("Treat", "Age", "IQ", "C1", "C2", "C3","C4","C5","C6","C7","C8","C9","C10","C11","C12",
"C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24")]),nrow=1)
variables.T0=as.data.frame(variables.T0)
colnames(variables.T0)=c("Treat","Age","IQ", "C1", "C2", "C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15",
"C16","C17","C18","C19","C20","C21","C22","C23","C24")
variables.T0[1]=0
variables.T1=variables.T0
variables.T1[1]=1
y0 = predict(model, newdata=variables.T0, type="response")
y1 = predict(model, newdata=variables.T1, type="response")
Probit.Estimates[i]=y1-y0
Probit.P.Vals[i]=summary(model)$coefficients[2,4]
Probit.P.Vals[i]=summary(model)$coefficients[2,4]
}

mean(Probit.Estimates)

plot(density(Probit.Estimates))

sum(Probit.P.Vals<0.05)/length(Probit.P.Vals)

# This value will consistently be much larger than 0.05, which is a problem.





Logit.Estimates=rep(0,1000)
Logit.P.Vals=rep(0,1000)

for(i in 1:1000){
Age=runif(100,27,30)
IQ=runif(100,140,150)
C1=c(rep(1,4),rep(0,96));C2=c(rep(0,4),rep(1,4),rep(0,92));C3=c(rep(0,8),rep(1,4),rep(0,88));C4=c(rep(0,12),rep(1,4),rep(0,84));
C5=c(rep(0,16),rep(1,4),rep(0,80));C6=c(rep(0,20),rep(1,4),rep(0,76));C7=c(rep(0,24),rep(1,4),rep(0,72));C8=c(rep(0,28),rep(1,4),
rep(0,68));C9=c(rep(0,32),rep(1,4),rep(0,64));C10=c(rep(0,36),rep(1,4),rep(0,60));C11=c(rep(0,40),rep(1,4),rep(0,56));
C12=c(rep(0,44),rep(1,4),rep(0,52));C13=c(rep(0,48),rep(1,4),rep(0,48));C14=c(rep(0,52),rep(1,4),rep(0,44));C15=c(rep(0,56),
rep(1,4),rep(0,40));C16=c(rep(0,60),rep(1,4),rep(0,36));C17=c(rep(0,64),rep(1,4),rep(0,32));C18=c(rep(0,68),rep(1,4),rep(0,28));
C19=c(rep(0,72),rep(1,4),rep(0,24));C20=c(rep(0,76),rep(1,4),rep(0,20));C21=c(rep(0,80),rep(1,4),rep(0,16));C22=c(rep(0,84),
rep(1,4),rep(0,12));C23=c(rep(0,88),rep(1,4),rep(0,8));C24=c(rep(0,92),rep(1,4),rep(0,4))
Treat=sample(c(0,1),100,replace=TRUE)
Prob.Of.Passing=0.005*Age+0.005*IQ+0.1*Treat
Pass=rbern(100,Prob.Of.Passing)
data=data.frame(Pass,Treat,Age,IQ,C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,C15,C16,C17,C18,C19,C20,C21,C22,C23,C24,
Prob.Of.Passing)
model=glm(Pass~Treat+Age+IQ+C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13+C14+C15+C16+C17+C18+C19+C20+C21+C22+C23+C24,
family=binomial(link="logit"),data)
variables.T0=matrix(colMeans(data[,c("Treat", "Age", "IQ", "C1", "C2", "C3","C4","C5","C6","C7","C8","C9","C10","C11","C12",
"C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24")]),nrow=1)
variables.T0=as.data.frame(variables.T0)
colnames(variables.T0)=c("Treat","Age","IQ", "C1", "C2", "C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15",
"C16","C17","C18","C19","C20","C21","C22","C23","C24")
variables.T0[1]=0
variables.T1=variables.T0
variables.T1[1]=1
y0 = predict(model, newdata=variables.T0, type="response")
y1 = predict(model, newdata=variables.T1, type="response")
Logit.Estimates[i]=y1-y0
Logit.P.Vals[i]=summary(model)$coefficients[2,4]
}

mean(Logit.Estimates)

plot(density(Logit.Estimates))

sum(Logit.P.Vals<0.05)/length(Logit.P.Vals)

# So the marginalized estimates for Logit and Probit are biased.






# Seeming Unrelated Regression Code

library(ggplot2)
library(systemfit)

Matrix1=matrix(c(130,125,150,140,155,145,120,29,45,12,32,18,24,52),nrow=7)
Matrix2=matrix(c(rep(1,7),29,45,12,32,18,24,52,16,16,22,12,16,16,20),nrow=7)
colnames(Matrix1)=c("IQ","Age")
colnames(Matrix2)=c("Intercept2","Age","Edu")
rownames(Matrix1)=c("Bob","Kevin","Amy","Claire","Jason","Sarah","Ben")
rownames(Matrix2)=c("Bob","Kevin","Amy","Claire","Jason","Sarah","Ben")

Matrix1=data.frame(Matrix1)
Matrix2=data.frame(Matrix2)

# Now we will compute the outcomes

Error1=rnorm(7,0,2000)

Error2=Error1/10+rnorm(7,0,10)

Matrix1$Outcome=with(Matrix1, 50000+70*IQ+1000*Age+Error1) # These are the incomes
Matrix2$Outcome=with(Matrix2, 20000+20*Age
+100*Edu+Error2) # These are the taxes

Matrix1$Intercept2=0
Matrix1$Edu=0

Matrix2$IQ=0

data=rbind(Matrix1,Matrix2)

X=matrix(c(rep(1,14),data$Intercept2,data$IQ,data$Age[1:7],rep(0,14),data$Age[8:14],data$Edu),nrow=14)

Y=c(data$Outcome)

model1=lm(Outcome~IQ+Age,Matrix1)
residuals1=model1$residuals

model2=lm(Outcome~Age+Edu,Matrix2)
residuals2=model2$residuals

df1=nrow(Matrix1)-3 # Since we have 3 parameters-Intercept, IQ, and Age

df2=nrow(Matrix2)-3 # Since we have 3 parameters-Intercept, Age, and Edu

sigma11=1/df1*sum(residuals1*residuals1)
sigma12=1/df1*sum(residuals1*residuals2)
sigma22=1/df2*sum(residuals2*residuals2)

Gamma=matrix(c(sigma11,sigma12,sigma12,sigma22),nrow=2)

W=kronecker(Gamma,diag(7))

Beta=solve(t(X)%*%solve(W)%*%X)%*%t(X)%*%solve(W)%*%Y

Beta

Cov=solve(t(X)%*%solve(W)%*%X)

Cov

# Faster way to do this

# Make a dataframe with all variables.

data=data.frame(Matrix1$Outcome,Matrix2$Outcome,Matrix1$IQ,Matrix1$Age,Matrix2$Edu)

colnames(data)=c("Income","Taxes","IQ","Age","Edu")

model1=Income~IQ+Age
model2=Taxes~Age+Edu

model=systemfit(list(Income.Reg=model1,Taxes.Reg=model2),method="SUR",data=data)

summary(model)

SUR.ests=rep(0,1000)
OLS.ests=rep(0,1000)
for(i in 1:1000){
Error1=rnorm(7,0,2000)
Error2=Error1/10+rnorm(7,0,10)
Matrix1$Outcome=with(Matrix1, 50000+70*IQ+1000*Age+Error1) # These are the incomes
Matrix2$Outcome=with(Matrix2, 20000+20*Age
+100*Edu+Error2) # These are the taxes
data=data.frame(Matrix1$Outcome,Matrix2$Outcome,Matrix1$IQ,Matrix1$Age,Matrix2$Edu)
colnames(data)=c("Income","Taxes","IQ","Age","Edu")
SUR.ests[i]=summary(systemfit(list(Income.Reg=model1,Taxes.Reg=model2),method="SUR",data=data))$coefficients[3,1]
OLS.ests[i]=summary(systemfit(list(Income.Reg=model1,Taxes.Reg=model2),method="OLS",data=data))$coefficients[3,1]
}

Ests=data.frame(Estimate=c(SUR.ests,OLS.ests))
Ests$Method=c(rep("SUR",1000),rep("OLS",1000))

ggplot(Ests, aes(Estimate, fill = Method)) + geom_density(alpha = 0.2)+geom_vline(xintercept=1000, lty=2) + 
annotate("text", x=980, y=0.0015, angle=90, label="True Effect", size=4.2) + ylab("Density") + xlab("Estimated Treatment Effect") + 
labs(title="Comparing OLS and SUR")
