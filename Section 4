# This simulation illustrates why fixed effects are not guarenteed to resolve the problem of immigrants sorting 
# in the Swedish Immigrants APSR study. For simplicity, assume that there are only two municipalities, and there 
# is only one covariate that matters (high/low skill-denoted 1/0). We will have three types of immigrants, Turkish,
# Yugoslavian, and Eastern European. We will have 1000 of each of these types of immigrants, and the Eastern Europeans
# will have higher incomes.

Eastern.Europeans.Skill=c(rep(0,200),rep(1,800))

Turks.Skill=c(rep(0,300),rep(1,700))

Yugo.Skill=c(rep(0,250),rep(1,750))

data=data.frame(cbind(c(rep(0,1000),rep(1,1000),rep(0,1000)),c(rep(0,2000),rep(1,1000)),c(Eastern.Europeans.Skill,
Turks.Skill,Yugo.Skill)))

colnames(data)=c("Turkish","Yugoslavian","Skill")



# We will start with having half of each group in Municipality A, and this to be uncorrelated with skill

data$MunicipalityA=rep(c(0,1),1500)

# We will start by making the probability of rejection has the following form

data$Prob.Rejection=0.4+0.2*data$Turkish+0.25*data$Yugoslavian-0.3*data$Skill+0.4*data$MunicipalityA

# So in this model, being in Municipality A makes people more likely to be rejected

summary(lm(Prob.Rejection~Turkish+Yugoslavian+Skill+MunicipalityA,data))

# So when the world works this way, everything is fine

# However, what happens if more skill Eastern Europeans move to Municipality A and the less skilled Eastern Europeans 
# to move to Municipality B. We will move the first 100 in each group

data$MunicipalityA[which(data$Turkish==0 & data$Yugoslavian==0 & data$Skill==1 & data$MunicipalityA==0)[1:100]]=1 

data$MunicipalityA[which(data$Turkish==0 & data$Yugoslavian==0 & data$Skill==0 & data$MunicipalityA==1)[1:100]]=0


# Now we will recalculate the rejection probabilities and run our model

data$Prob.Rejection=0.4+0.2*data$Turkish+0.25*data$Yugoslavian-0.3*data$Skill+0.4*data$MunicipalityA

summary(lm(Prob.Rejection~Turkish+Yugoslavian+Skill+MunicipalityA,data))

# So no problem yet. Now let's make it so that Municipality A is less xenophobic then Municipality B.

data$Prob.Rejection=0.4+0.2*data$Turkish+0.25*data$Yugoslavian-0.3*data$Skill+0.4*data$MunicipalityA-
0.1*data$Turkish*data$MunicipalityA-0.2*data$Yugoslavian*data$MunicipalityA


# Since the two municipalities have the same number of people, the true average baseline rate of rejection ()


# and the true average treatment effect of being Turkish is 

(0.2+0.1)/2

# and the true average treatment effect for being Yugoslavian (rate for Eastrn Europeans with low skill) is

(0.25+0.05)/2

# However, this isn't what we get when we run the normal regression with fixed effects

summary(lm(Prob.Rejection~Turkish+Yugoslavian+Skill+MunicipalityA,data))

# So now our estimates are biased. This is because fixed effects just take out the mean for different municipalites,
# but they don't deal with the fact that the treatment effects (slopes) might vary across municipalites. 

# There are two ways to fix this problem. The first is to run the regression on both municipalities seperately and reweight.

summary(lm(Prob.Rejection~Turkish+Yugoslavian+Skill,data[data$MunicipalityA==1,]))

summary(lm(Prob.Rejection~Turkish+Yugoslavian+Skill,data[data$MunicipalityA==0,]))

# So the estimated baseline rate (rate for Eastrn Europeans with low skill) is

(0.4+0.8)/2

# and the estimted treatment effect of being Turkish is 

(0.2+0.1)/2

# and the estimated treatment effect for being Yugoslavian is 

(0.25+0.05)/2

# The second is to control for both fixed effects and the interactions (remember to demean)

data$MunicipalityA=data$MunicipalityA-mean(data$MunicipalityA)

summary(lm(Prob.Rejection~Turkish+Yugoslavian+Skill+MunicipalityA+Turkish*MunicipalityA+Yugoslavian*MunicipalityA,data))


# However, the bias from the sorting is so small compared to the size of the effects reported in the paper that it 
# is not much of a concern in this case.







