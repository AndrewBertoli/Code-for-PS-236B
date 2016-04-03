# Section 12b Code

# Quantile Regression

require(quantreg)

# So we will have a sample of 10000 people with food expenditures drawn from unif(100,500).

Baseline.Expenditures=runif(n=10000,min=100,max=500)

# Now we will give everyone a boost in income, drawn from Unif(0,5000)

Raise=runif(n=10000,min=0,max=5000)

# So for people with low expenditures, a raise will increase their expenditures a lot, whereas for people with high 
# expenditures, a raise will hardly increase their expenditures.

New.Expenditures=Baseline.Expenditures+(1/Baseline.Expenditures)*Raise

# The following two graphs illustrate how the treatment effect varies for different quantiles of the outcome.

plot(x=New.Expenditures,y=(New.Expenditures-Baseline.Expenditures)/Raise,ylab="Increase in Expendtures per Dollar of Raise",
xlab="New Expenditures",main="Treatment Effect as a Function of the Outcome",col="cornflowerblue")

plot(x=New.Expenditures,y=New.Expenditures-Baseline.Expenditures,ylab="Increase in Expendtures",xlab="New Expenditures",
main="Increase in Expenditures as a Function of the Outcome",col="cornflowerblue")

# Here's the graph we would normal generate with real world data

plot(x=Raise,y=New.Expenditures,xlab="Raise",ylab="Expenditures",main="Basic Scatterplot",col="cornflowerblue")

# Normal Regression

model=lm(New.Expenditures~Raise)
abline(model,col="coral",lwd=2)
summary(model)

# Now let's do median regression

model=rq(New.Expenditures~Raise, tau=0.5)
summary(model)

# Now lets make lines for a lot of different quantiles

for(i in seq(0.05,0.95,by=0.1)){
model=rq(New.Expenditures~Raise, tau=i)
abline(model,col="Brown",lwd=2)
print(summary(model))}

tests=rq(New.Expenditures~Raise, tau=1:99/100)
plot(tests, nrow = 1, ncol = 2)

# Testing for difference in slopes

Model1=rq(New.Expenditures~Raise, tau=0.05)
Model2=rq(New.Expenditures~Raise, tau=0.5)
Model3=rq(New.Expenditures~Raise, tau=0.95)

anova(Model1,Model2,Model3)

# Synthetic Matching

require(Synth)
data("basque")

# The data is in an object called basque, and is stored as panel data. The treated region is "Basque Country (Pais Vasco)".  

dataprep.out = dataprep(
    foo = basque,
    predictors = c("school.illit", "school.prim", "school.med",
      "school.high", "school.post.high", "invest"),
    predictors.op = "mean",
    time.predictors.prior = 1964:1969,
    special.predictors = list(
      list("gdpcap", 1960:1969, "mean"),
      list("sec.agriculture", seq(1961, 1969, 2), "mean"),
      list("sec.energy", seq(1961, 1969, 2), "mean"),
      list("sec.industry", seq(1961, 1969, 2), "mean"),
      list("sec.construction", seq(1961, 1969, 2), "mean"),
      list("sec.services.venta", seq(1961, 1969, 2), "mean"),
      list("sec.services.nonventa", seq(1961, 1969, 2), "mean"),
      list("popdens", 1969, "mean")),
    dependent = "gdpcap",
    unit.variable = "regionno",
    unit.names.variable = "regionname",
    time.variable = "year",
    treatment.identifier = 17, # This is the index for the Basque region
    controls.identifier = c(2:16, 18), # We are dropping all of Spain and the Basque Region 
    time.optimize.ssr = 1960:1969,
    time.plot = 1955:1997)
    
    
# To see the means of the covariates for the treated unit  
  
dataprep.out$X1  
    
# To see the means of the covariates for the control units
    
dataprep.out$X0

# To see the previous outcomes for the treated unit

dataprep.out$Z1
   
# To see the previous outcomes for the control units

dataprep.out$Z0

# Now we can run synth() 

synth.out = synth(data.prep.obj = dataprep.out, method = "BFGS")

# Now we can make the synth tables

synth.tables = synth.tab(dataprep.res = dataprep.out,
    synth.res = synth.out)

synth.tables

# Now we can make the graphs

path.plot(synth.res = synth.out, dataprep.res = dataprep.out,
    Ylab = "real per-capita GDP (1986 USD, thousand)", Xlab = "year",
    Ylim = c(0, 12), Legend = c("Basque country",
    "synthetic Basque country"), Legend.position = "bottomright")

abline(v=1970,col="red",lty=2)

# Now we can do the gap plot

gaps.plot(synth.res = synth.out, dataprep.res = dataprep.out,
    Ylab = "gap in real per-capita GDP (1986 USD, thousand)", Xlab = "year", 
    Main="Per-capita GDP Gap between Basque Country and Synthetic Basque Country", Ylim = c(-1.5, 1.5))

abline(v=1970,col="red",lty=2)

gaps=matrix(0,ncol=length(unique(basque$year)),nrow=(length(unique(basque$regionname))-1)) # Drop all of Spain
   
MSPE=rep(0, (length(unique(basque$regionname))-1))   
   
for(i in 1:(length(unique(basque$regionname))-1)){

dataprep.out1 = dataprep(
    foo = basque,
    predictors = c("school.illit", "school.prim", "school.med",
      "school.high", "school.post.high", "invest"),
    predictors.op = "mean",
    time.predictors.prior = 1964:1969,
    special.predictors = list(
      list("gdpcap", 1960:1969, "mean"),
      list("sec.agriculture", seq(1961, 1969, 2), "mean"),
      list("sec.energy", seq(1961, 1969, 2), "mean"),
      list("sec.industry", seq(1961, 1969, 2), "mean"),
      list("sec.construction", seq(1961, 1969, 2), "mean"),
      list("sec.services.venta", seq(1961, 1969, 2), "mean"),
      list("sec.services.nonventa", seq(1961, 1969, 2), "mean"),
      list("popdens", 1969, "mean")),
    dependent = "gdpcap",
    unit.variable = "regionno",
    unit.names.variable = "regionname",
    time.variable = "year",
    treatment.identifier = c(2:18)[i],
    controls.identifier = c(2:18)[-i],
    time.optimize.ssr = 1960:1969,
    time.plot = 1955:1997)

synth.out1 = synth(data.prep.obj = dataprep.out1, method = "BFGS", verbose=FALSE)

gaps[i,]=dataprep.out1$Y1plot - (dataprep.out1$Y0plot %*% synth.out1$solution.w)

MSPE[i]=synth.out1$loss.v

}  

# For all controls

for(i in 1:(length(unique(basque$regionname))-1)){
lines(unique(basque$year),gaps[i,],col="gray")}
   
# For the controls that have less than the fives times the MSPE of our tread unit   
   
treatedMSPE=synth.out$loss.v 
 
gaps.plot(synth.res = synth.out, dataprep.res = dataprep.out,
    Ylab = "gap in real per-capita GDP (1986 USD, thousand)", Xlab = "year",
    Ylim = c(-1.5, 1.5), Main = "Permutation Test")

abline(v=1970,col="red",lty=2) 
 
for(i in 1:(length(unique(basque$regionname))-2)){
if(MSPE[i]<=5*treatedMSPE){lines(unique(basque$year),gaps[i,],col="gray")}
if(MSPE[i]>5*treatedMSPE){print(c(i,"MSPE to Large"))}  
}
 
