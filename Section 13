# Section 13-Choosing Regressors

require(Rlab)

# First we will make are dataset for the class.

Midterm.Grade=runif(1100,50,100)
Paper.Grade.1=Midterm.Grade+rnorm(1100,0,5)
Paper.Grade.2=mean(Midterm.Grade+Paper.Grade.1)+rnorm(1100,0,5)
Participation.Grade=mean(Paper.Grade.1+Paper.Grade.1)+rnorm(1100,0,10)
Homework.Average=mean(Paper.Grade.2+Participation.Grade)+rnorm(1100,0,10)
Gender=rbern(1100,0.5)
Attendance=rbeta(1100,12,2)*Midterm.Grade/100
Year.Of.College=sample(1:4,1100,replace=TRUE)
Presentation=rbern(1100,0.2*Midterm.Grade/100)
Extra.Credit=rbern(1100,Midterm.Grade/100)
Appearance.in.Office.Hours=rbern(1100,0.4*Midterm.Grade/100)
Poli.Sci.Major=rbern(1100,0.8*Midterm.Grade/100)
Double.Major=rbern(1100,0.7*(100-Midterm.Grade)/100)
Athlete=rbern(1100,(100-Midterm.Grade)/100)
Sits.In.Front=rbern(1100,0.5*Midterm.Grade/100)
Hat.In.Class=rbern(1100,0.1*(100-Midterm.Grade)/100)
Texts.In.Class=rbern(1100,(100-Midterm.Grade)/100)
Facebook.In.Class=rbern(1100,(100-Midterm.Grade)/100)
Sharp.Dresser=rbern(1100,0.1*Midterm.Grade/100)
Questions.In.Lecture=rbern(1100,0.3*Midterm.Grade/100)

Score=0.10*Midterm.Grade+0.5*Paper.Grade.1+0.15*Paper.Grade.2+0.01*Participation.Grade*Paper.Grade.2+0.01*Homework.Average*
Attendance+5*Year.Of.College+3*Extra.Credit+10*Appearance.in.Office.Hours+5*Poli.Sci.Major-5*Double.Major-5*Athlete+10*
Sits.In.Front-5*Hat.In.Class-5*Texts.In.Class-5*Facebook.In.Class+0.1*Sharp.Dresser+5*Questions.In.Lecture+rnorm(1100,0,25)

Final.Grade=Score/max(Score)*100

data=data.frame(Midterm.Grade, Paper.Grade.1, Paper.Grade.2, Participation.Grade, Homework.Average, Gender, Attendance, 
Year.Of.College, Presentation, Extra.Credit, Appearance.in.Office.Hours, Poli.Sci.Major, Double.Major, Athlete, 
Sits.In.Front, Hat.In.Class, Texts.In.Class, Facebook.In.Class, Sharp.Dresser, Questions.In.Lecture, Final.Grade)

Last.Year=data[1:100,]
This.Year=data[101:1100,]

# So for OLS with all the predictors

model=lm(Final.Grade ~ Midterm.Grade + Paper.Grade.1 + Paper.Grade.2 + Participation.Grade + Homework.Average + Gender + 
Attendance + Year.Of.College + Presentation + Extra.Credit + Appearance.in.Office.Hours + Poli.Sci.Major + Double.Major +
Athlete + Sits.In.Front + Hat.In.Class + Texts.In.Class + Facebook.In.Class + Sharp.Dresser + Questions.In.Lecture, Last.Year)

predictions=predict(model,This.Year[,1:20])

MSE=mean((This.Year$Final.Grade-predictions)^2)

MSE




# Best-Subset Selection

require(leaps)

leaps.out=leaps(x=This.Year[,1:20],y=This.Year$Final.Grade,nbest=1)

# If we set the number of covariates k at 10, then the ones we should use are

leaps.out$which[10,]

# or

which(leaps.out$which[10,]==TRUE)

# So our model would be

combo=paste(colnames(Last.Year[,which(leaps.out$which[10,]==TRUE)]),collapse="+")

line=paste(c("Final.Grade ~ ",combo),collapse=" ")

line

model=lm(noquote(line), Last.Year)

predictions=predict(model,This.Year[,which(leaps.out$which[10,]==TRUE)])

MSE=mean((This.Year$Final.Grade-predictions)^2)

MSE


# Ridge Regression

require(ridge)

model=linearRidge(Final.Grade ~ Midterm.Grade + Paper.Grade.1 + Paper.Grade.2 + Participation.Grade + Homework.Average 
+ Gender + Attendance + Year.Of.College + Presentation + Extra.Credit + Appearance.in.Office.Hours + Poli.Sci.Major 
+ Double.Major + Athlete + Sits.In.Front + Hat.In.Class + Texts.In.Class + Facebook.In.Class + Sharp.Dresser 
+ Questions.In.Lecture, Last.Year, lambda="automatic")

predictions=predict(model,This.Year[,1:20])

MSE=mean((This.Year$Final.Grade-predictions)^2)

MSE



LASSO

require(lars)

model=lars(x=as.matrix(Last.Year[,1:20]),y=Last.Year$Final.Grade,type="lasso")

model[13]

# So 16 has the smallest loss

predictions=predict(model,This.Year[,1:20])[[4]][,16]

MSE=mean((This.Year$Final.Grade-predictions)^2)

MSE


# Now to use OLS with the variables selected by LASSO

model=lm(Final.Grade ~ Paper.Grade.1 + Paper.Grade.2 + Participation.Grade + Homework.Average + Gender + Year.Of.College 
+ Extra.Credit + Appearance.in.Office.Hours + Poli.Sci.Major + Double.Major + Athlete + Sits.In.Front + Hat.In.Class 
+ Facebook.In.Class + Sharp.Dresser + Questions.In.Lecture, Last.Year)

predictions=predict(model,This.Year[,1:20])

MSE=mean((This.Year$Final.Grade-predictions)^2)

MSE
