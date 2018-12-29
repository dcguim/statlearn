# Install required packages and load dataset
install.packages("ISLR")
data(OJ,package="ISLR")
install.packages("ipred")
library(ipred)
summary(OJ)

# Aux function
mse <- function(y, y.hat) {
  mean((y - y.hat)^2)
}
### 1 - Univariate case ###
# Select univariate data
uniX <- OJ[,c(1,10)]

# Clean dataset
uniX <- na.omit(uniX)
nobs<-dim(uniX)[1]
# Switch CH to 1 and MM to 0, for compactness
uniX[,1]<-ifelse(uniX[,1] == "CH", 1, 0)
# Call logistic regression on the univariate case
mod.ulog <- glm(uniX$Purchase~.,data=uniX,family=binomial)
summary(mod.ulog)

# Preditct, Plot the prediction vs response & draw reg. line on linear scale
pred.ulog<-predict(mod.ulog, uniX,type="response")
x=seq(1,nobs,1)
plot(pred.ulog,col=uniX$Purchase+2)
lines(y=rep(0.5,nobs),x,type = "l")
# Plot on logit scale
pred.ulog<-predict(mod.ulog, uniX)
x=seq(1,nobs,1)
plot(pred.ulog,col=uniX$Purchase+2)
lines(y=rep(0,nobs),x,type = "l")

### 2 - Multivariate Case ###
# Clean & Factorize categorical explanatory variables
OJ<-na.omit(OJ)
OJ[,1]<-ifelse(OJ[,1] == "CH", 1, 0)
nobs<- dim(OJ)[1]
OJ$Store7<-ifelse(OJ$Store7 == "Yes", 1, 0)
OJ$Store7<-factor(OJ$Store7)
OJ$STORE <- factor(OJ$STORE)
OJ$SpecialCH <- factor(OJ$SpecialCH)
OJ$StoreID <- factor(OJ$StoreID)
# Randomly split the dataset
set.seed(123)
tr_size <- floor(2/3*nrow(OJ))
iTrain<-sample(seq_len(nrow(OJ)),tr_size, replace=FALSE)

# (a) Building the model considering all explanatory vars
mod.whole.mlog <- glm(OJ[iTrain,]$Purchase~.,data=OJ[iTrain,],family=binomial)
summary(mod.whole.mlog)
# Kick out attributes with no contribution to the model
# which are linearly dependent with other attributes. 
# Find a model that prevents singularity
OJl<- OJ[,-c(11,12,13,14,17,18)]
mod.lean.mlog <- glm(OJl[iTrain,]$Purchase~.,data=OJl[iTrain,],family=binomial)
summary(mod.lean.mlog)
# Predict, calculate MSE and plot on linear scale
pred.lean.mlog<-predict(mod.lean.mlog, OJ[-iTrain,],type="response")
summary(pred.lean.mlog)
pred.lean.mlog
mlog.lean.mser<-mse(OJ[-iTrain,]$Purchase,pred.lean.mlog)
mlog.lean.mser
plot(pred.lean.mlog,col=OJ[-iTrain,]$Purchase+2)
lines(y=rep(0.5,nobs),x,type = "l")
## The missclassification rate
# of the CH class value
nCH = sum(OJ[-iTrain,]$Purchase == 1)
sum(pred.lean.mlog>0.5)
mlog.lean.CHmc<-abs(nCH - sum(pred.lean.mlog>0.5))/nCH
mlog.lean.CHmc
# of the MM class value
nMM = sum(OJ[-iTrain,]$Purchase == 0)
mlog.lean.MMmc<-abs(nMM- sum(pred.lean.mlog<0.5))/nMM
mlog.lean.MMmc
# (b) Select significant attributes
mod.mlog<-step(mod.whole.mlog,direction="both")
summary(mod.mlog)
pred.mlog<-predict(mod.mlog, OJ[-iTrain,],type="response")
mlog.mser<-mse(OJ[-iTrain,]$Purchase,pred.mlog)
plot(pred.mlog,col=OJ[-iTrain,]$Purchase+2)
lines(y=rep(0.5,nobs),x,type = "l")
## The missclassification rate
# of the CH class value
mlog.CHmc<-abs(nCH- sum(pred.mlog>0.5))/nCH
mlog.CHmc
# of the MM class value
mlog.MMmc<-abs(nMM - sum(pred.mlog<0.5))/nMM
mlog.MMmc
# (c) if the mlog.mser and mlog.lean.mser are compared they are nearly the same 
# however the missclassification rates for the model given by the step-function
# is significantly better, percentage-wise we have:
abs(mlog.MMmc - mlog.lean.MMmc) * 100
abs(mlog.CHmc - mlog.lean.CHmc) * 100
anova(mod.whole.mlog,mod.mlog,test="Chisq")
# Assuming a reasonable significance level, alpha of 5%, we can see that for 
# the majority of the attributes which are included in the smaller model 
# indeed we have a p-value which is smaller than alpha, but not for all of them
# for instance PctDiscCH, PriceCH and SpecialMM could be considered possible
# candidates for further deletion. However, because the coeficient of 
# PctDiscCH under the confidence interval formed by Std.Error doesn`t  include
# zero, neither does PriceCH and SpecialMM we keep those attributes. Therefore, 
# the smaller model does provide a better fit for the model.

### 3 - Logistic to Bank dataset ###
d <- read.csv2("~/work/emcl/amcr/tutorial/assign4/bank.csv")
d = na.omit(d)
nobs<-dim(d)[1]
d[,17] <- ifelse(d$y=="yes",1,0)
iTrain<-sample(seq_len(nrow(d)),tr_size,replace=FALSE)
# Fixing the categorical variables:
# 2.job, 3.marital, 4.education, 5.default, 7.housing, 8.loan, 
# 9.contact, 10.day, 11.month, 16.poutcome
d[,2]<-factor(d$job)
d[,3]<-factor(d$marital)
ed<- array(rep(0),dim=c(nobs,1))
for (i in 1:nobs)
{
  if (d[i,4]=="primary"){
    ed[i] = 1
  }else if (d[i,4]=="secondary"){
    ed[i] = 2
  }else if (d[i,4] == "tertiary" ){
    ed[i] = 3
  }else ed[i] = 0
}
d[,4]<-ed
d[,5]<-ifelse(d$default=="yes",1,0)
d[,7]<-ifelse(d$housing=="yes",1,0)
d[,8]<-ifelse(d$loan=="yes",1,0)
d[,9]<-ifelse(d$contact=="unknown",0,1)
d[,11]<-as.numeric(d$month)
d[,16]<-factor(d$poutcome)
# Logistic regression on bank data
mod.whole.bank <- glm(d[iTrain,]$y~.,data=d[iTrain,],family=binomial)
mod.bank<-step(mod.whole.bank,direction="both")
summary(mod.bank)
# Predictions and Valuation (linear scale use for sake of comparison)
pred.bank<-predict(mod.bank, d[-iTrain,],type="response")
mse(d[-iTrain,]$y,pred.bank)
# Misclassifications
nyes = sum(d[-iTrain,]$y == 1)
bank.yes<-abs(nyes - sum(pred.bank>0.5))/nyes
nno = sum(d[-iTrain,]$y == 0)
bank.no<-abs(nno - sum(pred.bank<0.5))/nno
# Note that 57% of the yes classifications were wrong, in order to cope with
# this we can balance the training set by taking equal number of instances
# of each class
iyes<-grep(TRUE, d$y == 1)
ino<-grep(TRUE, d$y == 0)
length(ino)
neachclass.tr<-floor(length(iyes)*2/3)
neachclass.tt<-floor(length(iyes)*1/3)
yiTrain<-sample(iyes,neachclass.tr,replace=FALSE)
niTrain<-sample(ino,neachclass.tr,replace=FALSE)
iTrainU<-c(yiTrain,niTrain)
iTrain<-sample(iTrainU)
all<- append(iyes,sample(ino,neachclass.tt,replace=FALSE))
iTest<-all[!(all %in% iTrain)]
# Retraining with the new training set
mod.whole.bank2 <- glm(d[iTrain,]$y~.,data=d[iTrain,],family=binomial)
mod.bank2<-step(mod.whole.bank2,direction="both")
summary(mod.bank2)
# Predictions and Valuation (linear scale use for sake of comparison)
pred.bank2<-predict(mod.bank2, d[iTest,],type="response")
plot(pred.bank2,col=OJ[iTest,]$Purchase+2)
bank.mserr<-mse(d[iTest,]$y,pred.bank2)
sum(pred.bank2>0.5)
neachclass.tt
bank.yes<-abs(neachclass.tt - sum(pred.bank2>0.5))/neachclass.tt
bank.no<-abs(neachclass.tt - sum(pred.bank2<0.5))/neachclass.tt
# The missclassification error went from around 57% to 12% by 
# undersampling the traindata