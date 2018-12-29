# Install Required Packages
install.packages("ISLR")
install.packages("MASS")
install.packages("ipred")
install.packages("klaR")
data(OJ,package="ISLR")
library(ipred)
# oldOJ refers to the data as is from the package while the OJ will be 
# adapted as necessary
OJ = na.omit(OJ)
OJold<-OJ
# Clean and prepare dataset for classification
OJ[,14]<-as.numeric(OJ[,14])
OJold[,14]<-as.numeric(OJ[,14])
nobs<-dim(OJ)[1]
nvars<-dim(OJ)[2]
OJy<- array(rep(0),dim=c(nobs,2))
for (i in 1:nobs)
{
  if (OJ[i,1]=="CH")
    OJy[i,1] = 1
  else OJy[i,2] = 1
}
# Randomly splitting the training set
OJ[,1]<-OJy
set.seed(123)
smp_size <- floor(0.66*nrow(OJ))
iTrain<-sample(seq_len(nrow(OJ)),smp_size,replace=FALSE)
train<-OJ[iTrain,]
test<-OJ[-iTrain,]

# Linear Regression with indicator random variable
mod.lm <-lm(train$Purchase~., data=train)
lm.pred1<-predict(mod.lm,newdata=test)[,1]
lm.pred2<-predict(mod.lm,newdata=test)[,2]
lm.predind<- as.numeric(lm.pred1<lm.pred2)
dim(test$Purchase)
length(lm.predind)
TAB<-table(OJold[-iTrain,]$Purchase, lm.predind)
lm.missrate<-1-sum(diag(TAB))/sum(TAB)

# Linear Discrimant Analysis (LDA)

# Estimating parameteres (not necessary)
#trnobs<-dim(train)[1]
#nCH<-length(grep(TRUE, OJold[iTrain,]$Purchase == "CH"))
#nMM<-length(grep(TRUE, OJold[iTrain,]$Purchase == "MM"))
#mu1<-TAB[1,]/nCH
#mu2<-TAB[2,]/nMM
#sigv<-c((TAB[1,1]-mu1)/(trnobs-2),(TAB[1,2]-mu1)/(trnobs-2),
#        (TAB[2,1]-mu2)/(trnobs-2),(TAB[2,2]-mu2)/(trnobs-2))
#sig<-matrix(sigv,ncol=2)

# Aux functions
mypred = function(object, newdata) UseMethod("mypred", object)
mypred.lda<-function(object, newdata){
    predict(object, newdata = newdata)$class
}
# Computing the model
corOJ<-cor(OJold[,-1])
diss<- 1- abs(corOJ)
dist<- as.dist(diss)
hc <- hclust(dist);  
clusterV = cutree(hc,h=0.1);
# Colinerity is a problem when using LDA, in order to solve this, 
# we calculated the correlation matrix betweent those variables
# then clustered it`s distance. we can see that Store7, PctDiscMM and 
# PctDiscCH belong to clusters of another variables therefore can be removed.
OJmin<-OJold[,-c(14,15,16)]
train<-OJmin[iTrain,]
mod.lda<-MASS::lda(train$Purchase~.,data=train)
TAB<-table(OJmin[-iTrain,]$Purchase,mypred(mod.lda,OJmin[-iTrain,]))
lda.missrate<-1-sum(diag(TAB))/sum(TAB)

# QDA
train<-OJold[iTrain,]
nCH<-length(grep(TRUE, train$Purchase == "CH"))
nMM<-length(grep(TRUE, train$Purchase == "MM"))
mod.qda<-MASS::qda(train$Purchase~.,data=train)
# group deficiency in CH? 428/(428+278) ???

# RDA
mod.rda<-klaR::rda(train$Purchase~.,data=train)
rda.pred<-predict(mod.rda, OJold[-iTrain,])
TAB<-table(OJold$Purchase[-iTrain], rda.pred=rda.pred$class)
rda.missrate<-1-sum(diag(TAB))/sum(TAB)

# Bank marketing classification
d <- read.csv2("~/work/emcl/amcr/tutorial/assign4/bank.csv")
d = na.omit(d)
# (a) Select randomly a training set with 3000 observations, and use LDA to predict the
# group label of the remaining test set observations. Look at the confusion table and 
# the resulting misclassification rate.
smp_size <- 3000
iTrain<-sample(seq_len(nrow(d)),smp_size,replace=FALSE)
train<-d[iTrain,]
test<-d[-iTrain]
colnames(d)
nobs<-dim(d)[1]
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

d[,c(2,3,5,7,8,9,11,16)]<-cbind(as.numeric(d[,2]),as.numeric(d[,3]),as.numeric(d[,5]),
                                as.numeric(d[,7]),as.numeric(d[,8]),as.numeric(d[,9]),
                                as.numeric(d[,11]),as.numeric(d[,16]))
# Computing the model
cord<-cor(d[,-17])
diss<- 1- abs(cord)
dist<- as.dist(diss)
hc <- hclust(dist);  
clusterV = cutree(hc,h=0.1);
# Colinerity doesn`t seem to be a problem
mod.lda<-MASS::lda(d[iTrain,]$y~.,data=d[iTrain,])
TAB1<-table(d[-iTrain,]$y,mypred(mod.lda,d[-iTrain,]))
lda.missrate1<-1-sum(diag(TAB1))/sum(TAB1)

#(b) Using ???balanced??? samples from both groups
iyes<-grep(TRUE, d$y == "yes")
ino<-grep(TRUE, d$y == "no")
neachclass<-floor(length(iyes)*2/3)
yiTrain<-sample(iyes,neachclass,replace=FALSE)
niTrain<-sample(ino,neachclass,replace=FALSE)
iTrainU<-c(yiTrain,niTrain)
iTrain<-sample(iTrainU)

mod.lda<-MASS::lda(d[iTrain,]$y~.,data=d[iTrain,])
TAB2<-table(d[-iTrain,]$y,mypred(mod.lda,d[-iTrain,]))
lda.missrate2<-1-sum(diag(TAB2))/sum(TAB2)
# It can be easily observed that number of yes proportionally increased
# in comparison to TAB1, due to undersampling