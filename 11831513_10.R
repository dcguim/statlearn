install.packages(c('randomForest'))
library(randomForest)
d<-read.csv("~/work/emcl/amcr/tutorial/bank/bank.csv",sep=";")
summary(d)
# Clean & Split dataset
set.seed(123)
dim(d)
na.omit(d)
tr_size<-floor(2/3*nrow(d))
ts_size<-nrow(d)-tr_size
iTrain<-sample(seq_len(nrow(d)),tr_size,replace=FALSE)
iTest<-seq_len(nrow(d))[!seq_len(nrow(d)) %in% iTrain]
# 1 - Straight Random Forest
?randomForest
mod.rf<-randomForest(y ~ ., data=d, subset=iTrain,importance=TRUE)
mod.rf
plot(mod.rf)
# the black line on the bootom shows how the error decrease as the
# number of trees increase from 0 to 30 then slowly stabilize around 0.1
varImpPlot(mod.rf)
# this plot shows the importance of each variable, the left one shows the
# decrease of prediction accuracy once the variable is removed from the model,
# the most relevant variables will have the highest decreases.
# 2 - Improve Misclassification
# undersampling n of nos
iyes<-grep(TRUE, d$y == "yes")
ino<-grep(TRUE, d$y == "no")
ino.s<-sample(ino, size=length(iyes), replace = FALSE) 
length(c(iyes,ino.s))
Usamp<-c(iyes,ino.s)
samp<-sample(UiTrain.s)
tr_size.s<-floor(2/3*length(samp))
ts_size.s<-nrow(samp)-tr_size
iTrain.s<-sample(samp,tr_size.s,replace=FALSE)
mod1.rf<-randomForest(y ~ ., data=d, subset=iTrain.s,importance=TRUE)
mod1.rf
# modify sampsize
mod2.rf<-randomForest(y ~ ., data=d, subset=iTrain.s,importance=TRUE, sampsize=100)
sum(mod2.rf$confusion[,3])
mod3.rf<-randomForest(y ~ ., data=d, subset=iTrain.s,importance=TRUE, sampsize=150)
sum(mod3.rf$confusion[,3])
mod4.rf<-randomForest(y ~ ., data=d, subset=iTrain.s,importance=TRUE, sampsize=200)
sum(mod4.rf$confusion[,3])
mod5.rf<-randomForest(y ~ ., data=d, subset=iTrain.s,importance=TRUE, sampsize=250)
sum(mod5.rf$confusion[,3])
mod6.rf<-randomForest(y ~ ., data=d, subset=iTrain.s,importance=TRUE, sampsize=300)
sum(mod6.rf$confusion[,3])
mod7.rf<-randomForest(y ~ ., data=d, subset=iTrain.s,importance=TRUE, sampsize=350)
sum(mod7.rf$confusion[,3])
# aparently it is a convex function which has lowest missclassification rate
# around 250 samples
# modify the parameter classwt
mod8.rf<-randomForest(y ~ ., data=d, subset=iTrain.s,importance=TRUE,
                      classwt=c(20e2,1e-2))
mod8.rf
sum(mod8.rf$confusion[,3])
# Class weighting help improve the performance of variable with a lower 
# performance by prioritizing that class in the learning process, e.g.
# the weight of yes it too low and of no is too big than the predictions
# will all be nos, but when fine tuned, it can provide good balance between
# the categories
# modify cutoff
mod9.rf<-randomForest(y ~ ., data=d, subset=iTrain.s,importance=TRUE,
                      cutoff=c(0.35,0.5))
mod9.rf
sum(mod9.rf$confusion[,3])
# the cutoff can be used to prioritize the variable by with highest 
# classification error by diminishing the cutoff of the winning class for this
# variable.
mod10.rf<-randomForest(y ~ ., data=d, subset=iTrain.s,importance=TRUE,
                      strata=y)
mod10.rf
sum(mod10.rf$confusion[,3])
mod11.rf<-randomForest(y ~ ., data=d, subset=iTrain.s,importance=TRUE,
                       strata=d$y,sampsize=c(200,300))
mod11.rf
sum(mod10.rf$confusion[,3])
# Clearly the sampsize and the strata methods can be combined by specifying
# the amount of samples to be extracted from each strata. The best approach,
# was tuning class weight.