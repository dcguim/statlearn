# Load Dataset
install.packages(c('e1071'))
library(e1071)
d<-read.csv("~/work/emcl/amcr/tutorial/bank/bank.csv",sep=";")
summary(d)
# Split dataset
set.seed(123)
tr_size<-floor(2/3*nrow(d))
ts_size<-nrow(d)-tr_size
iTrain<-sample(seq_len(nrow(d)),tr_size,replace=FALSE)
### (a) default parameters
# SVM Model and Support Vectors
mod1<-svm(y~.,data=d[iTrain,], kernel='radial', cost = 1)
summary(mod1)
head(mod1$index) # suppport vectors
# Prediction
pred1<-predict(mod1,d[-iTrain,])
TAB1<-table(prediction=pred1,label=d[-iTrain,]$y)
mkrate1<-(TAB1[1,2]+TAB1[2,1])/length(d[-iTrain]$y)
mkrate1
### (b) varying lambda and cost
mods2<-tune.svm(y~.,data=d[iTrain,],kernel="radial", 
         cost = c(1,5,10,15),gamma=c(0.01,0.05,0.075,0.5,1,2))
summary(mods2)
plot(mods2)
mods2
# The smallest error found was 0.1031792
# As the cost increases as does the number of support vectors, since the 
# constraint on the summation of slack variables get lighter, and the other
# way around when the cost decreases. High gammas increase the bias and
# decrease the variance and the other way arount when decreases.\
mod3 <- tune.svm(y~.,data=d[iTrain,],kernel="radial", 
                 cost = 1,gamma=0.0232)
mod3
# Although the mkerror, with default parameters were smaller, it was not
# found with 10-fold CV, in which case it is higher 0.1118138
### (c) Prediction with new parameters
mod4<-svm(y~.,data=d[iTrain,], kernel='radial', cost = 15, gamma=0.01)
# Prediction
pred4<-predict(mod4,d[-iTrain,])
TAB4<-table(prediction=pred4,label=d[-iTrain,]$y)
TAB4
mkrate4<-(TAB4[1,2]+TAB4[2,1])/length(d[-iTrain]$y)
mkrate4
# Improvement from mkrate1
(mkrate1-mkrate4)/mkrate1
### (d) Diminish the mkrate of the yes class
costs<-table(d[iTrain,]$y)
costs[1]<- 0.5
costs[2]<- 1
costs
mods6<-tune.svm(y~.,data=d[iTrain,],kernel="radial",  class.weights=costs,
                costs = c(1,5,10,15),gamma=c(0.01,0.05,0.075,0.5,1,2))
summary(mods6)
mod7<-svm(y~.,data=d[iTrain,], kernel='radial', class.weights=costs,
          cost=5,gamma=0.05)
# Prediction
pred7<-predict(mod7,d[-iTrain,])
TAB7<-table(prediction=pred7,label=d[-iTrain,]$y)
TAB7
mkrate7<-(TAB7[1,2]+TAB7[2,1])/length(d[-iTrain]$y)
mkrate7
(mkrate7-mkrate4)/mkrate4
# With a increase in the mkrate of 4.5% from the mkrate obtained with equal
# weights to both class, was possible to diminish the number of missclassif.
# from the yes class 27%, which yielded a more balaced missclassified groups.
(TAB4[1,2] - TAB7[1,2])/sum(TAB4[,2])

