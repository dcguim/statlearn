# Loading data & installing required packages
install.packages("ISLR")
install.packages("pls")
library("pls")
library("leaps")
data(Hitters,package="ISLR")

# Cleaning and Splitting sample
Hitters = na.omit(Hitters)
Hitters[,c(14,15,20)]<-cbind(as.numeric(Hitters[,14]), as.numeric(Hitters[,15]), as.numeric(Hitters[,20]))
#Hitters[,]<-Hitters[,-c(14,15,20)]

set.seed(123)
smp_size <- floor(0.5*nrow(Hitters))
iTrain<-sample(seq_len(nrow(Hitters)),smp_size, replace=FALSE)
train<-Hitters[iTrain,]
test<-Hitters[-iTrain,]
dim(test)
dim(train)

model.pcr<-pcr(train$Salary~.,data=train, ncomp=19, validation="CV", segments=10, scale=TRUE,segment.type="random")
summary(model.pcr)
plot(model.pcr, plottype = "validation", val.type = "RMSEP", legend = "topright")
# As the number of components increase from 0 to 6, and correspondingly the number 
# of vectors that are used to represent the data, there is a non-monotonic decrease 
# in the RMSEP meaning that the variance on that axis (vectors form a basis) can be 
# modelled by the additional vector/component. The optimal number of components is 6,
# note that with 6 components a good amount of the variance of X is approx 90%. 
y <- train$Salary
ypred <- model.pcr$fitted.values[,,6]
x <-seq(100,2300,1)
plot(y,type = "o",col = "red")
lines(ypred, type = "o", col = "blue")
legend(-5, 2500, legend=c("pred", "meas"),
       col=c("red", "blue"), lty=1:2, cex=0.8)

# 2. Scalling and PLS
x<-scale(train[,-20],T,T)
y<-scale(train[,20],T,T)
dim(train)
model.plsr<-plsr(y~x, ncomp=19, validation="CV", segments=10,scale=TRUE, segment.type="random")
summary(model.plsr)
plot(RMSEP(model.plsr), legendpos="topright")
# RMSEP monotonically decreases from 0 to 3 components which then decrease
# insignificantly adding 4th componenet then increases. However, the variance
# explained with 3 comps corresponds to only approx. 60% of the variance in 
# the dataset, this is due to the small number of components hence the "partial" of PLS.
# The reduced number of components and the better prediction perfomance is due to
# the fact that PLS uses the prediction vector y to maximize the correlation 
# with PLS-directions of the modified predictors.

# PCR with variable selection
model.pcomp<-princomp(x)
summary(model.pcomp)
biplot(model.pcomp)
# the plot represent the ellipsoid formed by the principal components, the
# dispersion of the points decrease as the number of the component decrease
# and respectivelly the variance. So in this plot is shows that 1 comp 
# have a higher variance than 2 comp.
scores<-data.frame(model.pcomp$scores)
model.regsubsets <- regsubsets(train$Salary ~ ., data =scores)
plot(model.regsubsets)
model.best<-lm(train$Salary~Comp.1+Comp.3+Comp.4+Comp.5+Comp.6+Comp.7+Comp.9+Comp.10, data =scores)
predbest<-predict(model.best,newdata=test)
