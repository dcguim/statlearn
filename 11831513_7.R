# Install required packages and load dataset
install.packages(c("ElemStatLearn", "robustbase"))
library(robustbase)
library(ElemStatLearn)
library(splines)
data(bone, package="ElemStatLearn")
summary(bone)
# Clean & Split dataset
set.seed(123)
d <- bone[bone$gender=="male",c("age","spnbmd")]
tr_size<-floor(2/3*nrow(d))
iTrain<-sample(seq_len(nrow(d)),tr_size,replace=FALSE)
# Aux Functions
mse <- function(y, y.hat) {
  mean((y - y.hat)^2)
}
sstrain <- function(train,i)
{
    mod <- smooth.spline(x=train$age,y=train$spnbmd,df=i)
}
mod.ss2<-sstrain(d[iTrain,],2)
sstest <- function(mod,test)
{
  age.ord <- order(test$age)
  pred<-predict(mod,test$age[age.ord])
  plot(test,ylim=c(-0.05,0.2))
  lines(x=test$age[age.ord],y=pred$y,col="blue")
  list(y=pred$y,ord=age.ord)
}
trydfss<-function(train,test,rng)
{
  mserr<-list()
  for (i in rng)
  {
    print(i-1)
    mod<-sstrain(train,i)
    pred<-sstest(mod,test)
    mserr[[i-1]]<-mse(test$spnbmd[pred$ord],pred$y)
    print(mserr[[i-1]])
  }
  mserr
}
trydf<- function (stars,i,color, type)
{
  if (type == "ns"){
    mod <- ns(stars$temp,df=i)}
  else if (type == "poly"){
    mod<-poly(stars$temp,degree=i)}
  else{on.exit(cat("Unknown basis expansion type!\n"))}
  dataf <- data.frame(temp=mod,light=stars$light)
  linm <- lm(light~.,data=dataf)
  points(stars$temp,linm$fitted,col=color)
}

### 1 - Smooth Splines ###
# (a) smooth splines without explicit selection of df
# Create smooth splines for the train data
sum(is.na(d[iTrain,]) == TRUE) # no NA values
mod.ss.ws <- smooth.spline(x=d[iTrain,]$age,
                      y=d[iTrain,]$spnbmd,
                      cv=TRUE)
lines(ss.ws,col="red")
# Store the order of the test set, 
# necessary to draw the line in a coherent way
age.ord <- order(d[-iTrain,1])
# Predict using the predefined order and plot 
pred.ss.ws<-predict(mod.ss.ws,d[-iTrain,1][age.ord])
plot(d[-iTrain,],ylim=c(-0.05,0.2))
lines(x=d[-iTrain,]$age[age.ord],y=pred.ss.ws$y,col="blue")
mse(d[-iTrain,2][age.ord],pred.ss.ws$y)
# Obtained mse 0.001140674
# (b) smooth splines with explicit selection of df
plot.new()
mserr<-trydfss(d[iTrain,],d[-iTrain,],2:20)
plot(seq(1,19,1),mserr, ylim= c(0.001,0.0018))
lines(seq(1,19,1),mserr, col="red")
# the smallest mse is obtained is 0.001136498 for df = 3+1 = 4, index
# i of the vector of mserr represent i+1 df.

### 2 - Stars data ###
stars<-read.csv("~/path/to/starsdata.csv",sep=",")
summary(stars)
# light is used as response variable; temperature as explanatory variable.
plot(stars)  
# (a) Smoothing splines without explicit selection of df
mod.ss.whole <- smooth.spline(x=stars$temp,y=stars$light,cv=TRUE)
points(mod.stars.whole,col="red")
# (b) Fit a linear model on a “reasonable” number of natural cubic splines 
# (ns() from package splines), and visualize the fit in the plot.
plot.new()
plot(stars,xlim= c(100,3200))
trydf(stars,1,"green","ns")
# It can be seen in the plot that for df = 1 still we have a linear behaviour
# therefore we need to increase the df to provide higher polynomials to
# capture the variance on the clusters
trydf(stars,2,"blue","ns")
trydf(stars,3,"purple","ns")
trydf(stars,4,"red","ns")
# We can see in the plat that for df = 4 we already have an overfit therefore
# the appropriate valur of df would be 3, as depicted in the purple points.
# (c) Use the polynomial regression instead of natural splines.
plot.new()
plot(stars,xlim= c(100,3200))
trydf(stars,3,"purple","poly")
# (d) Use a classical linear model
cl.lm <- lm(light~.,data=stars)
points(stars$temp,cl.lm$fitted,col="green")
# The result is equal to the natural spline with one degree of freedom
# a poor linear result is obtained for the dataset
# (e) Use a robust (against data outliers) linear model
cl.lmrob <- lmrob(light~.,data=stars)
points(stars$temp,cl.lmrob$fitted,col="blue")
# Even with the robust linear model, the result obtained is
# not sufficient to capture the non-linearity of the dataset
# however it is an improvement from normal linar model on the
# sense that the inclination of the line account for the 
# variance, however it doesnt perform as well as the natural
# splines for instance because it ignores the other cluster
# centered around temp=2900