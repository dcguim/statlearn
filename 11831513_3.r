# Loading data & installing required packages
install.packages("ISLR")
install.packages("MASS")
install.packages("glmnet")
library(glmnet)
library(MASS)
data(Hitters,package="ISLR")
# Auxiliary function
mse <- function(y, y.hat) {
  mean((y - y.hat)^2)
}
# Cleaning and Splitting sample
Hitters = na.omit(Hitters)
Hitters[,c(14,15,20)]<-cbind(as.numeric(Hitters[,14]), 
                            as.numeric(Hitters[,15]), 
                            as.numeric(Hitters[,20]))
set.seed(123)
smp_size <- floor(0.5*nrow(Hitters))
iTrain<-sample(seq_len(nrow(Hitters)),smp_size, replace=FALSE)
train<-Hitters[iTrain,]
test<-Hitters[-iTrain,]
# Ridge with lambda selection
model.ridge.wlbd<-lm.ridge(train$Salary~.,data=train,lambda=seq(0,30,by=0.2))
select(model.ridge.wlbd)
plot(model.ridge.wlbd$lambda,model.ridge.wlbd$GCV,type='l')
# Generalized CV or Leave-one-out CV is when the size of each segment in the 
# CV is 1; then for each lambda the avg. sum of squared error is calculated: 
# yi to f-i(xi), where f-i is the fit f for xi using all the observations 
# except the ith. Intuitively, each term of the sum is the squared error 
# obtained from the prediction of the ith observation.
# The optimal lambda is 8 which gives the smallest error 1014.852.
model.ridge<-lm.ridge(train$Salary~.,data=train,lambda=8)
model.ridge$GCV
model.ridge$coef
# The ??0 is estimated by the mean of y
intercept = mean(train$Salary)
ridge.coef<-append(intercept,model.ridge$coef)
ridge.predSalary<-sqrt(as.matrix(cbind(const=1,test[,-19])) %*% ridge.coef)

# Plot of predicted x actual values
y <- test$Salary
x <-seq(1,132,1)
plot(y,type = "o",col = "red",xlab="Salary Values")
lines(ridge.predSalary, type = "o", col = "blue")
legend(-10, 2600, legend=c("meas", "pred"),
       col=c("red", "blue"), lty=2:2, cex=0.5)

# Lasso 
y<-train$Salary
x<-as.matrix(train[,-19])
model.lasso<-glmnet(x,y)
plot(model.lasso)
# The plot explains how the coefficients varies with
# respect with the choice of s, so for higher values of
# s the regression is not rigorous and the coefficients will
# get higher, so less shinkrage and it will tend to the least squares
# regression. It can also be seen that for some values of s
# small enough we have 0 coefficients, therefore the lasso
# is also performing continuous model selection as it varies
# the values of s. Because the nobs > nvars the minratio is 10-4
# however the ratio can be bigger, there are 90 values for lambda
# ranging from 0.059 to 234.869.
# Alpha (aph) is the mixing parameter, it is a value 
# 0<= aph<= 1, which combines the ridge and lasso penality,
# if aph = 1, we have the lasso and aph = 0 the ridge.
cv.lasso<-cv.glmnet(x, y)
plot(cv.lasso)
# Given the plot, to find the ideal log(lambda*), we add to
# the log(min. lambda) approx 3.2, a std error and select the most 
# variable parsimoneous lambda, in this case, log(lambda*) approx 4.85, 
# with MSE approx. 175,000. The MSE was generated by calculating
# by default the CV with 10 components.
lasso.coef.min<-coef(cv.lasso,s="lambda.min")
lasso.coef.ideal<-coef(cv.lasso,s=exp(4.85))
lasso.predSalary.min<-as.matrix(cbind(const=1,test[,-19])) %*% lasso.coef.min
lasso.predSalary.ideal<-as.matrix(cbind(const=1,test[,-19])) %*% lasso.coef.ideal
# Note that there is a tradeoff of choosing the ideal/parsimoneous lambda 
# in one side we get less variables an a smaller model, however the MSE is bigger
# Plot
y<-test$Salary
plot(y,type = "o",col = "red",xlab="Salary Values")
lines(lasso.predSalary.ideal, type = "o", col = "blue")
lines(lasso.predSalary.min, type = "o", col = "green")
legend(-10, 2600, legend=c("meas", "pred.ideal","pred.min"),
       col=c("red", "blue","green"), lty=2:2, cex=0.5)
# Comparison
mse(y,lasso.predSalary.ideal)
mse(y,lasso.predSalary.min)
mse(y,ridge.predSalary)
# The cost of the parsimoneous model is that it has the MSE bigger
# than the Ridge, and it can be improved by paying the price of a
# larger model.