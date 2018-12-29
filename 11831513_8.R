# Install required packages and load dataset
install.packages(c("ISLR","mgcv"))
library(splines)
library(mgcv)
data(Auto, package="ISLR")
summary(Auto)
# Clean & Split dataset
set.seed(123)
nm <- Auto[,9]
d<-Auto[,-9]
tr_size<-floor(2/3*nrow(d))
iTrain<-sample(seq_len(nrow(d)),tr_size,replace=FALSE)
length(iTrain)
# Aux Functions
mse <- function(y, y.hat) {
  mean((y - y.hat)^2)
}
### 1 - Linear model with natural cubic splines
# Categorical column origin won`t enter the natural splines
ns.form = mpg~cylinders+ns(displacement,df=4)+ns(horsepower,df=4)+
          ns(weight,df=4)+ns(acceleration,df=4)+ns(year,df=4)+origin
mod.whole.ns<-lm(ns.form,data=d[iTrain,])
# To consider which variables are significant we can estabilish a threshold 
# alpha, then check which absolute coefs. are smaller than alpha; let
aph = 0.5
abs(mod.whole.ns$coefficients) < aph
pred.whole<-predict(mod.whole.ns, newdata=d[-iTrain,])
rmse.whole<-sqrt(mse(pred.whole,d[-iTrain,]$mpg))
rmse.whole
# cylinders, displacement, year, origin are perhaps insignificant (aph=0.5)
# we can double check with stepwise selection
mod.ns<-step(mod.ns,direction="both")
# only cylinders and displacement were quicked out by the step criterium
pred.sel<-predict(mod.ns, newdata=d[-iTrain,])
rmse.sel<-sqrt(mse(pred.sel,d[-iTrain,]$mpg))
rmse.sel
# There is a small improvement using the smaller model
# To plot the fitted variables against the variables, we need the respective
# intervals in the model with variable selection. The fitted variables is 
# obtained by matrix multiplying the basis function by the respective coefs
# obtained from the reduced linear model
plot(d$horsepower,ns(d$horsepower,df=4)%*%mod.ns$coefficients[2:5],
     xlab="var",ylab="fitted var",sub="horsepower")
# A possible interpretation is, as the value of horsepower increase
# the contribution of the fitted var to the predicted mpg variable decrease
# Therefore we can see the relationship between the variation of the
# variable and it`s fitted contribution to the prediction of mpg.
# Simmilarly:
plot(d$weight,ns(d$weight,df=4)%*%mod.ns$coefficients[6:9],
     xlab="var",ylab="fitted var",sub="weight")
plot(d$acceleration,ns(d$acceleration,df=4)%*%mod.ns$coefficients[10:13],
     xlab="var",ylab="fitted var",sub="acceleration")
plot(d$year,ns(d$year,df=4)%*%mod.ns$coefficients[14:17],
     xlab="var",ylab="fitted var",sub="year")

### Generalized Additive Models
form1 = mpg ~ cylinders + s(displacement,fx=TRUE) + s(horsepower,fx=TRUE) + s(weight,fx=TRUE) + 
      s(acceleration,fx=TRUE) + s(year,fx=TRUE) + origin
form2 = mpg ~ cylinders + s(horsepower) + s(weight) + s(year) + origin
mod.whole.gam <- gam(form2,data=d[iTrain,])
# Estimated degrees of freedom of the smooth terms
summary(mod.whole.gam)
# As seen in the p-value of hypothesis test in the summary, displacement and
# perhaps even acceleration could be removed, since they are less 
# significant to the model. Horsepower and displacement has roughly a linear
# complexity, weight has squared and acceleration and year have the biggest
# estimated degrees of freedom, approx. 6.7 and 8.7 respectively.
# The following plot exhibit the contribution of each value to mpg.
plot(mod.whole.gam,page=1,shade=TRUE,shade.col="yellow")
# The year of the car, for instance, increases together with the predicted
# mpg, showing that newer cars have higher mpg, however the contribution
# over the years is not linear; we could interpret this as advancements in
# other areas that allow the production of more efficient motors.
# Calculating RMSE 
pred.gam<-predict(mod.whole.gam, newdata=d[-iTrain,])
rmse.gam<-sqrt(mse(pred.gam,d[-iTrain,]$mpg))
rmse.gam
?step.gam
# Shrinkage smoothers (whole model):
# The default basis used is thin plate reg. spline, which yielded 3.037091
# substituting the basis to cubic reg. spline the rmse worsen to 3.06963.
# Null space penalization (whole model):
# With the extra penalty for each smooth term also worsen the RMSE to 3.273759

# Best result obtained was found by removing displacement and acceleration
# vars and with default shrinkage method thin plate reg. of RMSE 3.006217.
# For this reduced model, also cubic reg basis and null space penalization
# was attempted but none had a smaller RMSE.
