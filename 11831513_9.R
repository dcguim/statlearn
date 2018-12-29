# Install required packages and load dataset
install.packages(c("rpart", "rpart.plot"))
library(rpart)
library(rpart.plot)
d<-read.csv("~/work/emcl/amcr/tutorial/assign9/bank.csv",sep=";")
summary(d)
names(bank)
# Aux Functions
ret<-function(res,iTest)
{
  t<-list()
  for (j in 1:length(iTest))
  {
    yc <- 0
    nc <- 0
    for (i in 101:200)
    {
      if (res[i][j] == "yes")
        yc <- yc+1
      else
        nc <- nc+1
    }
    t[[j]]<-c(iTest,yc,nc)
  }
  t
}

forest<-function(rng,nobs,ino,iyes,iTrain)
{
  mods<-list()
  preds<-list()
  for (i in 1:rng)
  {
    yit<-sample(iyes,nobs,replace=TRUE)
    nit<-sample(ino,nobs,replace=TRUE)
    itU<-c(yit,nit)
    it<-sample(itU)
    mods[[i]] <- rpart(y~., data=d, subset=it, method="class",
                          cp=0.001, xval=20)
    preds[[i]] <- predict(mods[[i]], d[-iTrain,],type="class")
  }
  c(mods,preds)
}
# Clean & Split dataset
set.seed(123)
tr_size<-floor(2/3*nrow(d))
ts_size<-nrow(d)-tr_size
iTrain<-sample(seq_len(nrow(d)),tr_size,replace=FALSE)
iTest<-seq_len(nrow(d))[!seq_len(nrow(d)) %in% iTrain]
### 1 - Classification Trees
tree.whole <- rpart(y~., data=d, subset=iTrain, method="class",
                    cp=0.001, xval=20) 
rpart.plot(tree.whole)
# Prediction
pred.whole <- predict(tree.whole, d[-iTrain,],type="class")
tab.whole <- table(d[-iTrain, "y"], pred.whole)
tab.whole
# Misclassification Rate
whole.mkerr<-1-sum(diag(tab.whole))/sum(tab.whole)
whole.mkerr
# Cross Validation
printcp(tree.whole)
plotcp(tree.whole,upper="size")
# It can be seen that as the complexity parameter approaches zero, the size
# of the tree, and the number of splits grows, which means that it cares more
# about the fit then the size. In the other hand as it increases the size
# reduces as the xerror increases. The plot helps to find the most
# parsimoneous (smallest) tree with the smallest error. In this case, by
# using the 1SE rule, we have the smallest xerror with 15 splits and with
# only three splits for CP 0.0231884 the xerror is already under the line.
tree.pr<-prune(tree.whole, cp=0.0231884)
# Corresponding tree with CP 0.0231884
rpart.plot(tree.pr)
# Prediction
pred.pr <- predict(tree.pr, d[-iTrain,],type="class")
tab.pr <- table(d[-iTrain, "y"], pred.pr)
tab.pr
# Misclassification Rate
pr.mkerr<-1-sum(diag(tab.pr))/sum(tab.pr)
pr.mkerr
# The improvement
whole.mkerr - pr.mkerr
# Under sampling the 'no' class
iyes<-grep(TRUE, d[iTrain,]$y == "yes")
ino<-grep(TRUE, d[iTrain,]$y == "no")
mods<-list()
res<-forest(1,200,ino,iyes,iTrain)
rpart.plot(res[[1]])
# It does change the tree, it`s complexity is shadowed by undersampling
# Doing it a 100 times ...
res<-forest(100,200,ino,iyes,iTrain)
# Gather all the numbers of yes and nos by test observation, calculate the
# majority vote
t<-ret(res,iTest)
# didnt have time to check whether it improve, merry christmas
