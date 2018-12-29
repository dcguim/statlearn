# Loading data & installing required packages
install.packages("ISLR")
install.packages("leaps")
data(Hitters,package="ISLR")

# Clean dataframe, remove 59 rows which contain na values
clhitts = na.omit(Hitters)
dim(clhitts)

# Split data into training and test data
trhitts = clhitts[1:140,]
tthitts = clhitts[141:263,]
dim(trhitts)
dim(tthitts)

# Create Model
bxnames <- !(colnames(trhitts) %in% 'Salary')
x <- trhitts[,bxnames]
y <- trhitts[,19]

# Computing the intercepts and coefficients of the full model
lm0 <- lm(y ~ 1,data=trhitts)
lm1 <- lm(y~AtBat+Hits+HmRun+Runs+RBI+Walks+Years+CAtBat+CHits+
                 CHmRun+CRuns+CRBI+CWalks+League+Division+PutOuts+
                 Assists+Errors+NewLeague, data = trhitts)
summary(lm1)
# Comments in http://mathb.in/28590

# Model selection by stepping 
lm1backstep<-step(lm1,direction="backward")
lm1forwstep<-step(lm1,direction="forward")
lm1bothstep<-step(lm1,direction="both")
anova(lm1backstep,lm1forwstep,lm1bothstep)
# Comments in http://mathb.in/28591

#Best Subset Regression
lm1regsubset <-leaps::regsubsets(trhitts$Salary~., data=trhitts, nbest = 3, nvmax = 8)
summary(lm1regsubset)  
plot(lm1regsubset)
#From the 12th step on, the bic doesn`t improve, therefore the smaller model that 
#can be selected for this dataset contains 4 variables: PutOuts, CmHmRun, Hits and AtBat. 
sregsubset <- summary(lm1regsubset)
str(sregsubset)
