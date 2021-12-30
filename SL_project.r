#################################################################
# Loading data into R
#####################################

# Imports
library(tidyverse)
library(naniar)
library(corrplot)
library(ellipse)
library(caret)
library(pROC)
library(multiclassPairs)
library(glmnet)
library(ramify)
library(leaps)
library(MASS)
library(boot)

#################################################################

# Import data
data <- read.csv("~/SL_project/full-1.data", header = FALSE)
attach(data)
detach(data)

# Extract 14 columns
data <- data %>% dplyr::select(3, 4, 9, 10, 12, 16, 19, 32, 38, 40, 41, 44, 51, 58)


# Set column names
data <- data %>% 
  rename(
    'age' = V3,
    'sex' = V4,
    'cp' = V9,
    'trestbps' = V10,
    'chol' = V12,
    'fbs' = V16,
    'restecg' = V19,
    'thalach' = V32,
    'exang' = V38, 
    'oldpeak' = V40, 
    'slope' = V41, 
    'ca'= V44,
    'thal' = V51, 
    'num'= V58
   )

#####################################
# Clean data

# Replace -9 with NA
data <- data %>% replace_with_na_all(condition = ~.x == -9)

# Statistics of NA values
count_NA <- sapply(data, function(x) sum(is.na(x)))
print(count_NA)
percentrage_NA <- sapply(data, function(x) round( sum(is.na(x))/900,3))
print(percentrage_NA)

# drop columns with a lot of missing values
data <- subset(data, select = -c(slope, ca, thal))
data <- na.omit(data)

# Summary statistics
summary(data)

##########################################################

# Normalization
class <- data$num
data <- scale(data[, 0:10])
rownames(data) <- NULL

data<- as.data.frame(data)
data$class <- class

# Summary statistics
summary(data)

#############################################################
# Bar plots

# Sex
sex_counts <- table(data$sex)
barplot(sex_counts, main="Sex", names.arg = c("Female", "Male"),col = "yellow")

# CP
cp_counts <- table(data$cp)
barplot(cp_counts, main = "Chests Pain Type", names.arg = c("Typical angina", "Atypical angina", "Non-anginal pain", "Asymptomatic"), col = "yellow")

# Exang
exang_counts <- table(data$exang)
barplot(exang_counts, main = "Exercices Induced Angina Presence", names.arg = c("No", "Yes"), col = "yellow")

# Age (before normalization)
age_counts <- table(data$age)
barplot(age_counts, main = 'Age', col = 'yellow')

# Class
class_counts <- table(data$class)
barplot(class_counts, main = "Class", col = 'yellow')

# Feature plot
featurePlot(x = data[,1:10], y = data$class, 'pairs')
##########################################################
# Covariance and Correlation

# From class script number 1
# panel.hist function
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

# panel.cor function
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Covariance
covariance <- cov(data)
pairs(covariance, diag.panel=panel.hist, upper.panel=panel.cor)
pairs(covariance, diag.panel=panel.hist, upper.panel=panel.cor, lower.panel=panel.smooth)

# Correlation
correlation <- cor(data)

#plot1
plotcorr(cor(correlation))

#plot2
corrplot(correlation)

# Plot 3 (barplot)
correlation <- as.data.frame(correlation)
barplot(correlation$class, names.arg=colnames(data[1:11]),col = 'yellow' )


# Covariance and Correlation models
round(var(data[,-1]), 2)
round(cor(data[,-1]), 2)


#########################################################
# Multiple Regression Linear Model

#######################################################
# Class 1 regressor

class1_data <- data %>% filter(class == 0 | class == 1)

# Balance dataset

class1_data = class1_data[order(class1_data$class),]
table(class1_data$class)
class1_data = class1_data[215:484,]
row.names(class1_data) <- NULL


index <- createDataPartition(class1_data$class, p = .70, list = FALSE)
train <- class1_data[index,]
test <- class1_data[-index,]
test<-test[order(test$class),]

mod.class1 <- glm(class~sex+cp+trestbps+exang, data=train, family=binomial)
summary(mod.class1)

pred.class1 <- predict(mod.class1, test, type="response")

pred.class1[pred.class1 >.5] <- 1
pred.class1[pred.class1 <.5] <- 0
table(pred.class1,test$class)
table(pred.class1)
table(test$class)

roc.out <- roc(test$class, pred.class1, levels=c(0,1))
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate", main="ROC curve for class 1")

##################################################################
# Class 2

class2_data <- data %>% filter(class == 0 | class == 2)
class2_data$class[class2_data$class == 2] <- 1

# Balance dataset

class2_data = class2_data[order(class2_data$class),]
table(class2_data$class)
class2_data = class2_data[253:446,]
row.names(class2_data) <- NULL


index <- createDataPartition(class2_data$class, p = .70, list = FALSE)
train <- class2_data[index,]
test <- class2_data[-index,]
test<-test[order(test$class),]

mod.class2 <- glm(class~age+cp+thalach+exang+oldpeak, data=train, family=binomial)
summary(mod.class2)

pred.class2 <- predict(mod.class2, test, type="response")

pred.class2[pred.class2 >.5] <- 1
pred.class2[pred.class2 <.5] <- 0
table(pred.class2,test$class)
table(pred.class2)
table(test$class)

roc.out <- roc(test$class, pred.class2, levels=c(0,1))
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate", main="ROC curve for class 2")

###########################################################
# Class 3


class3_data <- data %>% filter(class == 0 | class == 3)
class3_data$class[class3_data$class == 3] <- 1

# Balance dataset

class3_data = class3_data[order(class3_data$class),]
table(class3_data$class)
class3_data = class3_data[250:449,]
row.names(class3_data) <- NULL


index <- createDataPartition(class3_data$class, p = .70, list = FALSE)
train <- class3_data[index,]
test <- class3_data[-index,]
test<-test[order(test$class),]

mod.class3 <- glm(class~age+sex+thalach+exang+oldpeak, data=train, family=binomial)
summary(mod.class3)

pred.class3 <- predict(mod.class3, test, type="response")
pred.class3[pred.class3 >.5] <- 1
pred.class3[pred.class3 <.5] <- 0
table(pred.class3,test$class)
table(pred.class3)
table(test$class)

roc.out <- roc(test$class, pred.class3, levels=c(0,1))
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate", main="ROC curve for class 3")

####################################################################
# Class 4


class4_data <- data %>% filter(class == 0 | class == 4)
class4_data$class[class4_data$class == 4] <- 1

# Balance dataset

class4_data = class4_data[order(class4_data$class),]
table(class4_data$class)
class4_data = class4_data[314:385,]
row.names(class4_data) <- NULL


index <- createDataPartition(class4_data$class, p = .70, list = FALSE)
train <- class4_data[index,]
test <- class4_data[-index,]
test<-test[order(test$class),]

mod.class4 <- glm(class~oldpeak, data=train, family=binomial)
summary(mod.class4)

pred.class4 <- predict(mod.class4, test, type="response")
pred.class4[pred.class4 >.5] <- 1
pred.class4[pred.class4 <.5] <- 0
table(pred.class4,test$class)
table(pred.class4)
table(test$class)

roc.out <- roc(test$class, pred.class4, levels=c(0,1))
plot(roc.out, print.auc=TRUE, legacy.axes=TRUE, xlab="False positive rate", ylab="True positive rate", main="ROC curve for class 4")


##################################################################
# Predict using all four predictors

summary(mod.class1)
summary(mod.class2)
summary(mod.class3)
summary(mod.class4)

test <- data

predictions <- matrix(, nrow=4, ncol=717)
pred1 = predict(mod.class1, test, type="response")
predictions[1, ] <- pred1
pred2 = predict(mod.class2, test, type="response")
predictions[2, ] <- pred2
pred3 = predict(mod.class3, test, type="response")
predictions[3, ] <- pred3
pred4 = predict(mod.class4, test, type="response")
predictions[4, ] <- pred4

one.vs.all <- argmax(predictions, rows=FALSE)
accuracy <- sum(one.vs.all == test$class) / 717
accuracy


########################################################
####################Cross validation model
set.seed(1)

index <- createDataPartition(class4_data$class, p = .70, list = FALSE)

regfit.best <- regsubsets(class~., data=class4_data[index,], nvmax=10)
test.mat <- model.matrix(class~., data=class4_data[-index,])

# compute the validation error for the 10 selected models
val.errors <- rep(NA,10)
for(i in 1:10){
  coefi <- coef(regfit.best, id=i)
  pred <- test.mat[,names(coefi)]%*%coefi
  val.errors[i] <- mean((class4_data$class[-index]-pred)^2)
}
val.errors
plot(val.errors,type='b', main='Cross-validation')
which.min(val.errors)
points(10,val.errors[10],col="red",cex=2,pch=20)

coef(regfit.best, 1)
################################################################################

###############################################################################
# VARIABLE SELECTION

regfit.full <- regsubsets(class~., data=data)
summary(regfit.full)

regfit.full <- regsubsets(class~., data=data,nvmax=10)
reg.summary <- summary(regfit.full)
names(reg.summary)
reg.summary$rsq


# Plotting RSS, adjusted  R2,  Cp, and BIC
par(mfrow=c(2,2))

plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS", main='RSS',type="l")
which.min(reg.summary$rss)
points(10,reg.summary$rss[10], col="red",cex=2,pch=20)

# adjusted-R^2 with its largest value
plot(reg.summary$adjr2,xlab="Number",ylab="Adjusted RSq", main='Adjusted R2', type="l")
which.max(reg.summary$adjr2)
points(9,reg.summary$adjr2[9], col="red",cex=2,pch=20)

# Mallow's Cp with its smallest value
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp", main="Mallow's CP",type='l')
which.min(reg.summary$cp)
points(8,reg.summary$cp[8],col="red",cex=2,pch=20)

# BIC with its smallest value
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",main="BIC", type='l')
which.min(reg.summary$bic)
points(5,reg.summary$bic[5],col="red",cex=2,pch=20)
mtext("Subset Selection Models",side=3,outer=TRUE,padj=3)

par(mfrow=c(1,1))

# Plots with the regsubsets() function has a built-in plot()
par(mfrow=c(2,2))
plot(regfit.full,scale="r2", main='RSS')
plot(regfit.full,scale="adjr2", main='Adjusted R2')
plot(regfit.full,scale="Cp", main="Mallow's CP")
plot(regfit.full,scale="bic", main="BIC")
mtext("Best Subset Selection",side=3,outer=TRUE,padj=3)
par(mfrow=c(1,1))

coef(regfit.full,6)

# Data Partition
data$class <- replace(data$class, data$class == 2 | data$class == 3 | data$class == 4, 1)

index <- createDataPartition(data$class, p = .50, list = FALSE)
train <- data[index,]
test <- data[-index,]
test<-test[order(test$class),]

# Best model "RSS"

best.rss <- glm(class~., data=data)
summary(best.rss)
y.pred <- predict(best.rss, test, type="response")
y.pred[y.pred >.5] <- 1
y.pred[y.pred <.5] <- 0
accuracy <- sum(y.pred == test$class) / 717
accuracy

# Best model "RSS"

best.rss <- glm(class~., data=data)
y.pred <- predict(best.rss, test, type="response")
y.pred[y.pred >.5] <- 1
y.pred[y.pred <.5] <- 0
accuracy <- sum(y.pred == test$class) / 717
accuracy
summary(best.rss)

# Best model "Adjusted R2"

best.r2 <- glm(class~sex+cp+trestbps+chol+fbs+restecg+thalach+exang+oldpeak, data=data)
y.pred <- predict(best.r2, test, type="response")
y.pred[y.pred >.5] <- 1
y.pred[y.pred <.5] <- 0
accuracy <- sum(y.pred == test$class) / 717
print(accuracy)
summary(best.r2)

# Best model "Mallow"

best.mallow <- glm(class~sex+cp+trestbps+fbs+restecg+thalach+exang+oldpeak, data=data)
y.pred <- predict(best.mallow, test, type="response")
y.pred[y.pred >.5] <- 1
y.pred[y.pred <.5] <- 0
accuracy <- sum(y.pred == test$class) / 717
accuracy
summary(best.mallow)

# Best model "BIC"

best.bic <- glm(class~sex+cp+thalach+exang+oldpeak, data=data)
y.pred <- predict(best.bic, test, type="response")
y.pred[y.pred >.5] <- 1
y.pred[y.pred <.5] <- 0
accuracy <- sum(y.pred == test$class) / 717
accuracy
summary(best.bic)


#########################
# Best model "BIC"
n <- 717
coef(regfit.full,6)

#BIC Full
best.bic.full <- glm(class~., data=data)
summary(best.bic.full)
RSS.full <- sum(residuals(best.bic.full)^2)
y.pred <- predict(best.bic.full, test, type="response")
y.pred[y.pred >.5] <- 1
y.pred[y.pred <.5] <- 0
accuracy <- sum(y.pred == test$class) / 717
accuracy

# BIC reduced
best.bic.red <- glm(class~+sex+cp+thalach+exang+oldpeak, data=data)
summary(best.bic.red)
RSS.red <- sum(residuals(best.bic.red)^2)
y.pred <- predict(best.bic.red, test, type="response")
y.pred[y.pred >.5] <- 1
y.pred[y.pred <.5] <- 0
accuracy <- sum(y.pred == test$class) / 717
accuracy

plot(best.bic.red)

BIC.full <- n*log(RSS.full/n) + log(n)*11
BIC.red  <- n*log(RSS.red/n) + log(n)*6
BIC.full
BIC.red

par(mfrow=c(2,2))
plot(best.bic.red)
mtext("Best model BIC Model",side=3,outer=TRUE,padj=3)
par(mfrow=c(1,1))

##########################
# Forward Step-wise selection
regfit.fwd <- regsubsets(class~.,data=data,nvmax=10,method="forward")
summary(regfit.fwd)

#Backward Step-wise Selection
regfit.bwd <- regsubsets(class~.,data=data,nvmax=10,method="backward")
summary(regfit.bwd)

coef(regfit.full,5)
coef(regfit.fwd,5)
coef(regfit.bwd,5)

###################################################################
# Resampling Methods

####################################################################
# cross-validation 
set.seed(1)

train <- sample(c(TRUE,FALSE), nrow(data),rep=TRUE)
test <- (!train)
regfit.best <- regsubsets(class~., data=data[train,], nvmax=10)
test.mat <- model.matrix(class~., data=data[test,])

# compute the validation error for the 10 selected models
val.errors <- rep(NA,10)
for(i in 1:10){
  coefi <- coef(regfit.best, id=i)
  pred <- test.mat[,names(coefi)]%*%coefi
  val.errors[i] <- mean((data$class[test]-pred)^2)
}
val.errors

plot(val.errors,type='b', main='Cross-validation')
which.min(val.errors)
points(8,val.errors[8],col="red",cex=2,pch=20)

coef(regfit.best, 8)

# Data Partition
data$class <- replace(data$class, data$class == 2 | data$class == 3 | data$class == 4, 1)
index <- createDataPartition(data$class, p = .70, list = FALSE)
train <- data[index,]
test <- data[-index,]
test<-test[order(test$class),]

# Model
glm.fit <- glm(class~age+fbs+trestbps+restecg+cp+thalach+exang+oldpeak, data=train, family=binomial)
y.pred <- predict(glm.fit, test, type="response")

y.pred[y.pred >.5] <- 1
y.pred[y.pred <.5] <- 0
accuracy <- sum(y.pred == test$class) / 717
print(accuracy)

# MSE
Err <- sum(y.pred != test$class) / 717
Err 

# fitting polynomials of degree from 1 to 10
MSE.val <- c()
MSE.train <-c()
for(i in 1:10){
  lm.fit <- lm(class~poly(oldpeak, degree=i), data=train)
  MSE.train <- c(MSE.train, mean(residuals(lm.fit)^2))
  y.pred <- predict(lm.fit, test)
  mse <- mean((test$class-y.pred)^2)
  MSE.val <-  c(MSE.val, mse)
  
}

# validation MSE for the ten models
MSE.val

# validation MSE vs training MSE

plot(1:10, MSE.val, type="l", ylim=c(0.19, 0.30))
lines(1:10, MSE.train, col="blue")

###########################
# k-fold cross-validation
set.seed(1)
k=10
folds <- sample(1:k, nrow(data), replace=TRUE)
cv.errors <- matrix(NA,k,10)
colnames(cv.errors) <- 1:10

for(j in 1:k){
  best.fit <- regsubsets(class~., data=data[folds!=j,], nvmax=10)
  test.mat <- model.matrix(class~., data=data[folds==j,])
  for(i in 1:10){
    coefi <- coef(best.fit, id=i)
    pred <- test.mat[,names(coefi)]%*%coefi
    cv.errors[j,i] <- mean( (data$class[folds==j]-pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors,2,mean)
mean.cv.errors

plot(mean.cv.errors,type='b', main = '10-fold Cross-validation')
which.min(mean.cv.errors)
points(10,mean.cv.errors[10],col="red",cex=2,pch=20)

reg.best <- regsubsets(class~.,data=data, nvmax=10)
coef(reg.best,10)

# Model
set.seed(17)
cv.error.10 <- rep(0,10)
for (i in 1:10){
  glm.fit <- glm(class~poly(oldpeak,i),data=train)
  cv.error.10[i] <- cv.glm(train,glm.fit,K=10)$delta[1]
}

##############################################################################
# RIDGE REGRESSION

# Construct objects required to apply the glmnet() function
# design matrix 
X <- model.matrix(class~.,data)

# remove the first column relative to the intercept 
X <- X[,-1]

# vector of responses
y <- data$class

# grid of lambda values
grid <- 10^seq(10, -2, length=100)

# ridge regression
# data is already normalized so standardize = False 
ridge_mod <- glmnet(X,y,alpha=0,lambda=grid, standardize=FALSE)

plot(ridge_mod)

# coef() gives a matrix of coefficients

dim(coef(ridge_mod))

# coefficients and ell_2 norm for lambda[50]

ridge_mod$lambda[50]
coef(ridge_mod)[,50]
sqrt(sum(coef(ridge_mod)[-1,50]^2))

# coefficients and ell_2 norm for lamba[60]

ridge_mod$lambda[60]
coef(ridge_mod)[,60]
sqrt(sum(coef(ridge_mod)[-1,60]^2))

# use the predict function for a number of purposes
# s=lambda (interpolates over the grid of lambda used in fitting)
predict(ridge_mod, s=60, type="coefficients")

# 10-fold cross-validation 
set.seed(1)
cv.out <- cv.glmnet(X, y, alpha=0, nfolds=10, lambda=grid)

# lambda values
cv.out$lambda[1:10]
summary(cv.out$lambda)

# The mean cross-validated error - a vector of length length(lambda).

# estimate of standard error of cvm
cv.out$cvm[1:10]
cv.out$cvsd[1:10]



i.bestlam <- which.min(cv.out$cvm)
i.bestlam 
bestlam <- cv.out$lambda[i.bestlam]
bestlam
cv.out$cvm[bestlam]

# Best lambda
bestlam <- cv.out$lambda.min
bestlam

# This plots the cross-validation curve (red dotted line) along with upper and lower standard deviation curves
# along the ?? sequence (error bars). Two special values along the ?? sequence are indicated by the vertical
# dotted lines. lambda.min is the value of ?? that gives minimum mean cross-validated error, while lambda.1se
# is the value of ?? that gives the most regularized model such that the cross-validated error is within one
# standard error of the minimum.

plot(cv.out)

# Ridge Regression Model
ridge.mod <- glmnet(X, y, alpha=0, lambda=bestlam)
coef(ridge.mod)


##########################################
# Prediction

# Data Partition
data$class <- replace(data$class, data$class == 2 | data$class == 3 | data$class == 4, 1)
train <- data %>% sample_frac(0.5)
test <- data %>% setdiff(train)

x_train = model.matrix(class~., train)[,-1]
x_test = model.matrix(class~., test)[,-1]

y_train = train %>%
  dplyr::select(class) %>%
  unlist() %>%
  as.numeric()


y_test = test %>%
  dplyr::select(class) %>%
  unlist() %>%
  as.numeric()

ridge.mod <- glmnet(x_train, y_train, alpha=0, lambda=bestlam)
y.pred <- predict(ridge.mod,s = bestlam, newx = x_test)

y.pred[y.pred >.5] <- 1
y.pred[y.pred <.5] <- 0
accuracy <- sum(y.pred == test$class) / 717
print(accuracy)

################################################################################

###########################
# LASSO REGRESSION
###########################

# Construct objects required to apply the 
# glmnet() function


# design matrix 
X <- model.matrix(class~.,data)

# remove the first column relative to the intercept 
X <- X[,-1]

# vector of responses

y <- data$class

# grid of lambda values

grid <- 10^seq(10, -2, length=100)

lasso.mod <- glmnet(X,y,alpha=1,lambda=grid)

round(coef(lasso.mod)[,50], 3)
round(coef(lasso.mod)[,70], 3)
round(coef(lasso.mod)[,90], 3)

# Cross-valdiation to find best lambda
set.seed(1)
cv.out <- cv.glmnet(X,y,alpha=1, lambda=grid)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

lasso.mod <- glmnet(X, y, alpha=1, lambda=bestlam)
lasso.coef <- coef(lasso.mod)[,1]
round(lasso.coef, 3)

lasso.coef[lasso.coef!=0]

##########################################
# Prediction

# Data Partition
data$class <- replace(data$class, data$class == 2 | data$class == 3 | data$class == 4, 1)
train <- data %>% sample_frac(0.7)
test <- data %>% setdiff(train)

x_train = model.matrix(class~., train)[,-1]
x_test = model.matrix(class~., test)[,-1]

y_train = train %>%
  dplyr::select(class) %>%
  unlist() %>%
  as.numeric()


y_test = test %>%
  dplyr::select(class) %>%
  unlist() %>%
  as.numeric()

# Best Lasso Model
lasso.mod <- glmnet(x_train, y_train, alpha=1, lambda=bestlam)
y.pred <- predict(lasso.mod,s = bestlam, newx = x_test)

y.pred[y.pred >.5] <- 1
y.pred[y.pred <.5] <- 0
accuracy <- sum(y.pred == test$class) / 717
print(accuracy)

##############################################################################
##############################################################################
# Convert class to factor
is.factor(class)
class.f <- as.factor(class)
is.factor(class.f)

# Contrast
contrasts(class.f)

# model
mod.out <- glm(data$sex ~ class.f)
summary(mod.out)

par(mfrow=c(2,2))
plot(mod.out)
par(mfrow=c(1,1))

# bartlett.test() -> for equal variances
# sex+cp+thalach+exang+oldpeak
# ANOVA

bartlett.test(data$sex~class.f)
aov_sex <- aov(data$sex ~ class.f)
summary(aov_sex)

bartlett.test(data$cp~class.f)
aov_cp <- aov(data$cp ~ class.f)
summary(aov_cp)

bartlett.test(data$thalach~class.f)
aov_thalach <- aov(data$thalach ~ class.f)
summary(aov_thalach)

bartlett.test(data$exang~class.f)
aov_exang <- aov(data$exang ~ class.f)
summary(aov_exang)

bartlett.test(data$oldpeak~class.f)
aov_oldpeak <- aov(data$oldpeak ~ class.f)
summary(aov_oldpeak)

# post-hoc analysis

tukey.class <- TukeyHSD(aov_sex)
tukey.diet

par(mfrow=c(2, 1))
plot(data$sex ~ class.f)
plot(tukey.class)
par(mfrow=c(1,1))

##################################################################################
# ANCOVA
##################################################################################


coplot(cp ~ oldpeak | class, panel=panel.smooth, data=data)
abuse <- lm(cp ~ oldpeak * class, data=data)
summary(abuse)

data$class <- replace(data$class, data$class == 2 | data$class == 3 | data$class == 4, 1)
l <- class == 1
plot(data$oldpeak, class, pch=l+16, col=l+1, xlab="oldpeak", ylab="class")
beta.hat <- coefficients(data)
abline(beta.hat[1], beta.hat[2], col="red", lwd=2)
abline(beta.hat[1]+beta.hat[3], beta.hat[2]+beta.hat[4], col="black", lwd=2)
legend(4,1, legend = c("not sexually abused", "sexually abused"), col=1:2, pch=16:17)