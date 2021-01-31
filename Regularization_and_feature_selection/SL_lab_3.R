#### Dawid Skomorochow
#### Filip Uherek

library(caTools)
library(Rcmdr)
library(rcompanion)
library(lmtest)
library(oddsratio)
library(gplots)
library(caret)
library("OptimalCutpoints")
library(plyr)
library(glmnet)
library(ggplot2)
library(reshape2)

r2_general <-function(preds,actual){ 
  return(1- sum((preds - actual) ^ 2)/sum((actual - mean(actual))^2))
}

set.seed(222)

# 1. Data preparation

dataset_lab3 <- read.delim("~/Downloads/dataset_lab3.txt")
View(dataset_lab3)

#check for data types
sapply(dataset_lab3, class)

levels(dataset_lab3$Gender)
# "Female" "Male" 
levels(dataset_lab3$Married)
#"No"  "Yes"
levels(dataset_lab3$Ethnicity)
#"African American" "Asian"            "Caucasian" 

#data <-model.matrix(~ ., data=dataset_lab3, 
#                    contrasts.arg = lapply(dataset_lab3[7:9], contrasts, contrasts=FALSE))
data <-model.matrix(~ ., data=dataset_lab3)[,-1]
#spread dataset to X and y variable

X <- data[,1:10]
y <- data[,11]

#train/test split
sample <- sample.split(dataset_lab3[,1], SplitRatio = 0.8)

X_train <- subset(X, sample == TRUE)
y_train <- subset(y, sample == TRUE)

X_test <- subset(X, sample == FALSE)
y_test <- subset(y, sample == FALSE)


#Set initial lambdas
lambdas <- 10^seq(10,-2, length.out = 50)
#LAMBDA WITH 1 STD

# 2. Ridge regression
ridge_reg <- cv.glmnet(X_train, y_train,alpha=0, lambda = lambdas, type.measure = 'mse', nfolds = 10)
plot(ridge_reg)

#best lambda
best_lambda_ridge <- ridge_reg$lambda.1se
best_coef_ridge <- predict(ridge_reg$glmnet.fit, s=best_lambda_ridge, type='coef')
#plot coeffcients for lambdas
coef_lam_plot_ridge <- plot(ridge_reg$glmnet.fit,xvar="lambda", col=1:dim(coef(ridge_reg))[1], label=TRUE) +
              abline(v=log(best_lambda_ridge), lty=2, col='blue') +
              text(x=log(best_lambda_ridge), y=25, label ='MSE') +
              legend('topright', legend=rownames(best_coef_ridge)[2:11], col=1:dim(coef(ridge_reg))[1], pch=16)

#build best lambda
reg_ridge <- glmnet(X_train, y_train,alpha=0, lambda = best_lambda_ridge, type.measure = 'mse')

#prediction on train set
pred_ridge_train <- predict(reg_ridge, X_train)
r2_train_ridge <- r2_general(pred_ridge_train, y_train)
r2_train_ridge

#prediction on test set
pred_ridge_test <- predict(reg_ridge, X_test)
r2_test_ridge <- r2_general(pred_ridge_test, y_test)
r2_test_ridge


# 3. Lasso regression

lasso_reg <- cv.glmnet(X_train, y_train,alpha=1, lambda = lambdas, type.measure = 'mse', nfolds = 10)
plot(lasso_reg)

#best lambda
best_lambda_lasso <- lasso_reg$lambda.1se
best_lambda_lasso
#15.26418
best_coef_lasso <- predict(lasso_reg$glmnet.fit, s=best_lambda_lasso, type='coef')
#plot coeffcients for lambdas
coef_lam_plot_lasso <- plot(lasso_reg$glmnet.fit,xvar="lambda", col=factor(c(1:dim(coef(lasso_reg))[1])), label=TRUE) +
  abline(v=log(best_lambda_lasso), lty=2, col='blue') +
  text(x=log(best_lambda_lasso), y=25, label ='MSE') +
  legend('topright',col=factor(c(1:dim(coef(lasso_reg))[1])), legend=c(rownames(best_coef_lasso)[2:11]), pch=16)

#build best lambda
reg_lasso <- glmnet(X_train, y_train,alpha=1, lambda = best_lambda_lasso, type.measure = 'mse')

#predict on train set
pred_lasso_train <- predict(reg_lasso, X_train)
r2_train_lasso <- r2_general(pred_lasso_train, y_train)
r2_train_lasso

#predict on test set
pred_lasso_test <- predict(reg_lasso, X_test)
r2_test_lasso <- r2_general(pred_lasso_test, y_test)
r2_test_lasso


# 4. Lasso vs ridge - Compare

best_coef_lasso <- as.matrix(best_coef_lasso)
best_coef_ridge <- as.matrix(best_coef_ridge)
d <- data.frame(names=rownames(best_coef_ridge)[-1],lasso=best_coef_lasso[,1][-1], ridge=best_coef_ridge[,1][-1])
d <- melt(d)
names(d) <- c("Features", "method", "Coefficients")
ggplot(d, aes(x = Features, y= Coefficients, fill = method)) +
  geom_bar(stat="identity", width=.5, position = "dodge")


# 5. Recursive Feature Elimination - wrapper

control <- rfeControl(functions= lmFuncs, method="repeatedcv", number=10, repeats=5)
recursive_model <- rfe(X_train, y_train, rfeControl=control)
recursive_model
summary(recursive_model$fit)

pred_test <- predict(recursive_model, X_test)

#predict on train set
pred_recursive_train <- predict(recursive_model, X_train)
r2_train_recursive <- r2_general(pred_recursive_train, y_train)
r2_train_recursive

#predict on test set
pred_recursive_test <- predict(recursive_model, X_test)
r2_test_recursive <-r2_general(pred_recursive_test, y_test)
r2_test_recursive

