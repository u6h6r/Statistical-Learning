library(caTools)
library(Rcmdr)
library(rcompanion)
library(lmtest)
library(oddsratio)
library(gplots)
library(caret)
library("OptimalCutpoints")
library(plyr)

set.seed(420)
##1
#read dataset
data <- read.delim("data/dataset2.txt")
View(data)
#check for data types
sapply(data, class)
#change no.of.fam.members to factor type
data$no.of.fam.members <- as.factor(data$no.of.fam.members)
data$Credit.history <- as.factor(data$Credit.history)
sapply(data, class)

barplot(table(data$Status), xlab = "Status", ylab = "# of observations")
##2
#split data 
sample <- sample.split(data[,1], SplitRatio = 0.8)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
summary(train)
summary(test)

## 3
#Backward approach

#Model
glm_full <- glm(Status ~ ., family = binomial(link = "logit"), train)
summary(glm_full)
# verify BIC for full model
BIC(glm_full)
#353.1236

#null model
null_model <- glm(Status ~ 1, family = binomial(link = 'logit'), train)
summary(null_model)
# verify BIC for full model
BIC(null_model)
#396.3728

#get best model according to BIC
glm_step_bic <- step(glm_full, k = log(nrow(train)), direction="backward")
summary(glm_step_bic)
BIC(glm_step_bic)

#z-test
results_z <- coef(summary(glm_step_bic))[,"Pr(>|z|)"]

#Null hypothesis: B = 0
#Alternative hypothesis: B !=0
#alpha = 0.05

#(Intercept) 
#z_value = 4.942
#p_val = 7.72e-07
#p_val < alpha
# Reject null hypothesis and assume that B != 0

#MarriageYes 
#z_value = -2.578
#p_val = 0.00994
#p_val < alpha
# Reject null hypothesis and assume that B != 0

#Income 
#z_value = -4.593
#p_val = 4.37e-06
#p_val < alpha
# Reject null hypothesis and assume that B != 0

#Credit.in.thousend 
#z_value = 3.260
#p_val = 0.00111
#p_val < alpha
# Reject null hypothesis and assume that B != 0

#Credit.history1 
#z_value = -6.164
#p_val = 7.07e-10
#p_val < alpha
# Reject null hypothesis and assume that B != 0

#Region.of.buildingCountrysid 
#z_value = 0.844
#p_val = 0.39843
#p_val > alpha
# Fail to reject null hypothesis and assume that B = 0

#Region.of.buildingSuburbs 
#z_value = -2.333
#p_val = 0.01966
#p_val < alpha
# Reject null hypothesis and assume that B != 0


#reults BIC
BIC(glm_step_bic)
#294.4876

#calculate adjusted McFadden 
#data dim 12
LL.null <- logLik(null_model)
LL.mod <- logLik(glm_step_bic)
1-((LL.mod-dim(data)[2])/LL.null)
#'log Lik.' 0.2902302 (df=7)
#'
#'For our model we did achieve an adjusted McFadden of 0.29. To compare its
# value and as from what we know is that adjusted McFadden favors the more
#complex models having more parameters. For the full model (all of the features
#included) the value of McFadden is 0.304. But according to other metrics of
#measure which take complexity of mode into consideration we concluded that our
#model from backward step approach is a better model.

#performe LRT test 

#model vs null_model

#Perform Likelihood Ratio Test to decide if one model is better than other
#Null hypothesis: Null model is better
#Alternative hypothesis: Model with more features is better

lrtest(null_model, glm_step_bic)
#Likelihood ratio test
#
#Model 1: Status ~ 1
#Model 2: Status ~ Marriage + Income + Credit.in.thousend + Credit.history + 
#  Region.of.building
##Df  LogLik Df Chisq Pr(>Chisq)    
#1   1 -196.06                        
#2   7 -127.16  6 137.8  < 2.2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#We reject null hypothesis and assume that model with more parameters is a better model

# model vs full_model

#Null hypothesis: Model from backward approach is better
#Alternative hypothesis: Full model is better

lrtest(glm_step_bic, glm_full)
#Likelihood ratio test
#
#Model 1: Status ~ Marriage + Income + Credit.in.thousend + Credit.history + 
#  Region.of.building
#Model 2: Status ~ Sex + Marriage + no.of.fam.members + Education + Self.employ + 
#  Income + Income.other.fam.member + Credit.in.thousend + Time + 
#  Credit.history + Region.of.building
##Df  LogLik Df  Chisq Pr(>Chisq)
#1   7 -127.16                     
#2  15 -124.27  8 5.7667     0.6734

#We fail to reject the null hypotesis so we assume that the model from backward approach is a better model

#Calculate 95% confidence intervals for obtained coefficients
exp(confint(glm_step_bic))
#2.5 %        97.5 %
#  (Intercept)                    2.4227995944  5.4974708218
#MarriageYes                   -1.5127740857 -0.2083210432
#Income                        -0.0006587544 -0.0002726796
#Credit.in.thousend             0.0035846381  0.0135020289
#Credit.history1               -4.9672000922 -2.6015463958
#Region.of.buildingCountryside -0.4199499512  1.0708565611
#Region.of.buildingSuburbs     -1.7959936748 -0.1631043401

# OR
exp(coef(glm_step_bic))
#(Intercept)                   MarriageYes                        Income 
#46.21982851                    0.42542103                    0.99954814 
#Credit.in.thousend               Credit.history1 Region.of.buildingCountryside 
#1.00828382                    0.02610896                    1.37697526 
#Region.of.buildingSuburbs 
#0.38051032 

#error bar
co <- exp(coef(glm_step_bic))
cint <- exp(confint(glm_step_bic, level = 0.95))
co <- co[2:7]
cint <- cint[2:7,]

d=data.frame(co_names=names(co), point=co, lower=cint[,1], upper=cint[,2])
error_bar <- ggplot() + 
  geom_pointrange(data=d, mapping=aes(x=co_names, y=point, ymin=lower, ymax=upper), size=0.4, color='blue') + 
  geom_hline(yintercept=1, linetype='dashed', color='red')

#Looking at the plot and OR results we can conclude that the biggest factors
#increasing risk of credit rejection are Region.of.buildingCountryside (Not
#significant) and Credit.in.thousend while on other hand, Credit.history1,
#Region.of.buildingSuburbs, MarriageYes and Income shows "protection" from
#rejection of credit application

#Youden index 
thr_sequence <- seq(0.1,0.9, length.out = 1000)
truth <- train$Status
truth <- revalue(truth, c("Accepted"=0, "Reject"=1))

get_youden_index <- function(model, actual_values){
  thr_sequence <- seq(0.1,0.9, length.out = 1000)
  result_vector <- rep()
  for (thr in thr_sequence)
  {
    predicted_values<-ifelse(predict(model ,type="response")>thr ,1,0)
    conf_matrix<-table(predicted_values,actual_values)
    sens <- sensitivity(conf_matrix)
    spec <- specificity(conf_matrix)
    youden_index <-sens+spec-1
    result_vector <- append(result_vector, youden_index)
  }
  return(result_vector)
  
}

#figure for thr vs Youden index
results <- get_youden_index(glm_step_bic, truth)
plot(thr_sequence, results, type="l")

#get the best value according to youden index
max(results)
max_index <- which.max(results)
best_thr <- thr_sequence[max_index]
best_thr


# MODEL on train dataset
prob_glm_final_train <- predict(glm_step_bic, train, type = "response")
pref_df <- data.frame(pred = prob_glm_final_train, truth = truth)
oc <- optimal.cutpoints(X = "pred", status = "truth", methods="Youden", data=pref_df, tag.healthy = "0")
summary(oc)

#cutoff 0.2028362
predicted_values<-ifelse(predict(glm_step_bic ,type="response")>0.2028362 ,1,0)
conf_matrix_train<-table(truth, predicted_values)
conf_matrix_train

specificity(conf_matrix_train)
#0.9009901
sensitivity(conf_matrix_train)
#0.6428571
acc_train <- (conf_matrix_train[1]+conf_matrix_train[4])/(conf_matrix_train[1]+conf_matrix_train[2]+conf_matrix_train[3]+conf_matrix_train[4])
acc_train
#0.7266881
plot(oc,which=1)


# MODEL on test dataset
truth_test <- test$Status
truth_test <- revalue(truth_test, c("Accepted"=0, "Reject"=1))

pred_glm_final_test <- predict(glm_step_bic, test, type='response')
pref_df_test <- data.frame(pred = prob_glm_final_test, truth = truth_test)
oc_test <- optimal.cutpoints(X = "pred", status = "truth", methods="Youden", data=pref_df_test, tag.healthy = "0")
summary(oc_test)

predicted_values_test<-ifelse(predict(glm_step_bic, test ,type="response")>0.3424136 ,1,0)
conf_matrix_test<-table(predicted_values_test, truth_test)
conf_matrix_test

specificity(conf_matrix_test)
#0.5
sensitivity(conf_matrix_test)
#0.8571429
acc_test <- (conf_matrix_test[1]+conf_matrix_test[4])/(conf_matrix_test[1]+conf_matrix_test[2]+conf_matrix_test[3]+conf_matrix_test[4])
#0.7564103
plot(oc_test,which=1)


#FORWARD
glm_step_bic_forward <- step(null_model, k = log(nrow(train)), direction="forward", scope=formula(glm_full))
summary(glm_step_bic_forward)
BIC(glm_step_bic_forward)
#Same as for backward approach
#294.4876