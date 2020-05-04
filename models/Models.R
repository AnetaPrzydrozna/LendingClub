
library(pROC)
library(MASS)
library(ROCR)
library(stats)
library(caret)
library(xgboost)
library(randomcoloR)
library(dplyr)
library(smbinning)

######################################### Logistic Regression model
model_glm<-glm(loan_train$loan_status~., family = "binomial",data=loan_train) 
#summary(model_glm)
step_glm<-stepAIC(model_glm ,steps =500) 
model_step<-glm( formula =step_glm$formula , family = "binomial",data=loan_train)  #logistic regression stepwise
#model_glm<-train(formula=step_glm$formula , method = "glmnet",data=loan_train)



#########################################    logistic regression in subinning, Optimal Binning for Scoring Modeling
#inspired: http://r-statistics.co/Logistic-Regression-With-R.html


# segregate continuous and factor variables
factor_vars <- c ("term", "grade", "emp_length")  # other data have too many labels

continuous_vars <- c("loan_amnt", "int_rate","installment", "annual_inc", "dti", "inq_last_6mths","revol_bal","revol_util")

iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(13))  # init for IV results

loan_train$loan_status<-ifelse(loan_train$loan_status=="1",1,0)
loan_test$loan_status<-ifelse(loan_test$loan_status=="1",1,0)

# compute IV for categoricals
for(factor_var in factor_vars){
  smb <- smbinning.factor(loan_train, y="loan_status", x=factor_var)  
  if(class(smb) != "character"){ # heck if some error occured
    iv_df[iv_df$VARS == factor_var, "IV"] <- smb$iv
  }
}

# compute IV for continuous vars
for(continuous_var in continuous_vars){
  smb <- smbinning(loan_train, y="loan_status",  x=continuous_var)  
  if(class(smb) != "character"){  # any error while calculating scores.
    iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
  }
}

iv_df <- iv_df[order(-iv_df$IV), ]  # sort
iv_df

model_iv<-glm( loan_status~grade+int_rate+term+dti, family = "binomial",data=loan_train) 


######################################### XGBoost stands for eXtreme Gradient Boosting

#inspired: https://rstudio-pubs-static.s3.amazonaws.com/203258_d20c1a34bc094151a0a1e4f4180c5f6f.html

#str(loan)
loan_train_num<-loan_train[,c(1,3,4,8,11,12,13,14)]
loan_test_num<-loan_test[,c(1,3,4,8,11,12,13,14)]

############## default
test_watchlist = list(
  test = xgb.DMatrix(
    data = as.matrix(loan_train_num),
    label = as.numeric(loan_train$loan_status)-1
  )
)

model_xgb = xgb.train(
  data= xgb.DMatrix(
    data = as.matrix(loan_train_num),
    label = as.numeric(loan_train$loan_status)-1
  ),
  objective = "binary:logistic",
  nrounds = 350,
  watchlist = test_watchlist,
  eval_metric = "auc",
  early.stop.round = 10)


############# with parameters
 
etas = c(0.1,0.3)
alphas = c(0,0.5,1)
lambdas = c(0,0.5,1)


gbm_perf = data.frame(eta=numeric(0),alpha=numeric(0),lambda=numeric(0),auc=numeric(0))
for(eta in etas){
  for(alpha in alphas){
    for(lambda in lambdas){
      model = xgb.train(
        data= xgb.DMatrix(
          data = as.matrix(loan_train_num),
          label = as.numeric(loan_train$loan_status)-1
        ),
        objective = "binary:logistic",
        nrounds = 350,
        watchlist = test_watchlist,
        eval_metric = "auc",
        alpha = alpha,
        early.stop.round = 10,
        lambda = lambda,
        eta = eta)
      gbm_perf[nrow(gbm_perf)+1,] = c(eta,alpha,lambda,model$best_score)
    }
  }
}

gbm<-gbm_perf[which(gbm_perf[,4]>=max(gbm_perf[,4])) ,]

model_xgb_grid = xgb.train(
  data= xgb.DMatrix(
    data = as.matrix(loan_train_num),
    label = as.numeric(loan_train$loan_status)-1
  ),
  objective = "binary:logistic",
  nrounds = 350,
  watchlist = test_watchlist,
  eval_metric = "auc",
  early.stop.round = 10,
  alpha = gbm$alpha,
  lambda = gbm$lambda,
  eta = gbm$eta)


############################################## Comparison

comparison<-function(model,loan_test_status,loan_test,name){
  
  pred = predict(model,loan_test,type="response")
  roc= roc(response = loan_test_status,predictor = pred)
  auc = auc(roc)
  
  threshold<-coords(roc, "best", "threshold", transpose = TRUE)
  pred<-ifelse(pred> threshold[1],1,0)
  cm <- confusionMatrix(as.factor(pred), as.factor(loan_test_status))
  
  result= data.frame(Model=name,
                      AUC=round(auc,3),
                      Accuracy=round(cm$overall["Accuracy"],3),
                      Sensitivity=round(cm$byClass["Sensitivity"],3),
                      Specificity=round(cm$byClass["Specificity"],3),
                      GC=round(2*auc-1,3 ),
                      stringsAsFactors = FALSE  #for model
  )
  
  plot<-plot(roc,legacy.axes = TRUE,print.auc = TRUE,col=randomColor(luminosity = "dark"),main=name)
  return(result)
}



#############################################  Results

results= data.frame(Model=character(0),
                    AUC=numeric(0),
                    Accuracy=numeric(0),
                    Sensitivity=numeric(0),
                    Specificity=numeric(0),
                    GC=numeric(0),
                    stringsAsFactors = FALSE  
)



result<-comparison(model_glm,loan_test$loan_status,loan_test,"logistic regression")
results<-rbind(results, result, make.row.names = FALSE)

result<-comparison(model_step,loan_test$loan_status,loan_test,"logistic regression- Stepwise")
results<-rbind(results, result, make.row.names = FALSE)

result<-comparison(model_iv,loan_test$loan_status,loan_test,"logistic regression- IV")
results<-rbind(results, result, make.row.names = FALSE)

result<-comparison(model_xgb,loan_test$loan_status,as.matrix(loan_test_num),"xgboost- default parameters")
results<-rbind(results, result, make.row.names = FALSE)

result<-comparison(model_xgb_grid,loan_test$loan_status,as.matrix(loan_test_num),"xgboost- grid search")
results<-rbind(results, result, make.row.names = FALSE)

results
