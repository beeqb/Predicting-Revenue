library(dplyr)
library(ggplot2)
library(pROC)

##### Read Data
sales_df = read.csv('../_data/catalog sales data.csv')
sales_df$uid = row.names(sales_df)
sales_train = sales_df %>% filter(train==1)
sales_test = sales_df %>% filter(train==0)

##### Linearize and Normalize Transformations
data_manipulate <- function(sales_train){
  # transform date related column into date format
  sales_train$datelp6 = as.Date(sales_train$datelp6, "%m/%d/%Y")
  sales_train$datead6 = as.Date(sales_train$datead6, "%m/%d/%Y")
  
  # take log transformation of targdol
  sales_train$log_targdol = log(sales_train$targdol+1)
  
  # take log transformation of sales related columns
  sales_train$log_slstyr = log(sales_train$slstyr+1)
  sales_train$log_slslyr = log(sales_train$slslyr+1)
  sales_train$log_sls2ago = log(sales_train$sls2ago+1)
  sales_train$log_sls3ago = log(sales_train$sls3ago+1)
  
  # create the response variable for logistic regression
  sales_train$targdol_bol = ifelse(sales_train$targdol!=0, 1, 0)
  
  # create a new column ordhist_max with the highest value between ordhist and falord + sprord
  sales_train$ordhist_new = sales_train$falord + sales_train$sprord
  sales_train$ordhist_max = sales_train$ordhist_new
  for (i in 1:nrow(sales_train)){
    if (sales_train$ordhist_max[i] < sales_train$ordhist[i]){
      sales_train$ordhist_max[i] = sales_train$ordhist[i]
    }
  }
  
  # A variable calculate the consistency of a consumers
  subset_3yrsales = sales_train[,c("slslyr" , "sls2ago", "sls3ago")]
  RowVar <- function(x) {
    rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
  }
  salesvar = RowVar(subset_3yrsales)
  salesmean = rowMeans(subset_3yrsales)
  salescv = ifelse(salesmean!=0, salesvar/salesmean, 5000)
  sales_train$sales_consistency = salescv
  
  # Use the most recent year, from datelp6 & lpuryear
  sales_train$datelp6 <- as.integer(format(sales_train$datelp6, "%Y"))
  sales_train <- sales_train[sales_train['datelp6'] != 1980,]
  sales_train$lpuryear <- ifelse(sales_train$lpuryear == 2 , 2012,
                                 ifelse(sales_train$lpuryear == 1, 2011,
                                        ifelse(sales_train$lpuryear == 0, 2010,
                                               ifelse(sales_train$lpuryear == 9, 2009,
                                                      ifelse(sales_train$lpuryear == 8, 2008,
                                                             ifelse(sales_train$lpuryear == 7, 2007,
                                                                    ifelse(sales_train$lpuryear == 6, 2006,
                                                                           ifelse(sales_train$lpuryear == 5, 2005,
                                                                                  ifelse(sales_train$lpuryear == 4, 2004,
                                                                                         ifelse(sales_train$lpuryear == 3, 2003,
                                                                                                2002))))))))))
  
  # Use the most recent year, from datelp6 & lpuryear
  sales_train$lpuryear_new <- ifelse(sales_train$datelp6 >= sales_train$lpuryear, sales_train$datelp6, sales_train$lpuryear)
  sales_train$lpuryear_new <- 2012 - sales_train$lpuryear_new
  return(sales_train)
}

sales_train_2 = data_manipulate(sales_train)

##### Create Possible Features
# classificaiton_selected_features = c("log_slstyr", "log_slslyr","log_sls2ago","log_sls3ago",
#                                      "ordtyr","ordlyr","ord2ago","ord3ago",
#                                      "ordhist","falord","sprord",
#                                      "targdol_bol")
classificaiton_selected_features = c('slshist','ordhist_max','sales_consistency','lpuryear_new','targdol_bol')


########## Classification
##### Model Fitting
fit_classification = glm(targdol_bol ~ ., family=binomial, data=sales_train_2[classificaiton_selected_features])
summary(fit_classification)

predict_fit = predict(fit_classification, newdata=sales_train_2[classificaiton_selected_features], type="response")

real_response = sales_train_2$targdol_bol

get_optimal_p <- function(real_response, predict_fit, p_threshold_list){
  # get the optimal probability threshold according to CCR
  # @param real_response: actual response vector
  # @param predict_fit: predicted probabilty from the model
  # @param p_threshold_list: list of probability that we want to test on
  # @return: optimal probability threshold according to CCR

  max_ccr= 0
  optimal_p = 0
  for (p in p_threshold_list){
    pred = rep(0, length(real_response))
    pred[predict_fit > p]=1
    ccr = sum(diag(table(real=real_response, pred)))/ length(real_response)
    print(paste("CCR:",ccr," when p=",p))
    
    if (ccr >= max_ccr){
      max_ccr= ccr
      optimal_p = p
    }
  }
  
  return(optimal_p)
}

optimal_p = get_optimal_p(real_response, predict_fit, seq(0.3,0.8,0.01))


calculate_metrics <- function(real_response, predict_fit, optimal_p){
  # a function that calculate all the classification related metrics, including confusion table, auc, ccr, ...etc.
  # @param real_response: actual response vector
  # @param predict_fit: predicted probabilty from the model
  # @param optimal_p: optimal probability threshold according to CCR
  # @return: a list of auc, ccr, sensitivity, precision, f1_score 
  
  # generate the vecotr that transform the fitted result into a vector of classified numbers
  predict_response = rep(0,dim(sales_train_2)[1])
  predict_response[predict_fit>optimal_p[1]]=1
  
  
  confusion_table = table(actual = real_response, prediction = predict_response)
  print(confusion_table)
  
  ccr = sum(diag(confusion_table))/ sum(confusion_table)
  sensitivity=confusion_table[2,2]/(confusion_table[2,1]+confusion_table[2,2])
  specificity=confusion_table[1,1]/(confusion_table[1,1]+confusion_table[1,2])
  precision=confusion_table[2,2]/(confusion_table[1,2]+confusion_table[2,2]) 
  f1_score=2*precision*sensitivity/(precision+sensitivity)
  
  plot.roc(real_response, predict_response, xlab="1-Specificity")
  
  my_auc = auc(real_response, predict_fit)
  
  return(list(auc=my_auc,
              ccr=ccr,
              sensitivity=sensitivity, 
              specificity=specificity,
              f1_score=f1_score))
}

calculate_metrics(real_response, predict_fit, optimal_p)



### classification comparasion
# base model: 
# predictors: "log_slstyr", "log_slslyr","log_sls2ago","log_sls3ago","ordtyr","ordlyr","ord2ago","ord3ago","ordhist","falord","sprord"
#         prediction
# actual     0     1
# 0 44982   582
# 1  3658  1187
# $auc
# Area under the curve: 0.7763
# 
# $ccr
# [1] 0.915888
# 
# $sensitivity
# [1] 0.2449948
# 
# $specificity
# [1] 0.9872268
# 
# $f1_score
# [1] 0.3589356


# Nov 20 : 
# predictors: 'slshist','ordhist_max','sales_consistency','lpuryear_new'
# $auc
# Area under the curve: 0.7852
# 
# $ccr
# [1] 0.9063858
# 
# $sensitivity
# [1] 0.08235294
# 
# $specificity
# [1] 0.9940084
# 
# $f1_score
# [1] 0.1446438




########## Regression
# For regression, we will only use the data with targdol > 0 as our training data
sales_train_reg = sales_train_2 %>% filter(targdol != 0 )


##### Model Fitting
regression_selected_features = c('slshist','ordhist_max','sales_consistency','lpuryear_new',"log_targdol")
# regression_selected_features = c("log_slstyr", "log_slslyr","log_sls2ago","log_sls3ago",
#                                  "ordtyr","ordlyr","ord2ago","ord3ago",
#                                  "ordhist","falord","sprord",
#                                  "log_targdol")

##### > Multiple Linear Regression
fit_multiple = lm(log_targdol~.,data=sales_train_reg[regression_selected_features])
summary(fit_multiple)

model_validation <- function(sales_test, fit_classification, optimal_p, fit_regression, classificaiton_selected_features, regression_selected_features){
  ## predict sales_test_2 using fit_classification
  sales_test_2 = data_manipulate(sales_test)
  predict_classification_final = predict(fit_classification, newdata=sales_test_2[classificaiton_selected_features], type="response")
  
  ## keep the data with targdol prob > optimal_p, save as sales_test_reg
  sales_test_2$targdol_bol_predict = predict_classification_final
  sales_test_2$uid = row.names(sales_test_2)
  sales_test_reg = sales_test_2 %>% filter(targdol_bol_predict>optimal_p)
  
  ## predict sales_test_reg using fit_regression, and take exp(log_targdon) to recover back to real measure
  predict_regression_log_final = predict(fit_regression, newdata=sales_test_reg[regression_selected_features])
  predict_regression_final = exp(predict_regression_log_final)
  sales_test_reg$targdol_predict = predict_regression_final
  
  ## generate a data frame with original and predicted value, save as sales_test_final
  sales_test_final = merge(sales_test_2 %>% select(uid, targdol), sales_test_reg %>% select(uid, targdol_predict),by='uid',all.x= TRUE)
  sales_test_final[is.na(sales_test_final)]=0
  
  ## calculate MSPE & top_1000
  calculate_MSPE <- function(actual, predicted){
    return(sum((actual-predicted) ^ 2)/(length(actual)-length(regression_selected_features)))
  }
  
  calculate_top1000 <- function(actual, predicted, by_predicted=TRUE){
    df = data.frame(actual=actual, predicted=predicted)
    if (by_predicted){
      df = df %>% arrange(desc(predicted))
    }else{
      df = df %>% arrange(desc(actual))
    }
    return(sum(df[1:1000,]$actual))
  }
  
  mspe = calculate_MSPE(sales_test_final$targdol, sales_test_final$targdol_predict)
  top1000 = calculate_top1000(sales_test_final$targdol, sales_test_final$targdol_predict)
  actual_top1000 = calculate_top1000(sales_test_final$targdol, sales_test_final$targdol_predict, by_predicted=FALSE)
  
  return(list(sales_test_df=sales_test_final,mspe=mspe, top1000=top1000, actual_top1000=actual_top1000))
}

result = model_validation(sales_test, fit_classification, optimal_p, fit_multiple, classificaiton_selected_features, regression_selected_features)
result

### regression comparasion
# base model: 
# predictors: "log_slstyr", "log_slslyr","log_sls2ago","log_sls3ago","ordtyr","ordlyr","ord2ago","ord3ago","ordhist","falord","sprord"
# R-squared: 0.05259
# Adjusted R-squared: 0.05043

# Nov 20: 
# predictors: 'slshist','ordhist_max','sales_consistency','lpuryear_new'
# R-squared: 0.07694
# Adjusted R-squared: 0.07618 


### Final measurement comparasion
# classification: base model
# regression: base model
# $mspe
# [1] 5653056159
# 
# $top1000
# [1] 47182.81
# 
# $actual_top1000
# [1] 120252.4


# classification: Nov 20
# regression: Nov 20
# $mspe
# [1] 471.1245
# 
# $top1000
# [1] 22982.15
# 
# $actual_top1000
# [1] 120252.4



##################################################################
######## Some note
# consistency: 
# people who are consistent in buying, maybe they will have higher intention to buy thing from catalog.

# average order dollor: 
# people who bought more expensive things before may related to tard_bol
# compare the higher average_order_amount, the higher prob of targ_bol

# LTD dollars
# The higher LTD dollors a consumer spend, the higher prob of targ_bol

# the more fall order compare to spring order, the higher prob of targ_bol
