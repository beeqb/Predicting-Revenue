data_manipulate <- function(sales_train){
  # transform date related column into date format
  sales_train$datelp6 = as.Date(sales_train$datelp6, "%m/%d/%Y")
  sales_train$datead6 = as.Date(sales_train$datead6, "%m/%d/%Y")
  
  # take log transformation of targdol
  sales_train$log_targdol = log(sales_train$targdol+1)
  
  # # take log transformation of sales related columns
  sales_train$log_slstyr = log(sales_train$slstyr+1)
  sales_train$log_slslyr = log(sales_train$slslyr+1)
  sales_train$log_sls2ago = log(sales_train$sls2ago+1)
  sales_train$log_sls3ago = log(sales_train$sls3ago+1)
  sales_train$log_slshist = log(sales_train$slshist+1)
  
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
  sales_train$ordhist_new = NULL
  sales_train$ordhist_max_sqrt = sqrt(sales_train$ordhist_max)
  
  # A variable calculate the consistency of a consumers
  subset_3yrsales = sales_train[,c("slstyr" ,"slslyr" , "sls2ago", "sls3ago")]
  RowVar <- function(x) {
    rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
  }
  salesvar = RowVar(subset_3yrsales)
  salesmean = rowMeans(subset_3yrsales)
  salescv = ifelse(salesmean!=0, salesvar/salesmean, 5000)
  sales_train$sales_consistency = salescv
  sales_train$salesvar = salesvar
  sales_train$salesmean = salesmean
  
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
                                                                                         ifelse(sales_train$lpuryear == 3, 2003, NA))))))))))
  
  
  
  sales_train$lpuryear[is.na(sales_train$lpuryear)] <- 2002
  # Use the most recent year, from datelp6 & lpuryear
  sales_train$lpuryear_new <- ifelse(sales_train$datelp6 >= sales_train$lpuryear, sales_train$datelp6, sales_train$lpuryear)
  sales_train$lpuryear_new <- 2012 - sales_train$lpuryear_new
  
  # calculate consistency using sales history
  sale_consistency = sales_train %>% dplyr::select(slstyr, slslyr, sls2ago, sls3ago)
  sale_consistency[sale_consistency>0] = 1
  sales_train$sls_consistency = rowSums(sale_consistency)
  
  # calculate the percetage of sales within three years to total sales
  sale_within = sales_train %>% dplyr::select(slstyr, slslyr, sls2ago, sls3ago, slshist)
  sale_within$slshist[sale_within$slshist==0] = 1 
  sale_within = sale_within %>% mutate(sale_within_three = slstyr + slslyr + sls2ago + sls3ago) %>% mutate(sale_within_percent=sale_within_three/slshist)
  sales_train$sale_within_percent = sale_within$sale_within_percent
  sales_train$sale_within_three = log(sale_within$sale_within_three+1)
  
  # LOG: calculate the percetage of sales within three years to total sales
  sale_within_log = sales_train %>% dplyr::select(log_slstyr, log_slslyr, log_sls2ago, log_sls3ago, log_slshist)
  sale_within_log$log_slshist[sale_within_log$slshist==0] = 1 
  sale_within_log = sale_within_log %>% mutate(sale_within_three = log_slstyr + log_slslyr + log_sls2ago + log_sls3ago) %>% mutate(sale_within_percent_log=sale_within_three/log_slshist)
  sales_train$sale_within_percent_log = sale_within_log$sale_within_percent_log
  
  # average total order value 
  sales_train = sales_train %>% dplyr::mutate(avg_order_sale = slshist/ordhist_max,
                                       log_avg_order_sale = log_slshist/ordhist_max)
  sales_train$log_avg_order_sale[is.na(sales_train$log_avg_order_sale)] = 0
  sales_train$log_avg_order_sale[is.na(sales_train$log_avg_order_sale)] =0
  
  # maximum order witnin fall or spring, categorical
  max_season = sales_train %>% dplyr::select(falord, sprord)
  sales_train$max_season = colnames(max_season)[apply(max_season,1,which.max)]
  
  # generate interaction terms
  interaction_sales = sales_train %>% dplyr::select(log_slslyr, log_sls2ago, log_sls3ago)
  interaction_sales = model.matrix( ~.^3, data=interaction_sales)
  interaction_sales = interaction_sales[,-c(1:4)]
  sales_train = cbind(sales_train, interaction_sales)
  
  # sales trend
  subset_3yrsales = sales_train[,c("slstyr" ,"slslyr" , "sls2ago", "sls3ago")]
  RowVar <- function(x) {
    rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
  }
  subset_3yrsales = subset_3yrsales[,c(4,3,2,1)]
  alldiff = t(apply(subset_3yrsales,1,diff))
  salesign = rowSums(t(apply(subset_3yrsales,1,sign)))
  diffsign = t(apply(alldiff,1,sign))
  diffproduct = diffsign[,1]*diffsign[,2]
  diffeffect = ifelse(diffproduct==1,1,ifelse(diffproduct==0,0,0.5))
  trend = salesign + diffeffect
  sales_train$trend = trend
  
  # order trend
  ords = sales_train[,c("ordtyr" ,"ordlyr" , "ord2ago", "ord3ago")]
  ords = ords[,c(4,3,2,1)]
  ordalldiff = t(apply(ords,1,diff))
  ordsign = rowSums(t(apply(ords,1,sign)))
  orddiffsign = t(apply(ordalldiff,1,sign))
  orddiffproduct = orddiffsign[,1]*orddiffsign[,2]
  orddiffeffect = ifelse(orddiffproduct==1,1,ifelse(orddiffproduct==0,0,0.5))
  ordtrend = ordsign + orddiffeffect
  sales_train$ordtrend = ordtrend
  
  # consistency
  subset_3yrsales = sales_train[,c("log_slstyr" ,"log_slslyr" , "log_sls2ago", "log_sls3ago")]
  RowVar <- function(x) {
    rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
  }
  alldiff = t(apply(subset_3yrsales,1,diff))
  finaldiff = rowSums(alldiff)
  
  salesvar = RowVar(subset_3yrsales)
  salesmean = rowMeans(subset_3yrsales)
  salescv = salesvar/salesmean
  meancv = mean(salescv,na.rm=TRUE)
  salescv = ifelse(salesmean!=0, salesvar/salesmean,meancv)
  subset_3yrsales$cv = salescv
  subset_3yrsales$diff= -finaldiff
  sales_train$cv = salescv
  sales_train$diff = finaldiff
  return(sales_train)
}

get_optimal_p <- function(real_response, predict_fit, p_threshold_list){
  # get the optimal probability threshold according to CCR / f1_score
  # @param real_response: actual response vector
  # @param predict_fit: predicted probabilty from the model
  # @param p_threshold_list: list of probability that we want to test on
  # @return: optimal probability threshold according to CCR
  
  max_metric= 0
  optimal_p = 0
  
  
  for (p in p_threshold_list){
    pred = rep(0, length(real_response))
    pred[predict_fit > p]=1
    ccr = sum(diag(table(real=real_response, pred)))/ length(real_response)
    # confusion_table = table(real=real_response, pred)
    # 
    # sensitivity=confusion_table[2,2]/(confusion_table[2,1]+confusion_table[2,2])
    # specificity=confusion_table[1,1]/(confusion_table[1,1]+confusion_table[1,2])
    # precision=confusion_table[2,2]/(confusion_table[1,2]+confusion_table[2,2]) 
    # f1_score=2*precision*sensitivity/(precision+sensitivity)
    # print(paste("f1_score:",f1_score," when p=",p))
    
    if (ccr >= max_metric){
      max_metric= ccr
      optimal_p = p
    }
  }
  
  return(optimal_p)
}

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
  
  plot.roc(real_response, predict_fit, xlab="1-Specificity")
  
  my_auc = auc(real_response, predict_fit)
  
  return(list(auc=my_auc,
              ccr=ccr,
              sensitivity=sensitivity, 
              specificity=specificity,
              f1_score=f1_score))
}

model_validation <- function(sales_test, fit_classification, optimal_p, fit_regression, classificaiton_selected_features, regression_selected_features){
  # a function that calculate all the classification related metrics, including confusion table, auc, ccr, ...etc.
  # @param sales_test: test data 
  # @param fit_classification: model for classification
  # @param optimal_p: optimal probability threshold to decide the data that we should keep for regression
  # @param fit_regression: model for regression
  # @param classificaiton_selected_features: predictors for classificaiton
  # @param regression_selected_features: predictors for regression
  # @return: a dataframe with the actual sales and final predicted sales, mspe, top100, actual_top1000
  
  
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
  sales_test_final = merge(sales_test_2 %>% dplyr::select(uid, targdol), sales_test_reg %>% dplyr::select(uid, targdol_predict),by='uid',all.x= TRUE)
  sales_test_final[is.na(sales_test_final)]=0
  
  ## calculate MSPE & top_1000
  calculate_MSPE <- function(actual, predicted){
    return(sum((actual-predicted) ^ 2)/
             (length(actual)-length(fit_regression$coefficients)-1)
           )
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

my_cv_glmnet <- function(y, x, alpha){
  # set seed for cross validation
  set.seed(1)
  
  # using cv.glmnet to build lasso. The following line calculate 3 fold cv for each lambda, so there will be 1000*3 model fitting.
  fit.cv=cv.glmnet(x,y,alpha=alpha,nfold=3,lambda=seq(0,10,0.01))
  
  # get the lambda with the smallest Mean-Squared Error
  fitted_min_lambda=fit.cv$lambda.min
  
  # get the index of the smallest lambda, and use it to find our ideal coefficient
  small.lambda.index <- which(fit.cv$lambda == fit.cv$lambda.min)
  small.lambda.betas <- coef(fit.cv$glmnet.fit)[,small.lambda.index]
  
  return(list(lambda=fitted_min_lambda,
              small.lambda.betas=small.lambda.betas))
}
