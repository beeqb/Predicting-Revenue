source('revenue_prediction_function.R')
library(dplyr)
library(ggplot2)
library(pROC)
library(MASS)
library(glmnet)


##### Read Data
sales_df = read.csv('../_data/catalog sales data.csv')
sales_df$uid = row.names(sales_df)
sales_train = sales_df %>% filter(train==1)
sales_test = sales_df %>% filter(train==0)

##### Linearize and Normalize Transformations
sales_train_2 = data_manipulate(sales_train)




########## Classification
##### Create Possible Features
classificaiton_selected_features = c('log_slshist','ordhist_max_sqrt','sales_consistency',
                                     'sls_consistency','lpuryear_new','sale_within_percent',
                                     'avg_order_sale','max_season','targdol_bol')

# #### Try undersampling
# sales_train_2_yes = sales_train_2 %>% filter(targdol_bol==1)
# sales_train_2_no = sales_train_2 %>% filter(targdol_bol==0) %>% sample_n(dim(sales_train_2_yes)[1])
# sales_train_undersample = rbind(sales_train_2_yes, sales_train_2_no)


##### Model Fitting
fit_classification = glm(targdol_bol ~ ., family=binomial, data=sales_train_2[classificaiton_selected_features])
summary(fit_classification)



predict_fit = predict(fit_classification, newdata=sales_train_2[classificaiton_selected_features], type="response")

real_response = sales_train_2$targdol_bol

optimal_p = get_optimal_p(real_response, predict_fit, seq(0.3,0.8,0.01))

calculate_metrics(real_response, predict_fit, optimal_p)

##### Compare between all fitted classification models 
if(FALSE){
  ### Version 3: 
  # fit model using many more features including sale_within_percent, avg_order_sale, max_season, log_slshist, ordhist_max_sqrt, sls_consistency
  # predictors: 
  # c('log_slshist','ordhist_max_sqrt','sales_consistency',
  #   'sls_consistency','lpuryear_new','sale_within_percent',
  #   'avg_order_sale','max_season','targdol_bol')
  #  prediction
  # actual     0     1
  # 0 45265   299
  # 1  4406   439
  # $auc
  # [1] 0.7920445
  # 
  # $ccr
  # [1] 0.9066635
  # 
  # $sensitivity
  # [1] 0.09060888
  # 
  # $specificity
  # [1] 0.9934378
  # 
  # $f1_score
  # [1] 0.1572631
  
  ### Version 2: 
  # fit model using only predictors of interests
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
  
  ### base model: 
  # fit logistic using all the predictors
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
}





########## Regression
# For regression, we will only use the data with targdol > 0 as our training data
sales_train_reg = sales_train_2 %>% filter(targdol != 0 )


##### Model Fitting
### base model
# regression_selected_features = c("log_slstyr", "log_slslyr","log_sls2ago","log_sls3ago","ordtyr","ordlyr","ord2ago","ord3ago","ordhist","falord","sprord","log_targdol")

### version 2
# regression_selected_features = c('slshist','ordhist_max','sales_consistency','lpuryear_new',"log_targdol")
# regression_selected_features = c('slshist','ordhist_max','sales_consistency',"log_targdol")

### version 3
regression_selected_features = c('log_slshist','ordhist_max_sqrt','sales_consistency',
                                  'sls_consistency','lpuryear_new','sale_within_percent',
                                  'avg_order_sale','max_season',"log_targdol")
regression_selected_features = c('log_slshist','sales_consistency','sls_consistency',
                                 'sale_within_percent','avg_order_sale','log_targdol')


##### > Multiple Linear Regression
fit_multiple = lm(log_targdol~.,data=sales_train_reg[regression_selected_features])
summary(fit_multiple)

##### > Stepwise Linear Regression
# **Backward**
fit_stepback <- stepAIC(fit_multiple,direction = c("backward"))
summary(fit_stepback)

# **Forward**
fit_stepforw <- stepAIC(fit_multiple,direction = c("forward"))
summary(fit_stepforw)

##### > Lasso Linear Regression
# set model x and y
y=sales_train_reg$log_targdol
x=model.matrix(log_targdol~.,sales_train_reg[regression_selected_features])
my_cv_glmnet(y,x,1)

# predict using glmnet fitted model. Should be integrate into model_validation
# lassofit = glmnet(x, y, alpha=1, lambda=lambdalasso)
# x_test=model.matrix(log_targdol~.,sales_test_reg[regression_selected_features])
# predict(lassofit, newx = x_test)


# ##### > Ridge Linear Regression
my_cv_glmnet(y,x,0)

##### Compare between all fitted regression models
if(FALSE){
  ### Verson 3: 
  # fit model using many more features including sale_within_percent, avg_order_sale, max_season, log_slshist, ordhist_max_sqrt, sls_consistency
  # predictors:
  # c('log_slshist','ordhist_max_sqrt','sales_consistency',
  #                                     'sls_consistency','lpuryear_new','sale_within_percent',
  #                                     'avg_order_sale','max_season',"log_targdol")
  # Multiple R-squared:  0.09608,	Adjusted R-squared:  0.09458
  
  # keep only significant predictors:
  # c('log_slshist','sales_consistency','sls_consistency',
  #   'sale_within_percent','avg_order_sale','log_targdol')
  # Multiple R-squared:  0.09597,	Adjusted R-squared:  0.09504 
  
  
  ### Verson 2: 
  # fit model using only predictors of interests
  # predictors: 'slshist','ordhist_max','sales_consistency','lpuryear_new'
  # Multiple R-squared: 0.07694,	Adjusted R-squared:  0.07618 
  
  # keep only significant predictors:
  # predictors: 'slshist','ordhist_max','sales_consistency'
  # Multiple R-squared: 0.07664,	Adjusted R-squared: 0.07606 
  
  
  ### base model:
  # fit regression using all the predictors
  # predictors: "log_slstyr", "log_slslyr","log_sls2ago","log_sls3ago","ordtyr","ordlyr","ord2ago","ord3ago","ordhist","falord","sprord"
  # Multiple R-squared: 0.05259,	Adjusted R-squared: 0.05043
}



########## Final Evaluation
result = model_validation(sales_test, fit_classification, optimal_p, fit_multiple, classificaiton_selected_features, regression_selected_features)
result


##### Final measurement comparasion
if(FALSE){
  ### Version 3
  # classification: Nov 20 
  # regression: Nov 20 after deleting lpuryear_new
  # $mspe
  # [1] 444.5706
  # 
  # $top1000
  # [1] 27638.48
  # 
  # $actual_top1000
  # [1] 120252.4
  
  
  ### Version 2
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
  
  # classification: Nov 20
  # regression: Nov 20 after deleting lpuryear_new
  # $mspe
  # [1] 470.615
  # 
  # $top1000
  # [1] 23061
  # 
  # $actual_top1000
  # [1] 120252.4
  
  
  ### Base model
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
  

  

}


########################################
######## Some note
if(FALSE){
  # consistency: 
  # people who are consistent in buying, maybe they will have higher intention to buy thing from catalog.
  
  # average order dollor: 
  # people who bought more expensive things before may related to tard_bol
  # compare the higher average_order_amount, the higher prob of targ_bol
  
  # LTD dollars
  # The higher LTD dollors a consumer spend, the higher prob of targ_bol
  
  # the more fall order compare to spring order, the higher prob of targ_bol
}

