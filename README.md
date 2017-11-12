# Predicting-Revenue
A project from the MSiA course, *Predictive Analytics I*, that predicting future revenue for a retail company based on  customers' transaction data.


### Business Situation: 
The file catalog sales data.csv posted on Canvas under Data Sets page comes from a retail company that sells upscale clothing on its website and via catalogs, which help drive customers to the website. All customers were sent a catalog mailing on Sep 1, 2012. On Dec 1, 2012 it was recorded whether or not they responded by making a purchase. There is one row for each customer. The targdol is the response variable, which is the purchase amount in response to receiving the catalog (targdol = 0 indicates that the customer did not respond). The remainder of variables are potential predictor variables which give information about the customer as of the time of the mailing. LTD means “life-to-date,” i.e., since the customer purchased for the first time.

### Data: 
There are a total 101,532 customers, who are randomly split into 50418 in the training set and the remaining 51,114 in the test set (train =1 training set, train =0 test set). The definitions of the variables are as follows.
* **targdol**: dollar purchase resulting from catalog mailing – datead6: date added to file
* **datelp6**: date of last purchase
* **lpuryear**: latest purchase year
* **slstyr**: sales ($) this year
* **slslyr**: sales ($) last year
* **sls2ago**: sales ($) 2 years ago
* **sls3ago**: sales ($) 3 years ago
* **slshist**: LTD dollars
* **ordtyr**: orders this year
* **ordlyr**: orders last year
* **ord2ago**: orders 2 years ago
* **ord3ago**: orders 3 years ago
* **ordhist**: LTD orders
* **falord**: LTD fall orders
* **sprord**: LTD spring orders
* **train**: training/test set indicator (1 = training, 0 = test)

### Goal: 
Build a predictive model for targdol based on the training set and then test it on the test set.
