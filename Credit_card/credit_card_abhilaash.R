---
  title: "Machine Learning Project"
---
  
  
  **Your Name**: Venkata Abhilaash Annamreddi
**Your G Number**: G01336658



```{r warning = FALSE, message = FALSE}
# Suppress dplyr summarise grouping warning messages
options(dplyr.summarise.inform = FALSE)
library(tidyverse)
library(tidymodels)
credit_card_df <- readRDS(url('https://gmubusinessanalytics.netlify.app/data/credit_card_df.rds'))
```



# Data Analysis




# Question 1

Check for trend in customers to close their account based on personal 
circumstances

**Answer**:
  
  Customers who are part time employees are most likely to close their account

```{r}
plot1<-ggplot(credit_card_df2,aes(x=dependents,fill=customer_status))+
  geom_bar(position='dodge')+
  labs(title = "Dependents Vs Customer Status Bar graph",x="No. of dependents",
       y="Customer count")
plot2<-ggplot(credit_card_df2,aes(x=education,fill=customer_status))+
  geom_bar(position = 'dodge')+
  labs(title = "Education Vs Customer Status Bar graph",x="Education",
       y="Customer count")+
  theme(axis.text.x = element_text(angle = 30,vjust = 0.5, hjust=0.5))
plot3<-ggplot(credit_card_df2,aes(x=marital_status,fill=customer_status))+
  geom_bar(position = 'dodge')+
  labs(title = "Marital Status Vs Customer Status Bar graph",x="Marital Status",
       y="Customer count")
plot4<-ggplot(credit_card_df2,aes(x=employment_status,fill=customer_status))+
  geom_bar(position = 'dodge')+
  labs(title = "Employment Status Vs Customer Status Bar graph",x="Employment Status",
       y="Customer count")+
  theme(axis.text.x = element_text(angle = 30,vjust = 0.5, hjust=0.5))

library(gridExtra)
grid.arrange(plot1, plot2,ncol=2)
grid.arrange(plot3,plot4,ncol=2)
#Checking numbers for closed account based on employment status
credit_card_df2%>%
  filter(customer_status=='closed_account')%>%
  group_by(employment_status)%>%
  summarize(n=n())%>%
  mutate(n_percentage=100*(n/sum(n)))
#Checking the education level of the part time employees
credit_card_df2%>%
  filter(employment_status=='part_time',customer_status=='closed_account')%>%
  group_by(education)%>%
  summarize(n=n())%>%
  mutate(n_percentage=100*(n/sum(n)))
#Checking marital status of part time employees    
credit_card_df2%>%
  filter(employment_status=='part_time',customer_status=='closed_account')%>%
  group_by(marital_status)%>%
  summarize(n=n())%>%
  mutate(n_percentage=100*(n/sum(n)))
#Checking the income range of  pat time employees who closed their account
credit_card_df2%>%
  filter(employment_status=='part_time',customer_status=='closed_account')%>%
  ggplot(.,aes(x=income))+
  geom_bar()+
  scale_x_binned()+
  labs(x='Income',y='Customer count',
       title='Bar Plot: Part-Time employed customer with closed account')
#Checking the age range of  pat time employees who closed their account
credit_card_df2%>%
  filter(employment_status=='part_time',customer_status=='closed_account')%>%
  ggplot(.,aes(x=age))+
  geom_bar()+
  scale_x_binned()+
  labs(x='Age',y='Customer count',
       title='Bar Plot: Part-Time employed customer with closed account')
#Checking the credit limit and income for part time employees who 
#closed their account
credit_card_df2%>%
  filter(employment_status=='part_time',customer_status=='closed_account')%>%
  ggplot(.,aes(x=income,y=credit_limit))+
  geom_point()+
  labs(x='Income',y='Customer count',
       title='Income Vs Credit Limit: Part-Time employed customer with closed account')
```



# Question 2

Is there trend w.r.t no. of times credit card was used and amount 
used spent to whether customers have closed their account?
  
  **Answer**:
  
  We can observe that customers who spent less (below 5000) and also used thier
credit card less frequently tend to close their account

```{r}
#Plotting for transaction count Vs spend last year
ggplot(credit_card_df,aes(x=transactions_last_year,y=total_spend_last_year,
                          color=customer_status))+
  geom_point()+
  scale_x_continuous()+
  scale_y_continuous()+
  labs(title="Transactions Vs Spend Scatter Plot",x="Transactions(Last Year)",
       y="Spend(Last Year)")
#Getting some basic stats
credit_card_df2%>%
  group_by(customer_status)%>%
  summarize(n=n(),avg_spend=mean(total_spend_last_year),
            avg_transactions=mean(transactions_last_year))%>%
  mutate(n_precentage=100*(n/sum(n)))
```


# Question 3

Are customer having a specific card type more likely to close their account?
  
  **Answer**:
  
  Customers with blue cards who have less than 5000 card limit are more likely 
to close their account

```{r}
#Plotting to check the distribution of customer status with respect to card types
ggplot(credit_card_df,aes(x=card_type,fill=customer_status))+
  geom_bar(position = 'dodge')+
  labs(title = "Card Type Vs Customer Status Bar graph",x="Card Type",
       y="Customer count")
#Plotting for card type with respect to utilization ratio
ggplot(credit_card_df,aes(x=card_type,y=utilization_ratio,fill=customer_status))+
  geom_boxplot(position = 'dodge')
#Plotting to check users for the credit limit range with highest customer 
#attrition(Blue card)
credit_card_df%>%
  filter(card_type=='blue')%>%
  group_by(customer_status)%>%
  ggplot(.,aes(x=credit_limit,fill=customer_status))+
  geom_bar(position = 'dodge')+
  scale_x_binned()+
  labs(title="Customer count Vs Credit Limit",x="Credit Limit(Binned)",
       y="Customer Count")
#Checking the numbers and percentages
credit_card_df%>%
  group_by(customer_status,card_type)%>%
  summarize(n=n())%>%
  mutate(percent_n=100*(n/sum(n)))%>%
  arrange(desc(n))
#Checking the numbers only for closed accounts
credit_card_df%>%
  filter(customer_status=='closed_account')%>%
  group_by(card_type)%>%
  summarize(n=n())%>%
  mutate(percent_n=100*(n/sum(n)))
```



# Question 4

Is there a trend/correlation with respect to spend and transaction ratio from 
Q4 to Q1?
  
  **Answer**:
  
  Customers who have spent less and have had lesser transactions over the quarter
are more likely to close their account


```{r}
ggplot(credit_card_df,aes(y=spend_ratio_q4_q1,x=transaction_ratio_q4_q1,
                          color=customer_status))+
  geom_point()+
  scale_x_continuous()+
  scale_y_continuous()+
  labs(title = "Spend Ratio Vs Transaction Ratio(Q4 to Q1)",
       x="Transaction Ratio",y="Spend Ratio")
```



# Question 5

Is there a trend in customers being inactive not being contacted by sales 
representatives leading to customer closing their accounts

**Answer**:
  
  There is no specific correlation, but there was an anomaly where customers who
were contacted 3 times by sales representative and those who were inactive for
3 months were more likely to close their account

```{r}
ggplot(credit_card_df,aes(y=months_inactive_last_year,x=customer_status,
                          fill=customer_status))+
  geom_boxplot()+
  theme(legend.position="none")
ggplot(credit_card_df,aes(y=contacted_last_year,x=customer_status,
                          fill=customer_status))+
  geom_boxplot()+
  theme(legend.position="none")
ggplot(credit_card_df,aes(y=contacted_last_year,x=months_inactive_last_year,
                          fill=customer_status))+
  geom_violin()
credit_card_df%>%
  filter(customer_status=="closed_account")%>%
  group_by(customer_status,months_inactive_last_year)%>%
  summarize(n=n())%>%
  mutate(percent_n=100*(n/sum(n)))
credit_card_df%>%
  group_by(customer_status,contacted_last_year)%>%
  summarize(n=n())%>%
  mutate(percent_n=100*(n/sum(n)))
```




# Machine Learning


# Model 1

```{r}
#Logistic model
#Calling relevant libraries
library(tidymodels)
library(vip)
#Splitting the dataset
set.seed(314)
df_split<-initial_split(credit_card_df, prop = 0.75,
                        strata = customer_status)
#Training set
df_training<-df_split %>% training()
#Testing set
df_test <- df_split %>% testing()
#Creating a recipe 
df_recipe <- recipe(customer_status ~ ., data = df_training) %>% 
  step_YeoJohnson(all_numeric(), -all_outcomes()) %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), -all_outcomes())
#Checking the data
df_recipe %>% 
  prep(training = df_training) %>% 
  bake(new_data = NULL)
#Specifying the logistic model
logistic_model <- logistic_reg() %>% 
  set_engine('glm') %>% 
  set_mode('classification')
#Creating the logistic model workflow
logistic_wf<-workflow() %>% 
  add_model(logistic_model) %>% 
  add_recipe(df_recipe)
#Fitting the model
logistic_fit <- logistic_wf %>% 
  fit(data = df_training)
#Extracting the model
logistic_trained_model <- logistic_fit %>% 
  extract_fit_parsnip()
#Variable importance plot
vip(logistic_trained_model)
#Creating custom metrics
my_metrics <- metric_set(accuracy, sens,f_meas, roc_auc)
#Using the last fit function here to get the testing results of the model
last_fit_logistic <- logistic_wf %>% 
  last_fit(split = df_split,
           metrics = my_metrics)
#Checking the result from 
last_fit_logistic %>% 
  collect_metrics()
#Collecting the predictions
last_fit_logistic <- last_fit_logistic %>% 
  collect_predictions()
#Checking the result with predictions
last_fit_logistic
#Plotting ROC curve
last_fit_logistic %>% 
  roc_curve(truth = customer_status, estimate = .pred_closed_account) %>% 
  autoplot()
```





# Model 2

```{r}
#Decision Tree model
#Calling relevant libraries
library(rpart.plot)
#Setting seed and creating folds for cross validation
set.seed(314)
df_folds <- vfold_cv(df_training, v = 5,strata=customer_status)
#Specifying decision tree model
tree_model <- decision_tree(cost_complexity = tune(),
                            tree_depth = tune(),
                            min_n = tune()) %>% 
  set_engine('rpart') %>% 
  set_mode('classification')
#Creating the decision tree model workflow
tree_workflow <- workflow() %>% 
  add_model(tree_model) %>% 
  add_recipe(df_recipe)
#Setting seed and performing hyperparameter tuning using random grid search
set.seed(314)
tree_grid <- grid_random(parameters(tree_model),
                         size = 6)
tree_grid
#Setting seed and tuning the hyperparameteres 
set.seed(314)
tree_tuning <- tree_workflow %>% 
  tune_grid(resamples = df_folds,
            grid = tree_grid)
#Showing the top 5 best models roc_auc
tree_tuning %>% show_best('roc_auc')
# Select best model based on roc_auc
best_tree <- tree_tuning %>% 
  select_best(metric = 'roc_auc')
# View the best tree parameters
best_tree
#Finalizing the workflow
final_tree_workflow <- tree_workflow %>% 
  finalize_workflow(best_tree)
#Fitting the model
tree_wf_fit <- final_tree_workflow %>% 
  fit(data = df_training)
#Extracting from the model
tree_fit <- tree_wf_fit %>% 
  extract_fit_parsnip()
#Variable importance plot
vip(tree_fit)
#Decision tree plot
rpart.plot(tree_fit$fit, roundint = FALSE, extra = 2)
#Using the last fit function here to get the testing results of the model
tree_last_fit <- final_tree_workflow %>% 
  last_fit(df_split,metrics = my_metrics)
#Collecting the metrics
tree_last_fit %>% collect_metrics()
#Plotting ROC curve
tree_last_fit %>% collect_predictions() %>% 
  roc_curve(truth  = customer_status, estimate = .pred_closed_account) %>% 
  autoplot()
```





# Model 3

```{r}
#Random Forest model
#Calling relevant library
library(ranger)
#Specifying Random forest model
rf_model <- rand_forest(mtry = tune(),
                        trees = tune(),
                        min_n = tune()) %>% 
  set_engine('ranger', importance = "impurity") %>% 
  set_mode('classification')
#Creating the decision tree model workflow
rf_workflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(df_recipe)
#Setting seed and performing hyperparameter tuning using random grid search
set.seed(314)
rf_grid <- grid_random(mtry() %>% range_set(c(2, 17)),
                       trees(),
                       min_n(),
                       size = 6)
rf_grid
#Setting seed and tuning the hyperparameteres 
set.seed(314)
rf_tuning <- rf_workflow %>% 
  tune_grid(resamples = df_folds,
            grid = rf_grid)
# Show the top 5 best models based on roc_auc metric
rf_tuning %>% show_best('roc_auc')
# Select best model based on roc_auc
best_rf <- rf_tuning %>% 
  select_best(metric = 'roc_auc')
# View the best parameters
best_rf
#Finalizing workflow
final_rf_workflow <- rf_workflow %>% 
  finalize_workflow(best_rf)
#Fitting the model
rf_wf_fit <- final_rf_workflow %>% 
  fit(data = df_training)
#Extracting form the model
rf_fit <- rf_wf_fit %>% 
  extract_fit_parsnip()
#Variable importance plot
vip(rf_fit)
#Training and evaluating the model with last fit function
rf_last_fit <- final_rf_workflow %>% 
  last_fit(df_split,metrics = my_metrics)
#Collecting metrics
rf_last_fit %>% collect_metrics()
#Plotting ROC curve
rf_last_fit %>% collect_predictions() %>% 
  roc_curve(truth  = customer_status, estimate = .pred_closed_account) %>% 
  autoplot()
```




# Summary of Results

Introduction:
  
  Banks must boost the number of consumers with credit lines in order to retain revenues. Customers carrying huge credit card amounts from month to month is also in their best interests to maximize income from interest charges.

The goal of this analysis is to understand what factors are most likely leading to customers closing their account. Some of the questions that were tried to answer in this analysis are:
  
  •	Any trend where customers based on their personal circumstances? 
  •	Is there a correlation between total amount spent and number of transactions by customers to customer closing their account?
  •	Are customers using specific card more likely to close their account?
  •	Are customers who are inactive for longer duration more likely to close their account?
  
  Exploratory data analysis:
  
  Some of the key highlights found when exploring and analyzing the data are as follows:
  
  •	Most customers who had part-time job closed their account (48.5% closed their account).  The trend among these customers were that most of them had income below 50,000 and had a credit limit below 10,000.
•	Customers overall who don’t spend much and don’t often use their credit card are much more likely to close their account. It’s been observed that customers who spend on average less than 5000 and have had less than 50 transactions in a year constituted 33% of the overall closed accounts.
•	Customers having blue card have the highest account closing rate at 71.6%.

Classification model performance analysis:
  
  The primary purpose of building a classification model was to understand the key predictors for accounts being closed and to be able to accurately predict if future customers are likely to close their account.

The following three models were built to draw some conclusion and predict whether customers would close their accounts:
  •	Logistic Regression (Sensitivity:84.9%)
•	Decision Tree (Sensitivity: 92.2%)
•	Random Forest (Sensitivity: 96.2%)

Sensitivity was considered the key metric to choose the best model. Hence, the Random Forest model was chosen as the best model to predict whether a customer will close their account having a sensitivity of 96%. Which means that out of every 100 people the model predicts whether they will close their account, it correctly predicts for 96 customers.

Also, among all the three models built, all of them showed that ‘total_spend_last_year’ and ‘transactions_last_year’ i.e., the amount being spent (using a credit card) and the number of times customers used their credit card are the most important predictors to whether a customer closes their account.

Recommendations:
  
  •	Loyalty program:
  Create a loyalty program portfolio strategically targeting customers who spend less or don’t spend often. Offer better benefits or limited time benefits/loyalty points to these targeted customers to incentivize them to use their credit card.

•	Customer service:
  Have a more proactive approach in contacting customers who have been inactive for quite a few months or those who don’t spend often or spend much and offer them services/limited time offers.

•	Additional offers/benefits for specific categories:
  We see that most of the closed accounts belonged to people having part-time jobs and are married. They generally lie in the age range of 40s. Hence additional offers such as discounts/cashbacks on shopping, travel and so forth, will greatly help in customer retention.

•	Customer feedback survey:
  Send out survey form to customers to get a better understanding of customer requirements and have a better understanding of their pain points and set action items to work on.


