{
  library(caTools)
  library(lubridate)
  library(openxlsx)
  library(ggplot2)
  library(plyr)
  library(dplyr)
}

# loading brazilian trade data
trade_br=read.csv('data_comexstat.csv',stringsAsFactors = FALSE)
trade_br$date <- ymd(trade_br$date)

# loading covariates data
covariates <- read.xlsx('covariates.xlsx')


############ CORN
############ multiple linear regression


# total amount of corn per year
corn_total_exported <- trade_br %>% 
  mutate(year = as.numeric(format(date, "%Y"))) %>% 
  filter(product=='corn') %>% 
  group_by(year) %>% 
  summarise(tons=sum(tons))

#joining covariates and amount of corn per year. also, keeping data from 1997 to 2019
dataset <- left_join(covariates,corn_total_exported, by=c('year'='year')) %>% 
  filter(between(year, 1997, 2019))

#spliting into training and test set
split = sample.split(dataset$tons, SplitRatio = 0.9)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# training the model 
regressor = lm(formula = tons ~ .,
               data = select(training_set, -c(1,10,12,11,8,7,3,13,5,2,4)))

summary(regressor)

# applying to the test set
y_pred = predict(regressor, newdata = select(test_set,c('gdp_iran','gdp_thailand')))

# 'gdp_iran','gdp_thailand' 0.05 satisfied
#  'gdp_iran','gdp_spain','gdp_egypt','gdp_japan' highest adjusted r2


# creating a different dataset for the years to be predicted
dataset_pred <- left_join(covariates, corn_total_exported, by=c('year'='year'))
dataset_pred <- dataset_pred %>% 
  select(c('year','gdp_iran', 'gdp_iran', 'gdp_thailand', 'tons')) %>% 
  filter(year>=2019)

# predicting using the regressor built
dataset_pred[2:12,4] <- predict(regressor, newdata = dataset_pred[2:12,-4])

# plotting past and predicted values
ggplot(dataset,aes(year, tons)) +
  geom_line()+
  geom_line(data = dataset_pred,aes(year,tons), color="blue", linetype=2)+
  theme_classic()+
  geom_point(data = dataset_pred)+
  ggtitle('Braziian Corn Export from 1997 to 2030')+
  theme(plot.title = element_text(hjust = 0.5))



######### SOYBEAN_MEAL
######### multiple linear regression

# total amount of soybean_meal per year
soybean_meal_total_exported <- trade_br %>% 
  mutate(year = as.numeric(format(date, "%Y"))) %>% 
  filter(product=='soybean_meal') %>% 
  group_by(year) %>% 
  summarise(tons=sum(tons))

#joining covariates and amount of soybean_meal per year. also, keeping data from 1997 to 2019
dataset <- left_join(covariates,soybean_meal_total_exported, by=c('year'='year')) %>% 
  filter(between(year, 1997, 2019))


#spliting into training and test set
split = sample.split(dataset$tons, SplitRatio = 0.9)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# training the model 
regressor = lm(formula = tons ~ .,
               data = select(dataset, -c(1,7,2,11,6,3,4,10,8,12)))

summary(regressor)

y_pred = predict(regressor, newdata = select(test_set, -c(1,7,2,11,6,3,4,10,8,12)))

#-c(1,7,2,11,6,3,4,10,8,12))) 0.05 satisfied
# -c(1, 7, 2, 11, 6))) maior r adjus


# creating a different dataset for the years to be predicted
dataset_pred <- left_join(covariates, soybean_meal_total_exported, by=c('year'='year'))
dataset_pred <- dataset_pred %>% 
  select(c('year', 'gdp_china', 'gdp_vietnam', 'gdp_thailand', 'tons')) %>% 
  filter(year>=2019)

# predicting using the regressor built
dataset_pred[2:12,5] = predict(regressor, newdata = dataset_pred[2:12,-5])

# plotting past and predicted values
ggplot(dataset,aes(year, tons)) +
  geom_line()+
  geom_line(data = dataset_pred,aes(year,tons), color="blue", linetype=2)+
  theme_classic()+
  geom_point(data = dataset_pred)+
  ggtitle('Braziian Soybean Meal Export from 1997 to 2030')+
  theme(plot.title = element_text(hjust = 0.5))



#########  SOYBEANS
######### multiple linear regression


soybeans_total_exported <- trade_br %>% 
  mutate(year = as.numeric(format(date, "%Y"))) %>% 
  filter(product=='soybeans') %>% 
  group_by(year) %>% 
  summarise(tons=sum(tons))

#joining covariates and amount of soybeans per year. also, keeping data from 1997 to 2019
dataset <- left_join(covariates, soybeans_total_exported, by=c('year'='year')) %>% 
  filter(between(year, 1997, 2019))

#spliting into training and test set
split = sample.split(dataset$tons, SplitRatio = 0.9)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# training the model 
regressor = lm(formula = tons ~ .,
               data = select(training_set, -c(1,7,8,11,5,3,10,6,9,12,4)))

summary(regressor)

y_pred = predict(regressor, newdata = select(test_set, -c(1,7,8,11,5,3,10,6,9,12,4)))

#pricesoybeanmeal price soybean gdpvietnam best adjusted r2
# vietnam price soybean 0.05


# creating a different dataset for the years to be predicted
dataset_pred <- left_join(covariates, soybeans_total_exported, by=c('year'='year'))
dataset_pred <- dataset_pred %>% 
  select(c('year','price_soybeans', 'gdp_vietnam', 'tons')) %>% 
  filter(year>=2019)

# predicting using the regressor built
dataset_pred[2:12,4] = predict(regressor, newdata = dataset_pred[2:12,-4])

# plotting past and predicted values
ggplot(dataset,aes(year, tons)) +
  geom_line()+
  geom_line(data = dataset_pred,aes(year,tons), color="blue", linetype=2)+
  theme_classic()+
  geom_point(data = dataset_pred)+
  ggtitle('Braziian Soybean Export from 1997 to 2030')+
  theme(plot.title = element_text(hjust = 0.5))




###### Testing other models to the corn prediction. Not included in the solution

library(randomForest)


split = sample.split(dataset$tons, SplitRatio = 0.9)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

regressor = randomForest(x = training_set[,c('gdp_iran','gdp_thailand')],
                         y = training_set[,14],
                         ntree = 50)

y_pred = predict(regressor, test_set[, 2:13])
summary(regressor)
 

predict(regressor, dataset_pred[-1,2:3])

############

library(xgboost)

classifier = xgboost(data = as.matrix(training_set[,c('gdp_iran','gdp_thailand')]), label = training_set[,14], nrounds = 200)

y_pred = predict(classifier, newdata = as.matrix(test_set[,c('gdp_iran','gdp_thailand')]))


#########


