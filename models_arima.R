{
  library(caTools)
  library(lubridate)
  library(openxlsx)
  library(ggplot2)
  library(dplyr)
  library(forecast)
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


# manually differencing the time series
plot.ts(diff(dataset$tons, differences=1), main='stationary time series')
acf(diff(dataset$tons, differences=1), main='correlogram')
pacf(diff(dataset$tons, differences=1), main='partial correlogram')


#training and testing model performance 
regressor <- auto.arima(ts(dataset[1:19,14]), xreg = as.matrix(select(dataset,c('gdp_iran','gdp_thailand'))[1:19,]))
plot(forecast(regressor,xreg = as.matrix(select(dataset,c('gdp_iran','gdp_thailand'))[20:23,])), main = 'corn model performance')
lines(ts(dataset$tons))

#training the final model
regressor <- auto.arima(ts(dataset$tons), xreg = as.matrix(select(dataset,c('gdp_iran','gdp_thailand'))))

# creating a different dataset for the years to be predicted
dataset_pred <- left_join(covariates, corn_total_exported, by=c('year'='year'))
dataset_pred <- dataset_pred %>% 
  filter(year>2019)

# deploying the model
prediction <- forecast(regressor, xreg = as.matrix(select(dataset_pred,c('gdp_iran','gdp_thailand'))))


corn_pred <- setNames(data.frame(prediction$mean),nm = c('corn'))



# plotting the model
plot(prediction, 
     main = 'Braziian Corn Export from 1997 to 2030', xlab = 'Year', ylab="Tons", xaxt='n')
axis(1, at = 1:34, labels = 1997:2030)
grid()



######### SOYBEAN_MEAL

# total amount of soybean_meal per year
soybean_meal_total_exported <- trade_br %>% 
  mutate(year = as.numeric(format(date, "%Y"))) %>% 
  filter(product=='soybean_meal') %>% 
  group_by(year) %>% 
  summarise(tons=sum(tons))

#joining covariates and amount of soybean_meal per year. also, keeping data from 1997 to 2019
dataset <- left_join(covariates,soybean_meal_total_exported, by=c('year'='year')) %>% 
  filter(between(year, 1997, 2019))


# manually differencing the time series
plot.ts(diff(dataset$tons, differences=1), main='stationary time series')
acf(diff(dataset$tons, differences=1), main='correlogram')
pacf(diff(dataset$tons, differences=1), main='partial correlogram')

#training and testing model performance 
regressor <- auto.arima(ts(dataset[1:19,14]), xreg = as.matrix(select(dataset,c('gdp_china', 'gdp_vietnam', 'gdp_thailand'))[1:19,]))
plot(forecast(regressor,xreg = as.matrix(select(dataset,c('gdp_china', 'gdp_vietnam', 'gdp_thailand'))[20:23,])), main = 'soybean meal model performance')
lines(ts(dataset$tons))

#training the final model
regressor <- auto.arima(ts(dataset$tons), xreg = as.matrix(select(dataset,c('gdp_china', 'gdp_vietnam', 'gdp_thailand'))))

# creating a different dataset for the years to be predicted
dataset_pred <- left_join(covariates, soybean_meal_total_exported, by=c('year'='year'))
dataset_pred <- dataset_pred %>% 
  filter(year>2019)

# deploying the model
prediction <- forecast(regressor, xreg = as.matrix(select(dataset_pred,c('gdp_china', 'gdp_vietnam', 'gdp_thailand'))))

soybeanmeal_pred <- setNames(data.frame(prediction$mean),nm = c('soybean meal'))

# plotting the model
plot(prediction, 
     main = 'Braziian Soybean Meal Export from 1997 to 2030', xlab = 'Year', ylab="Tons", xaxt='n')
axis(1, at = 1:34, labels = 1997:2030)
grid()



######### soybeans

# total amount of soybeans per year
soybeans_total_exported <- trade_br %>% 
  mutate(year = as.numeric(format(date, "%Y"))) %>% 
  filter(product=='soybeans') %>% 
  group_by(year) %>% 
  summarise(tons=sum(tons))

#joining covariates and amount of soybeans per year. also, keeping data from 1997 to 2019
dataset <- left_join(covariates,soybeans_total_exported, by=c('year'='year')) %>% 
  filter(between(year, 1997, 2019))


# manually differencing the time series
plot.ts(diff(dataset$tons, differences=1), main='stationary time series')
acf(diff(dataset$tons, differences=1), main='correlogram')
pacf(diff(dataset$tons, differences=1), main='partial correlogram')


#training and testing model performance 
regressor <- auto.arima(ts(dataset[1:19,14]), xreg = as.matrix(select(dataset,c('price_soybeans', 'gdp_vietnam'))[1:19,]))
plot(forecast(regressor,xreg = as.matrix(select(dataset,c('price_soybeans', 'gdp_vietnam'))[20:23,])), main = 'soybeans model performance')
lines(ts(dataset$tons))

#training the final model
regressor <- auto.arima(ts(dataset$tons), xreg = as.matrix(select(dataset,c('price_soybeans', 'gdp_vietnam'))))

# creating a different dataset for the years to be predicted
dataset_pred <- left_join(covariates, soybeans_total_exported, by=c('year'='year'))
dataset_pred <- dataset_pred %>% 
  filter(year>2019)

# deploying the model
prediction <- forecast(regressor, xreg = as.matrix(select(dataset_pred,c('price_soybeans', 'gdp_vietnam'))))


soybeans_pred <- setNames(data.frame(prediction$mean),nm = c('Soybeans'))

# plotting the model
plot(prediction, 
     main = 'Braziian Soybean Export from 1997 to 2030', xlab = 'Year', ylab="Tons", xaxt='n')
axis(1, at = 1:34, labels = 1997:2030)
grid()






