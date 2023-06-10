#Libraries to be included 
library(readxl) #To import Excel File
library(fpp2) #To Plot Time Series Data
library(stlplus) #Time Series Decomposition
library(tidyverse) #To convert Excel to Time Series 
library(tseries) #Time Series Functions

#Importing Data Set
temperature.data <- read_excel("/Users/Lumiere/Downloads/Raw Month Average Temperature in the Philippines.xlsx")

#Converting data set into Time Series
temperature.data.ts <- temperature.data %>%
mutate (Month = 1:n()) %>%
gather (Year, value, -Month) %>%
arrange (Year, Month) %>%  
  {ts(.$value, start = c(2000, 1), frequency = 12)}  

#Plotting the Time Series
autoplot(temperature.data.ts)

#Decompose the Time Series Data to see if the data exhibits trend and seasonality
temperature.data.stl <- stlplus(temperature.data.ts, s.window = "period")

#Plotting the data to see if the data exhibits trend and seasonality
plot(temperature.data.stl)

#Utilize Augmented Dickey-Fuller (ADF) test to support non-stationary behavior of the time series
#If p-value>0.05 significance level, then the time series data exhibits non-stationary behavior
adf.test(temperature.data.ts, k=12)

#The time series has a p-value of 0.2346>0.05, thus it is non-stationary
#Perform first level differencing to approach Stationarity
temperature.data.ts.d1 <- diff(temperature.data.ts, differences = 1)

#Perform ADF Test on first level difference
adf.test(temperature.data.ts.d1, k=12)

#First level difference has p-value of 0.01<0.05. Thus, stationary is achieved
#Plot first level differences to verify stationary behavior
plot(temperature.data.ts.d1)

#Following first level difference utilized, d-value in ARIMA=1
#Choose p-value for ARIMA using PACF plot
pacf(temperature.data.ts.d1)

#We find from PACF plot that p=3 has high significance.
#Choose q-value for ARIMA using ACF plot
acf(temperature.data.ts.d1)

#We find from ACF plot that q=4 has high significance. 
#Thus, we consider ARIMA (3,1,4) in forecasting data.
Model <- Arima(y=temperature.data.ts, order = c(3,1,4))
print(Model)

#ARIMA (3,1,4) provides AICc=353.67
#Thus, we should consider other models. We need to perform auto ARIMA
(fit_autoarima <- auto.arima(temperature.data.ts, seasonal=FALSE))

#Auto ARIMA function considers ARIMA(2,1,1) which provides AICc=401.25
#We should consider autoarima with parameter approximation=FALSE and stepwise=FALSE
(fit_autoarima_noapprox <- auto.arima(temperature.data.ts, seasonal=FALSE, stepwise=FALSE, approximation=FALSE))

#Latest Auto ARIMA function considers ARIMA(2,1,3) which provides AICc=388.61
#Based in all ARIMA models considered, we find ARIMA(3,1,4) to provide the lowest AICc

#Compare residuals of all models
checkresiduals(Model)
checkresiduals(fit_autoarima)
checkresiduals(fit_autoarima_noapprox)

#Perform Model Training
#Set test data as the past five years
training.set <- subset(temperature.data.ts, end=length(temperature.data.ts)-61)
test.set <- subset(temperature.data.ts, start = length(temperature.data.ts)-60)

#Illustrate ARIMA (3,1,4) on training data
Model.train <- Arima(training.set, order = c(3,1,4), lambda=0)
Model.train %>%
  forecast(h=60) %>%
  autoplot()+autolayer(test.set)

#Plot 12-step fitted values to training data
autoplot(training.set, series="Training data") + 
  autolayer(fitted(Model.train, h=12),
            series="12-step fitted values")

#Apply same ARIMA model to test data
Model.test <- Arima(test.set, model=Model.train)

#Check results for accuracy
accuracy(Model.test)

#Forecast next 5 years of time series with ARIMA (0,1,4)
Five.year.forecast <- forecast(Model, h=60)
Five.year.forecast

#Plot Forecast
autoplot(Five.year.forecast) +
  ggtitle("Five-year Forecast for the Monthly Average Temperature in the Philippines using ARIMA (3,1,4)") +
  xlab("Year") +
  ylab("Temperature (in degrees)")

