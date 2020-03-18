# Loading Libraries
if (!require("forecast")) install.packages("forecast"); library(forecast)

# Functions
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Importing Data
raw_data <- read.csv("Sample_TimeSeries.csv")



##### Prod_1 #####

t1 <- ts(raw_data[raw_data$Product == "Prod_1", "Demand"], start = c(2013, 01), freq = 12)

plot(t1) # Erratic within range of about 100 units of demand
boxplot(t1~cycle(t1)) # November to April can be considered high demand months. Seasonality unclear.
plot(decompose(t1)) # Trend within small range. Seasonality explains almost half of erraticity.

# ETS
t1.ets <- ets(t1) # Detected No Seasonality/ Trend. ETS Model Type: A,N,N
plot(forecast(t1.ets, n.ahead = 12))
rmse(t1.ets$residuals) # RMSE = 29.75381

# Custom ETS
t1.ets_2 <- ets(t1, model = "ANA")
plot(predict(t1.ets_2, n.ahead = 12))
rmse(t1.ets_2$residuals) # RMSE = 23.04582, Better than ETS Model Type: A,N,N

# Selecting Result for A,N,A ETS:
prod_1 <- forecast(t1.ets_2, h = 3)



##### Prod_2 #####

t2 <- ts(raw_data[raw_data$Product == "Prod_2", "Demand"], start = c(2013, 01), freq = 12)

plot(t2) # Trending Up - Non Stationary series
boxplot(t2~cycle(t2)) # Minor Seasonality
plot(decompose(t2)) # Obvious Trend
acf(t2) # Anticipated due to Linear Trend
pacf(t2) # No evidence of Auto-Regression
plot(decompose(diff(t2))) # First order differential does not bring about stationarity
plot(decompose(diff(diff(t2)))) # Second order differential appears stationary
acf(diff(diff(t2))) # ACF plot of second order differential suggests MA coefficient of 1
pacf(diff(diff(t2))) # PACF plot of second order differential suggests AR coefficient of 3

# Corresponding ARIMA model
t2.arima <- arima(t2, order = c(3,2,1)) # AIC = 413.98

# Best practise to try nearby values of AR and MA coefficient
t2.arima_test <- arima(t2, order = c(2,2,1)) # AIC = 415.49
t2.arima_test <- arima(t2, order = c(4,2,1)) # AIC = 415.92
t2.arima_test <- arima(t2, order = c(3,2,0)) # AIC = 424.65
t2.arima_test <- arima(t2, order = c(3,2,2)) # AIC = 416.05

# Auto Arima Function from forecast package suggests ARIMA(0,1,2)
t2.aa <- auto.arima(t2, ic = "aic") # AIC = 415.39
plot(forecast(t2.aa, h = 12))

# Lowest AIC achieved with ARIMA(3,2,1) model
t2.predict <- predict(t2.arima, n.ahead = 12); ts.plot(t2, t2.predict$pred, lty=1:2) # Captures Trend. Fails to capture Seasonality.
plot(t2.arima$residuals) # Appear to be white noise
rmse(t2.arima$residuals) # RMSE = 80.75743

# Applying Holt Winters Forecast
t2.hw <- HoltWinters(t2); plot(t2.hw)
# Best fit is found with parameters alpha = 0.16, beta = 0.08, gamma = 0.88
# Small alpha justified as trend is linear throughout. Small beta justified due to zig-zag nature. 
# Oversensitive to Seasonality. Will emulate previous years seasonality almost completely.
ts.plot(t2, predict(t2.hw, n.ahead = 12), lty=1:2)
(t2.hw$SSE/36)^0.5 # RMSE = 76.85733

t2.ets_1 <- ets(t2); plot(t2.ets_1)
# ETS method gives best fit for alpha = beta = 0.0583. Ignores seasonality.
t2.predict <- predict(t2.ets_1, n.ahead = 12); ts.plot(t2, ts(t2.predict$mean[1:12], freq = 12, start = c(2016, 01)), lty=1:2)
rmse(t2.ets_1$residuals) # RMSE = 82.32962

t2.ets_2 <- ets(t2, model = "AAA"); plot(t2.ets_2)
# ETS method gives best fit for alpha = 0.1284, beta = 1e-04, gamma = 1e-04
t2.predict <- predict(t2.ets_2, n.ahead = 12); ts.plot(t2, ts(t2.predict$mean[1:12], freq = 12, start = c(2016, 01)), lty=1:2)
# Less sensitive to Seasonality. Final Forecast.
rmse(t2.ets_2$residuals) # RMSE = 63.99216, Lowest So Far. Using AAA ETS For Results.

prod_2 <- forecast(t2.ets_2, h = 3)



##### Prod_3 #####

t3 <- ts(raw_data[raw_data$Product == "Prod_3", "Demand"], start = c(2013, 01), freq = 12)

plot(t3) # Trending Down - Non Stationary series
boxplot(t3~cycle(t3)) # Minor Seasonality, Mostly attributed to Decreasing Trend
plot(decompose(t3)) # Trend Dictates Time Series
acf(t3) # AR Component as expected
pacf(t3) # Auto-Regression with lag 1
plot(decompose(diff(t3))) # First order differential brings little stationarity. Trend is contanimated.
acf(diff(t3)) # ACF plot of first order differential no MA component

# Attempting Exponential Smoothing Method with low response to seasonality
t3.ets <- ets(t3, model = "AAN")
t3.predict <- t3.predict <- predict(t3.ets, n.ahead = 12); ts.plot(t3, ts(t3.predict$mean[1:12], freq = 12, start = c(2016, 01)), lty=1:2)
rmse(t3.ets$residuals) # RMSE = 41.06534

t3.ets_2 <- ets(t3, model = "AAA")
t3.predict <- predict(t3.ets_2, n.ahead = 12); ts.plot(t3, ts(t3.predict$mean[1:12], freq = 12, start = c(2016, 01)), lty=1:2)
# alpha = 0.000108, beta = 0.000106, gamma = 0.00168. Inline with a consistently decreasing time series.
rmse(t3.ets_2$residuals) # RMSE = 38.80471. Better Model.

prod_3 <- forecast(t3.ets_2, h = 3)



##### Prod_4 #####
t4 <- ts(raw_data[raw_data$Product == "Prod_4", "Demand"], start = c(2013, 01), freq = 12)

plot(t4) # Trending Up, Appears Seasonal
boxplot(t4~cycle(t4)) # Strong Seasonality in Winters
plot(decompose(t4)) # Time Series well explained by Trend and Seasonality. 
plot(acf(diff(t4))) #Supports Annual Seasonality

t4.ets <- ets(t4, model = "AAA")
t4.predict <- forecast(t4.ets, h = 12); ts.plot(t4, ts(t4.predict$mean[1:12], freq = 12, start = c(2016, 01)), lty=1:2)
# Alpha = 0.001078, Beta = 0.000116, Gamma = 0.004372
rmse(t4.ets$residuals) # RMSE = 45.4825

prod_4 <- forecast(t4.ets, h = 3)



##### Prod_5 #####
t5 <- ts(raw_data[raw_data$Product == "Prod_5", "Demand"], start = c(2013, 01), freq = 12)

plot(t5) # Trending Up with 2 spikes annually
boxplot(t5~cycle(t5)) # Strong Seasonal effects observed in July and November
plot(decompose(t5)) 
pacf(t5) # Stong Annual Seasonality

# Attempting Exponential Smoothing Method with sensitivity to seasonality
t5.hw <- HoltWinters(t5); plot(t5.hw) # Fits very well
# Alpha = 0.5657, Beta = 0.001527, Gamma = 0.1911
ts.plot(t5, predict(t5.hw, n.ahead = 12), lty=1:2)
(t5.hw$SSE/36)^0.5 # RMSE = 96.235

prod_5 <- forecast(t5.hw, h = 3)



##### Prod_6 #####
t6 <- ts(raw_data[raw_data$Product == "Prod_6", "Demand"], start = c(2013, 01), freq = 12)

plot(t6)
boxplot(t6~cycle(t6)) # Strong Annual Seasonality
plot(decompose(t6))

t6.hw <- HoltWinters(t6); plot(t6.hw)
ts.plot(t6, predict(t6.hw, n.ahead = 12), lty=1:2)
(t6.hw$SSE/36)^0.5 # RMSE = 46.05257

t6.ets <- ets(t6, model = "AAA")
t6.predict <- predict(t6.ets, n.ahead = 12); ts.plot(t6, ts(t6.predict$mean[1:12], freq = 12, start = c(2016, 01)), lty=1:2)
rmse(t6.ets$residuals) # RMSE = 40.09659

t6.ets_2 <- ets(t6)
t6.predict <- predict(t6.ets_2, n.ahead = 12); ts.plot(t6, ts(t6.predict$mean[1:12], freq = 12, start = c(2016, 01)), lty=1:2)
rmse(t6.ets_2$residuals) # RMSE = 0.030928

prod_6 <- forecast(t6.ets_2, h = 3)

##### Prod_7 #####
t7 <- ts(raw_data[raw_data$Product == "Prod_7", "Demand"], start = c(2013, 01), freq = 12)

plot(t7)
boxplot(t7~cycle(t7)) # Seasonal. Dips in March and August/
plot(decompose(t7)) # Consistent Trend, Randomness Minimized.

acf(t7)
pacf(t7)

plot(diff(t7))
plot(pacf(diff(t7)))

# Auto ARIMA
t7.arima <- auto.arima(t7, ic = "aic") # ARIMA(0,0,0)(1,1,0)[12]
plot(forecast(t7.arima, h = 12))
rmse(t7.arima$residuals) #RMSE = 75.7521

# ETS
t7.ets <- ets(t7)
plot(predict(t7.ets, n.ahead = 12))
rmse(t7.ets$residuals) #RMSE = 0.003400456

# Selecting ETS model for Results
prod_7 <- forecast(t7.ets, h = 3)



##### Prod_8 #####
t8 <- ts(raw_data[raw_data$Product == "Prod_8", "Demand"], start = c(2013, 01), freq = 12)

plot(t8) # Cyclic Demand. 
boxplot(t8~cycle(t8)) # High Demand in Winters. Low Demand in September.
plot(decompose(t8)) # Range of Randomness more than Range of Trend

acf(t8)
pacf(t8) # Auto-Regression with lag 1

# Auto ARIMA
t8.arima <- auto.arima(t8, ic = "aic") # Model Selects Seasonal ARIMA with order (1,0,0)(1,0,0)[12]
plot(forecast(t8.arima, h = 12))
rmse(t8.arima$residuals) #RMSE = 13.98552

# ETS with Level and Seasonal Components will perform better. Model Type: ANA
t8.ets <- ets(t8, model = "ANA")
t8.predict <- predict(t8.ets, n.ahead = 12); ts.plot(t8, ts(t8.predict$mean[1:12], freq = 12, start = c(2016, 01)), lty=1:2)
rmse(t8.ets$residuals) #RMSE = 8.760234

# Letting Algorithm decide model type.
t8.ets_2 <- ets(t8) # Model Type determind: MNA
t8.predict <- predict(t8.ets_2, n.ahead = 12); ts.plot(t8, ts(t8.predict$mean[1:12], freq = 12, start = c(2016, 01)), lty=1:2)
rmse(t8.ets_2$residuals) #RMSE = 0.035409, Significantly less

# Selecting ETS with Model(MNA) for Results
prod_8 <- forecast(t8.ets_2, h = 3)



##### Prod_9 #####
t9 <- ts(raw_data[raw_data$Product == "Prod_9", "Demand"], start = c(2013, 01), freq = 12)

plot(t9) # Cyclic Demand
boxplot(t9~cycle(t9)) # Big uptick in February, Annual Trend Prominent
plot(decompose(t9)) # Evident Seasonality, Range of Trend is negligible

# ETS Method (ANA) giving more importance to Level. Alpha higher than needed. Gamma, Low. 
t9.ets <- ets(t9);
t9.predict <- forecast(t9.ets, h = 12); ts.plot(t9, ts(t9.predict$mean[1:12], freq = 12, start = c(2016, 01)), lty=1:2)
rmse(t9.ets$residuals) #RMSE = 5.784867

# Holt Winter Method choosing alpha = beta = 0. Gamma = 1. Model will implement last cycle's seasonality only.
t9.hw <- HoltWinters(t9); plot(t9.hw)
ts.plot(t9, predict(t9.hw, n.ahead = 12), lty = 1:2)
# RMSE = 6.819834
(t9.hw$SSE/36)^0.5

# Attempting Auto-Arima
acf(t9)
pacf(t9) # Strong dependence on lag 1 and lag 12

t9.arima <- auto.arima(t9, ic = "aic") # Gives us Seasonal ARIMA with order: (0,0,0)(0,1,0)[12]
# This will give us same result as Holt Winter with alpha = beta = 0 and gamma = 1
plot(forecast(t9.arima, h = 12))
rmse(t9.arima$residuals) # RMSE = 6.86247, Almost Same as Holt Winter Method

#Selecting Seasonal ARIMA for results
prod_9 <- forecast(t9.arima, h = 3)




##### Prod_10 #####
t10 <- ts(raw_data[raw_data$Product == "Prod_10", "Demand"], start = c(2013, 01), freq = 12)

plot(t10) # Intermittent Demand
boxplot(t10~cycle(t10)) # All Statistics will be misleading with such sparse data
plot(decompose(t10)) # No Trend. Comparable randomness. Very slgiht seasonality.

# Croston Method for Intermittent Demand
t10.croston <- croston(t10, h = 12)
t10.predict.croston <- predict(t10.croston); plot(t10.predict.croston)

# Aggregating Demand Quaterly
test <- raw_data[raw_data$Product == "Prod_10",]
test$index <- ceiling(seq(1,36,1)/3)
test <- aggregate(list(Demand = test$Demand), by = list(index = test$index), FUN = sum)

t10_quaterly <- ts(test$Demand, start = c(2013, 01), freq = 4)

plot(t10_quaterly) # Erratic
boxplot(t10_quaterly~cycle(t10_quaterly)) # First Quarter usually has higher demand
plot(decompose(t10_quaterly)) # Randomness range higher than seasonality and trend.

acf(t10_quaterly) # Could Try MA coefficient 1
pacf(t10_quaterly) # Could try AR coefficient 1

t10_quaterly.arima <- arima(t10_quaterly, order = c(1,0,1))
t10_quaterly.predict <- forecast(t10_quaterly.arima, h = 4); ts.plot(t10_quaterly, t10_quaterly.predict$pred, lty=1:2) # Captures Trend. Fails to capture Seasonality.

# Both methods yield similar results ie Quaterly demand of ~2.5
# Using monthly results from Croston Method

prod_10 <- forecast(t10.croston, h = 3)




# Collecting Results

result <- cbind(data.frame(c(prod_1$mean,prod_2$mean,prod_3$mean,prod_4$mean,prod_5$mean,
            prod_6$mean,prod_7$mean,prod_8$mean,prod_9$mean,prod_10$mean)),
            rep(c(201601, 201602, 201603), 10),
            c(rep('Prod_1', 3), rep('Prod_2', 3), rep('Prod_3', 3), rep('Prod_4', 3), rep('Prod_5', 3),
              rep('Prod_6', 3), rep('Prod_7', 3), rep('Prod_8', 3), rep('Prod_9', 3), rep('Prod_10', 3))
            ); colnames(result) <- c('Forecast', 'Date', 'Product')

result$Forecast <- trunc(result$Forecast + 0.5)

result <- merge(raw_data, result, by = c('Date', 'Product'), all = TRUE)

write.csv(result, '~/Downloads/Result_ParthSood/Task2_Results.csv', row.names = FALSE)
