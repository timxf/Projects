#Reading Data into R
interest <- read.csv("~/Desktop/interest.csv")
#Create and Plot Time series
ts = ts(interest[,-1], start = c(2000,7), frequency = 12)
plot(ts, plot.type = "single", col = 1:ncol(ts), main = "Interest Rates with Different Maturity Times", ylab = "Interest Rate")
legend("bottomleft", colnames(ts), col=1:ncol(ts), lty=1, cex=.65)

## Create Time Series with Thirty Years of Maturity
irts = ts(interest[,2], start = c(2000,7), frequency=12)
plot(irts)

#ACF of Time Series with Thirty Years of Maturity
par(mfrow =c(1,1))
acf = acf(irts, plot = FALSE, lag.max = 50)
acf$lag = acf$lag*12

plot(acf, main = "ACF of Interest Rate")
#PACF of Time Series with Thirty Years of Maturity
pacf = pacf(irts, plot = FALSE,lag.max = 50)
pacf$lag = pacf$lag*12
plot(pacf, main = "PACF of Interest Rate")

#First Difference of Time Series with Thirty Years of Maturity
diff <- diff(irts, differences=1)
plot(diff, main = "Figure 2. First Differencing of Interest Rate", ylab = "")

#ACF of First Difference
par(mfrow =c(1,2))
acf = acf(diff, plot = FALSE,lag.max=50 )
acf$lag = acf$lag*12
plot(acf, main = "ACF of First Difference")

#PACF of First Difference
pacf = pacf(diff, plot = FALSE,lag.max=50)
pacf$lag = pacf$lag*12
plot(pacf, main = "PACF of First Difference")

# ARIMA model 
arima1 = Arima(irts, order = c(0,1,1))
plot(irts, main = "Autoregressive Integrated Moving Average", ylab = "Interest Rates", type = "o", pch = "*")
lines(fitted(arima1), col = 2, type = "o", pch ="*")
legend("bottomleft",lty=1, col=1:2, c("Data", "ARIMA"),pch="*")

#Auto Arima
auto.arima(irts,stepwise = FALSE)

#ARIMA Model Diagnosis
acf = acf(residuals(fit), plot = FALSE, lag.max=50 )
acf$lag = acf$lag*12
plot(acf, main = "ACF of Residuals")

Box.test(residuals(fit), lag=50, fitdf=4, type="Ljung")

# Fitting Holt's Linear Trend models
ts1  = ts(irts[-c(167:178)], start = c(2000,7), frequency=12)
fit = holt(ts1, alpha=0.75, beta=0.2, initial="simple", exponential=TRUE, h =12)
summary(fit)
fit2 = holt(ts1, alpha=0.50, beta=0.2, initial="simple", exponential=TRUE, h =12)
summary(fit2)
fit3 = holt(ts1, alpha=0.25, beta=0.2, initial="simple", exponential=TRUE, h =12)
summary(fit3)
fit4 = holt(ts1, alpha=0.75, beta=0.4, initial="simple", exponential=TRUE, h =12)
summary(fit4)
fit5 = holt(ts1, alpha=0.75, beta=0.6, initial="simple", exponential=TRUE, h =12)
summary(fit5)
fit6 = holt(ts1, alpha=0.75, beta=0.8, initial="simple", exponential=TRUE, h =12)
summary(fit6)

# Finding Opitmal Holt's Model
plot(ts(irts[-c(167:178)], start = c(2000,7), frequency=12), main = expression(paste("Holt's Linear Trend Model ",beta == 0.2)), ylab = "Interest Rates", type = "o", pch = "*")
lines(fitted(fit), col = 2, type ="o", pch = "*")
lines(fitted(fit2), col = 3, type ="o", pch = "*")
lines(fitted(fit3), col = 4, type ="o", pch = "*")
legend("bottomleft",lty=1, col=1:4, c("Data", expression(alpha == 0.75), expression(alpha == 0.5),expression(alpha == 0.25)),pch="*")
plot(ts(irts[-c(167:178)], start = c(2000,7), frequency=12), main = expression(paste("Holt's Linear Trend Model with ",alpha == 0.75)) , ylab = "Interest Rates", type = "o", pch = "*")
lines(fitted(fit4), col = 5, type ="o", pch = "*")
lines(fitted(fit5), col = 6, type ="o", pch = "*")
lines(fitted(fit6), col = 7, type ="o", pch = "*")
legend("bottomleft",lty=1, col=c(1,5:7), c("Data", expression(beta == 0.4), expression(beta == 0.6),expression(beta == 0.8)),pch="*")

## Neural Networking Model
library(caret)
nn = nnetar(irts)
plot(irts, main = "Neural Networking Model", ylab = "Interest Rates", type = "o", pch = "*")
lines(fitted(nn), col = 2, type = "o", pch = "*")
legend("bottomleft",lty=1, col=1:2, c("Data", "Neural Networking"),pch="*")


## Accuracy of Model without last 12 observations
a = forecast(Arima(ts1, order = c(0,1,1)), h =12)
b = forecast(fit6, h =12)
c = forecast(nnetar(ts1), h =12)
accuracy(arima1)
accuracy(fit6)
accuracy(nn)

## Plot of last 12 forecast
plot(irts, main = "Time Series of Interest Rate with Forecasts", ylab = "Interest Rates", pch = "*")
lines(fitted(a), col = 2, type ="o", pch = "*")
lines(fitted(b), col = 3, type ="o", pch = "*")
lines(fitted(c), col = 4, type ="o", pch = "*")
lines(a$mean, col = 2,type ="o" ,pch ="*")
lines(b$mean, col = 3,type ="o" ,pch ="*")
lines(c$mean, col = 4,type ="o" ,pch ="*")
legend("bottomleft",lty=1, col=1:4, c("Observated", "ARIMA","Holt's Linear Trend" ,"Neural Networking"),pch="*")

## Forecast Interest Rates with Different Maturity Times
plot(ts, plot.type = "single", col = 1:ncol(ts), main = "Interest Rates with Different Maturity Times", ylab = "Interest Rate",pch ="*", type ="o")
legend("bottomleft", colnames(ts), col=1:ncol(ts), lty=1, cex=.65, pch = "*")
for (i in 1:4) {
  lines(fitted(holt(ts[,i], alpha=0.75, beta=0.8, initial="simple", exponential=TRUE, h =12)), col =i)
  lines((holt(ts[,i], alpha=0.75, beta=0.8, initial="simple", exponential=TRUE, h =12)$mean), col =i)
  print(holt(ts[,i], alpha=0.75, beta=0.8, initial="simple", exponential=TRUE, h =12))}




