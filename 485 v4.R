library(dplyr)
library(TSA)
library(tseries)
library(forecast)
library(zoo)

psmsl <- read.csv("psmsl.csv", header = FALSE)

# clean data
psmsl.new <- filter(psmsl, V2 != -99999 & V1 > 1943)
psmsl.new <- psmsl.new[1:2]

# plot linear regression
lm = lm(V2 ~ V1, data = psmsl.new)
summary(lm)
plot(psmsl.new$V1, psmsl.new$V2, type = 'o', pch = 20,
     xlab = "Date", ylab = "Sea Level (mm)", main = "Sea Level in Vancouver")
abline(lm, col = "red")

# plot with rolling 12 month average and standard deviation
plot(psmsl.new$V1, psmsl.new$V2, type = 'o', pch = 20, col = "gray",
     xlab = "Date", ylab = "Sea Level (mm)", main = "Sea Level in Vancouver")

roll.avg = rollmean(psmsl.new$V2, 12, fill = NA, align = "right")
lines(psmsl.new$V1, roll.avg, col = "red")

roll.sd = rollapplyr(psmsl.new$V2, 12, sd, fill = NA, align = "right")
lines(psmsl.new$V1, roll.avg + roll.sd, col = "blue")
lines(psmsl.new$V1, roll.avg - roll.sd, col = "blue")

legend("topleft", inset = 0.01, legend = c("12 Month Rolling Mean", "12 Month Rolling SD"), 
       col = c("red", "blue"), lty=1)

# test for d
ndiffs(psmsl.new$V2)
adf.test(psmsl.new$V2); pp.test(psmsl.new$V2); kpss.test(psmsl.new$V2)
adf.test(diff(psmsl.new$V2)); pp.test(diff(psmsl.new$V2)); kpss.test(diff(psmsl.new$V2))

# acf, pacf, and eacf
par(mfrow=c(1,2))
acf(psmsl.new$V2, max.lag = 60)
pacf(psmsl.new$V2, max.lag = 60)
eacf(psmsl.new$V2)

# try arima(1,1,1) has seasonality
model_111_000 = arima(x = psmsl.new$V2, order = c(1, 1, 1), 
                      seasonal=list(order=c(0,0,0), period=12))
model_111_000
acf(model_111_000$residuals)
pacf(model_111_000$residuals)

# try arima(1,1,1) Seasonal(2,0,1) sar2 within 1 standard error
model_111_201 = arima(x = psmsl.new$V2, order = c(1, 1, 1), 
                      seasonal=list(order=c(2,0,1), period=12))
model_111_201
acf(model_111_201$residuals)
pacf(model_111_201$residuals)

# arima(1,1,1) Seasonal(1,0,1)
model_111_101 = arima(x = psmsl.new$V2, order = c(1, 1, 1), 
                      seasonal=list(order=c(1,0,1), period=12))
model_111_101
acf(model_111_101$residuals)
pacf(model_111_101$residuals)

# p-value for coefficients
(1-pnorm(abs(model_111_101$coef)/sqrt(diag(model_111_101$var.coef))))*2

# test model
model = arima(x = psmsl.new$V2[0:918], order = c(1, 1, 1),
              seasonal=list(order=c(1, 0, 1), period=12))
model
acf(model$residuals)
pacf(model$residuals)

par(mfrow=c(1,3))
qqnorm(model$residuals);qqline(model$residuals, col = "red", lwd = 2)
hist(model$residuals,breaks=12)
plot(model$residuals)

# plot forecasts
forcasts = predict(model, 12)

par(mfrow=c(1,1))
plot(psmsl.new$V1[798:929], psmsl.new$V2[798:929], type = 'o', pch = 20,
     xlab = "Date", ylab = "Sea Level (mm)", main = "Sea Level in Vancouver")
points(psmsl.new$V1[918:929], psmsl.new$V2[918:929], pch = 20, col="blue")
lines(psmsl.new$V1[918:929], psmsl.new$V2[918:929], col="blue")

points(psmsl.new$V1[918:929], forcasts$pred, pch = 20, col="red")
lines(psmsl.new$V1[918:929], forcasts$pred, pch = 20, col="red")

lines(psmsl.new$V1[918:929], forcasts$pred+forcasts$se, col="red")
lines(psmsl.new$V1[918:929], forcasts$pred-forcasts$se, col="red")

legend("topright", inset = 0.01, legend = c("pred", "actual"), 
       col = c("red", "blue"), lty=1)
