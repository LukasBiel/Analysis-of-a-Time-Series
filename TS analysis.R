
library(zoo)
library(forecast)
library(tseries)
library(ggplot2)

dane <- read.csv("dane_projekt.csv")
dane <- ts(dane, end = c(2023,12), frequency = 12) 
plot(dane)

dane<-dane[,-1]
plot(dane)
dane
plot(decompose(dane))

plot(decompose(a))
a = c(1,2,3,4.1,5,5.9,6.05,7,8.01,9.03,10,10.94)
a <- ts(a, end = c(2023,12), frequency = 2)

ndiffs(dane)
nsdiffs(dane)
tsdisplay(dane)
acf(dane, lag.max = 250)





trend_model = tslm(dane~trend+poly(trend,raw=TRUE,degree = 3))
summary(trend_model)
plot(dane)
lines(fitted(trend_model))

monthplot(dane,main="Wykres miesięczny") 
seasonplot(dane)

dane2 = diff(dane, differences = 2)
plot(dane2)
abline(a = mean(dane2), b = 0, col = "red")
c(mean(dane2[1:50]),mean(dane2[50:100]),mean(dane2[100:150]),mean(dane2[150:200]))
c(sd(dane2[1:50]),sd(dane2[50:100]),sd(dane2[100:150]),sd(dane2[150:200]))
adf.test(dane2)

?nsdiffs

dane3 = diff(dane, differences = 1)
plot(dane3)
acf(dane3,lag.max = 50)
pacf(dane3, lag.max = 50)

mean(dane3[1:50])
mean(dane3[130:180])
adf.test(dane3)

tsdisplay(dane2)
acf(dane2, lag.max = 250)
pacf(dane2, lag.max = 250)

shapiro.test(diff(dane2))
shapiro.test(dane3)

summary(ar.aic)
pacf(dane2, lag.max = 50)
acf(dane2, lag.max = 50)
acf(dane2, lag.max = 12)
ar.aic<-ar(dane2)
ar.aic$aic
a = ar(dane2, order.max = 4)

ar.aic<-ar(dane2,bic=T)
ar.aic$aic
ma.aic <- ma(dane2, order = 3)
ma.aic
plot(as.numeric(names(ar.aic$aic)),ar.aic$aic, xlab="rząd modelu autoregresji (p)",
     ylab="Kryterium AIC", type="b")

summary(ma.aic)
AR.4 <- Arima(dane, order = c(4,1,0))
AR.4.2 <- Arima(dane, order = c(4,2,0))
AR.8 <- Arima(dane, order = c(8,2,0))
MA.3 <- Arima(dane, order = c(0,2,3))
MA.3.2 <- Arima(dane, order = c(0,1,3))
AR.4
AR.8
MA.3
MA.3.2
AR.4.2

AR.2 <- Arima(dane, order = c(2,1,0))

testo<- Arima(dane,order=c(0,1,7))

Box.test(AR.4$residuals, lag=50, type="Ljung-Box")
Box.test(AR.8$residuals, lag=50, type="Ljung-Box")
Box.test(AR.4.2$residuals, lag=50, type="Ljung-Box")
Box.test(MA.3$residuals, lag=50, type="Ljung-Box")
Box.test(MA.3.2$residuals, lag=50, type="Ljung-Box")
Box.test(arima.aic$residuals, lag=50, type="Ljung-Box")

arima.aic<-auto.arima(dane,ic="bic")
arima.aic2<-auto.arima(dane,ic="aic")


AR.8$aicc

summary(AR.2)
summary(AR.4)
summary(MA.3.2)
summary(AR.4.2)
summary(AR.8)
summary(MA.3)
summary(arima.aic)
summary(arima.aic2)

summary(testo)

ar.aic
p <- forecast(AR.2, h=24) 
wykres <- autoplot(dane) +
  autolayer(p, series = "Prognoza") +
  labs(title = "Prognoza SARIMA",
       y = "Wartość") +
  theme_minimal()
wykres + theme(axis.text.x = element_text(margin = margin(b = 10)),axis.text.y = element_text(margin = margin(l = 10)))

AR.2

model_AR4
p_ar4 <- forecast(AR.2, h=24) 
wykres <- autoplot(dane) + 
  autolayer(p_ar4, series = "Prognoza") + 
  labs(title = "Prognoza SARIMA", y = "Wartość") +
  theme_minimal()
wykres + 
  theme(axis.text.x = element_text(margin = margin(b = 10)),axis.text.y = element_text(margin = margin(l = 10)))


p_1diff <- forecast(model_AR4, h=24) 
wykres4 <- autoplot(dane) + autolayer(p_1diff, series = "Prognoza") + labs(title = "Prognoza SARIMA", y = "Wartość") + theme_minimal()
wykres4 + theme(axis.text.x = element_text(margin = margin(b = 10)),axis.text.y = element_text(margin = margin(l = 10)))


dane.forecast.mean<-meanf(dane,20)

usgdp.forecast.mean

ap.forecast.mean<-meanf(ap,20)

ap.forecast.mean

plot(dane.forecast.mean,main="Prognoza na podstawie średniej")

plot(ap.forecast.mean,main="Prognoza na podstawie średniej")


dane.forecast.rwf<-rwf(dane,drift = TRUE,20)
plot(dane.forecast.rwf)


model <- auto.arima(dane, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
prognoza <- forecast(model, h = 20)
plot(prognoza)

# Rysowanie wykresu
autoplot(prognoza) +
  autolayer(dane, series = "Oryginalne dane") +
  labs(title = "Prognoza ARIMA z trendem wielomianowym",
       y = "Wartość") +
  theme_minimal()
