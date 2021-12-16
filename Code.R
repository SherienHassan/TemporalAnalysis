Regular AutoArima:
> install.packages("forecast")
> library(forecast)
> collide <- read.csv(file.choose(),header=T)
> collidets=ts(collide$`pedestrian`, freq=365,start=c(2014, 1)) 
> plot(collidets)
> auto.arima(collidets)
> autoarima <- auto.arima(collidets)
> autoarima$residuals
> residualautoarima=autoarima$residuals
> plot(residualautoarima)
*GET CONFIDENCE INTERVAL*
> auto.arima(collidets)
#take the sigma^2 value
> sqrt(#sigma^2 value)
> #the value from the line above *1.96
> plot(residualautoarima)
> plot(residualautoarima)
> abline(h=174.1204, lty=2)
> abline(h=-174.1204, lty=2)
#abline(h=#value found after multiplying sqrt(sigma^2)*1.96,lty=2)
#abline(h=-#value found after multiplying sqrt(sigma^2)*1.96,lty=2)
> plot(residualautoarima,type="o")
*****next section is to forecast*****
> forecast<-forecast(autoarima, h=17)
#predict(autoarima, #)
> forecast
> plot(forecast)
> plot(forecast$residuals)
> qqnorm(forecast$residuals)
> acf(forecast$residuals)
> pacf(forecast$residuals)
> summary(autoarima)
> accuracy(autoarima)

Regular Arima:
arima(collidets, order=c())

REGULAR ARIMA TRIALS:
#arima(collidets, order=c())

***(0,0,0)****
>arima(collidets,order=c(0,0,0)) #gives you back summary(collidets)

***(1,0,0)****
> arima(collidets,order=c(1,0,0))
> onezerozero <- arima(collidets,order=c(1,0,0))
> onezerozero
#sigma^2=9010
> sqrt(9010)
> 94.92102*1.96 #=186.0452
> residualonezerozero<- onezerozero$residuals
> plot(residualonezerozero)
> abline(h=186.0452, lty=2)
> abline(h=-186.0452, lty=2)

**log arima**
> log <- log(collidets)  
> autoarimalog <- auto.arima(log)
> sqrt(0.02318) #=0.1522498
> 0.1522498*1.96 #=0.2984096
> autoarimalog
> autoarimalog$residuals
> residualautoarimalog <- autoarimalog$residuals
> plot(residualautoarimalog)
> abline(h=0.2984096, lty=2)
> abline(h=-0.2984096, lty=2)

**sqrt arima**
> sqrt <- sqrt(collidets)  
> autoarimasqrt <- auto.arima(sqrt)
> sqrt(3.315) #=1.820714
> 1.820714*1.96 #=3.568599
> autoarimasqrt
> autoarimasqrt$residuals
> residualautoarimasqrt <- autoarimasqrt$residuals
> plot(residualautoarimasqrt)
> abline(h=3.568599, lty=2)
> abline(h=-3.568599, lty=2)

****Garch****
>library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(TSA)
library(forecast)
library(rugarch)
> install.packages("quantmod”)
> library("quantmod”)
>newcollidets=diff(collidets)
>garchmodel <-  ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1, 1)),  mean.model = list(armaOrder=c(5, 1)), distribution.model = "std")
>actualgarchmodel<-ugarchfit(spec=garchmodel,data=newcollidets)
#look for Akaike
>forcastgarch=ugarchforecast(actualgarchmodel,n.ahead=20,method=c("Partial","Full")[1])
>forecastgarch


Attempt 2:
install.packages("timeDate")
install.packages("timeSeries")
install.packages("fBasics")
install.packages("fGarch")

library("timeDate")
library("timeSeries")
library("fBasics")
library("fGarch")

collide <- read.csv(file.choose(),header=T)
collidets=ts(collide$`pedestrian`, freq=365,start=c(2014, 1)) 
newcollidets=diff(collidets)
model_0=garchFit(~ garch(1,1), data=newcollidets, trace=FALSE)
predict_0 <- predict(model_0, n.ahead=10)

predict_012 <- predict(model_0, n.ahead=10, plot=TRUE, crit_val=2)

> fit2=garchFit(~ garch(1,1), data=newcollidets, trace=FALSE,cond.dist="sged")
> predict(fit2,n.ahead=20, plot=TRUE)

> fit3=garchFit(~ garch(1,1), data=newcollidets, trace=FALSE,cond.dist="QMLE")
> predict(fit3,n.ahead=20,plot=TRUE,conf=.9,nx=100)



decompose:
plot(decompose(collidets))



AutoSeasonalArima #(could be done with data=collidets or date=deseasonal_cnt)
install.packages("rio")
install.packages("ggplot2")
install.packages("forecast")
install.packages("tseries")
install.packages("tidyverse")
library('ggplot2')
library('readxl')
library('zoo')
collide <- read.csv(file.choose(),header=T)
collidets=ts(collide$`pedestrian`, freq=365,start=c(2014, 1)) 
install.packages("forecast")
library("forecast")
decomp=stl(collidets,"periodic")
deseasonal_cnt<-seasadj(decomp)
plot(decomp,main="Pedestrian Crash Rates Decomposed")
install.packages("tseries")
library("tseries")
plot(deseasonal_cnt, main="Pedestrian Crash Rates",xlab="Date", ylab="Pedestrian", type="l", col="black")
adf.test(deseasonal_cnt,alternative="stationary")

#SQRT OF deseasonal_cnt AUTO ARIMA SEASONAL
deseasonal_cnt_sqrt=sqrt(deseasonal_cnt) #square root
Acf(Acount_sqrt, lag.max=200, main='')
Pacf(Acount_sqrt, lag.max=200, main='')
autosqrt<-auto.arima(deseasonal_cnt_sqrt,seasonal=FALSE)
autosqrt
#sigma^2 estimated as 2.385
#sqrt(2.385)=1.544345
#1.544345*1.96=3.026916
residualautosqrt=autosqrt$residuals
plot(residualautosqrt)
abline(h=3.026916, lty=2)
abline(h=-3.026916, lty=2)

#LOG OF deseasonal_cnt AUTO ARIMA SEASONAL
deseasonal_cnt_log=log(deseasonal_cnt) #log
Acf(deseasonal_cnt_log, lag.max=200, main='')
Pacf(deseasonal_cnt_log, lag.max=200, main='')
autolog<-auto.arima(deseasonal_cnt_log,seasonal=FALSE)
autolog
#sigma^2 estimated as 0.01615
#sqrt(0.01615)=0.1270827
#0.1270827*1.96=0.2490821
residualautolog=autolog$residuals
plot(residualautolog)
abline(h=0.2490821, lty=2)
abline(h=-0.2490821, lty=2)

#DIFF ONCE OF deseasonal_cnt AUTO ARIMA SEASONAL
count_d1=diff(deseasonal_cnt,differences=1) #differencing once does not need to be done here
Acf(count_d1, lag.max=200, main='')
Pacf(count_d1, lag.max=200, main='')
autodiff<-auto.arima(count_d1,seasonal=FALSE)
autodiff
#sigma^2 estimated as 5785
#sqrt(5785)=76.05919
#76.05919*1.96=149.076
residualautodiff=autodiff$residuals
plot(residualautodiff)
abline(h=149.076, lty=2)
abline(h=-149.076, lty=2)



#REGULAR AUTOSESONAL
Acf(deseasonal_cnt, lag.max=200, main='')
Pacf(deseasonal_cnt, lag.max=200, main='')
autodeseasonal_cnt<-auto.arima(deseasonal_cnt,seasonal=FALSE)
autodeseasonal_cnt
#sigma^2 estimated as: 5785
#sqrt(5785)=76.05919
#76.05919*1.96
#149.076
residualautodeseasonal_cnt=autodeseasonal_cnt$residuals
plot(residualautodeseasonal_cnt)
abline(h=149.076, lty=2)
abline(h=-149.076, lty=2)

#autocollidets<-auto.arima(collidets,seasonal=FALSE)
#autocollidets
##sigma^2 estimated as: 8239
##sqrt(8239)=90.76894
##90.76894*1.96
##h=177.9071
#residualautocollidets=(autocollidets$residuals)
#plot(residualautocollidets)
#abline(h=177.9071, lty=2)
#abline(h=-177.9071, lty=2)

Forecast:
fcast=forecast(fit, h = 7)
autoplot(fcast,include=90, xlab="",ylab="",main="")


fit= (autodeseasonal_cnt)
forecast(fit, h = 7)
stupid=forecast(fit, h = 7)
autoplot(stupid,include=90, xlab="",ylab="",main="")


par(mfrow=c(2,1))


Winning Seasonal Arima 
model1=arima(collidets,order=c(5,1,1),seasonal=list(order=c(1,0,1),method="ML",period=7))
model1
plot(residualmodel1)
abline(h=74.63913, lty=2)
abline(h=-74.63913, lty=2)
residualmodel1=model1$residuals
acf(residualmodel1)
pacf(residualmodel1)
fit=(model1)
forecast(fit, h=7)
model1forecast=forecast(fit, h=7)
autoplot(model1forecast,include=90)

#with deseasonal_cnt
install.packages("forecast")
library("forecast")
collide <- read.csv(file.choose(),header=T) 
collidets=ts(collide$`pedestrian`, freq=365,start=c(2014, 1)) 
decomp=stl(collidets,"periodic")
deseasonal_cnt<-seasadj(decomp)
model2=arima(deseasonal_cnt,order=c(5,1,2),seasonal=list(order=c(1,0,1),method="ML",period=7))
model2
residualmodel2=model2$residuals
acf(residualmodel2)
pacf(residualmodel2)
fit2=(model2)
forecast(fit2, h=7)
model2forecast=forecast(fit2, h=7)
autoplot(model2forecast,include=90)
plot(residualmodel2)
abline(h=192.569, lty=2)
abline(h=-192.569, lty=2)


decomp=stl(collidets,"periodic")
deseasonal_cnt<-seasadj(decomp)
model3=arima(deseasonal_cnt,order=c(5,1,2),seasonal=list(order=c(1,1,1),method="ML",period=7))
model3
residualmodel3=model3$residuals
acf(residualmodel3)
pacf(residualmodel3)
fit3=(model3)
forecast(fit3, h=7)
model3forecast=forecast(fit3, h=7)
autoplot(model3forecast,include=90)
plot(residualmodel3)
abline(h=127.6711, lty=2)
abline(h=-127.6711, lty=2)



# 2.58(when creating confidence bands to finnese instead of 1.96)

#WORKING ARIMA GARCH CODE
library("tsbox")
install.packages("remotes")
library("remotes")
remotes::install_github("christophsax/tsbox")
library(tsbox)
library("tsbox")
x.ts <- ts_xts(collidets)
is.xts(x.ts)
x.ts
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(TSA)
library(forecast)
library(rugarch)
model_01=ugarchspec(mean.model=list(armaOrder=c(5,1),include.mean=TRUE),variance.model=list(garchOrder=c(1,1)))
garchmodel_01=ugarchfit(data=x.ts,spec=model_01)
show(garchmodel_01)
plot(garchmodel_01)
ugarchforecast(garchmodel_01,n.ahead=20)
plot(ugarchforecast(garchmodel_01,n.ahead=20))


model_01=ugarchspec(mean.model=list(arimaOrder=c(5,1,1),include.mean=TRUE),variance.model=list(garchOrder=c(1,1)))
garchmodel_01=ugarchfit(data=x.ts,spec=model_02)
ugarchforecast(garchmodel_01,n.ahead=7)

T+1  625.9 96.88
T+2  570.0 96.29
T+3  621.1 95.74
T+4  675.0 95.23
T+5  591.7 94.75
T+6  568.4 94.31
T+7  656.1 93.90



model_02=ugarchspec(mean.model=list(armaOrder=c(5,1),include.mean=TRUE),variance.model=list(garchOrder=c(1,0)))
garchmodel_02=ugarchfit(data=x.ts,spec=model_02)
ugarchforecast(garchmodel_02,n.ahead=7)

T+1   653.7 85.22
T+2   626.0 90.36
T+3   627.1 91.28
T+4   626.0 91.45
T+5   622.6 91.48
T+6   622.7 91.49
T+7   623.3 91.49

model_03=ugarchspec(mean.model=list(armaOrder=c(5,1),include.mean=TRUE),variance.model=list(garchOrder=c(0,1)))
garchmodel_03=ugarchfit(data=x.ts,spec=model_03)
ugarchforecast(garchmodel_03,n.ahead=7)

T+1  635.3 91.69
T+2  584.4 91.69
T+3  596.7 91.69
T+4  624.1 91.69
T+5  610.3 91.69
T+6  606.4 91.69
T+7  611.3 91.69


model_03=ugarchspec(mean.model=list(armaOrder=c(2,3),include.mean=TRUE),variance.model=list(garchOrder=c(2,2)))
garchmodel_03=ugarchfit(data=x.ts,spec=model_03)
ugarchforecast(garchmodel_03,n.ahead=7)












