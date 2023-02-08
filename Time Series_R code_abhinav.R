# Importing and preprocessing time series data
my_ts_raw <- read.csv("C:/Users/abhin/Desktop/NCI 2022/Stats/TABA/CarRegistrations.csv", header = FALSE)
head(my_ts_raw)

# Correcting the 1995M01 cell 
my_ts_raw[1,1] = "1995M01"
head(my_ts_raw)
tail(my_ts_raw)

# Missing values check
sum(is.na(my_ts_raw))

my_tsdata= my_ts_raw[2]

library(fpp2)
library(TSstudio)

mycars_ts = ts(my_tsdata, start = c(1995,1), frequency = 12)
start(mycars_ts)  
end(mycars_ts)
head(mycars_ts,20)

# gives seasonality overview 
ts_heatmap(mycars_ts)

# plots timeseries
# plot(mycars_ts)
autoplot(mycars_ts)

#Basic Dataset Information
# ts_info(mycars_ts)

monthplot(mycars_ts)
# seasonplot(mycars_ts)
ggseasonplot(mycars_ts,year.labels = TRUE,year.labels.left = TRUE)+ylab("No. of cars")+ggtitle("Seasonal plot: No of cars registered")

#Seasonal decomposition using decompose() - additive 
car_r_add<-decompose(mycars_ts, type='additive')
car_r_add
plot(car_r_add)

#Seasonal decomposition using decompose() - multiplication
car_r_mult<-decompose(mycars_ts, type='multiplicative')
car_r_mult
plot(car_r_mult)

# check to stabilize variance
# plot(decompose(log(mycars_ts), type = 'multiplicative'))
# Model building and evaluation

# Basic models first
# #mean model
fcast.mean<-meanf(mycars_ts,h=20)
summary(fcast.mean)
plot(fcast.mean)


#naive model
fcast.naive<-naive(mycars_ts,h=20)
summary(fcast.naive)
plot(fcast.naive)

#seasonal naive model
fcast.seasonalnaive<-snaive(mycars_ts,h=20)
summary(fcast.seasonalnaive)
plot(fcast.seasonalnaive)

print("----------------------------------------SES---------------------------------------------")
mycars_ses = ses(mycars_ts, h=6)
mycars_ses
round(accuracy(mycars_ses),2)
autoplot(mycars_ses)
autoplot(mycars_ses)+autolayer(fitted(mycars_ses), series="Fitted")

#Evaluation
qqnorm(mycars_ses$residuals)
qqline(mycars_ses$residuals)
# Check H0: autocrrs of residuals are all zero
Box.test(mycars_ses$residuals, type='Ljung-Box') # signi so problem with acf all zero assumpt

round(accuracy(mycars_ses),2)
checkresiduals(mycars_ses)

print("----------------------------------------Holt---------------------------------------------")
mycars_holt = holt(mycars_ts, h=6)
mycars_holt
round(accuracy(mycars_holt),2)
autoplot(mycars_holt)
autoplot(mycars_holt)+autolayer(fitted(mycars_holt), series="Fitted")

qqnorm(mycars_holt$residuals)
qqline(mycars_holt$residuals) # check if residuals normally distr
Box.test(mycars_holt$residuals, type='Ljung-Box')
round(accuracy(mycars_holt),2)
checkresiduals(mycars_holt)

print("----------------------------------------Holt Winter---------------------------------------------")
mycars_hw = hw(mycars_ts, h=6)
mycars_hw
round(accuracy(mycars_hw),2)
autoplot(mycars_hw)
autoplot(mycars_hw)+autolayer(fitted(mycars_hw), series="Fitted")

#Evaluation
qqnorm(mycars_hw$residuals)
qqline(mycars_hw$residuals)
Box.test(mycars_hw$residuals, type='Ljung-Box')
round(accuracy(mycars_hw),2)
checkresiduals(mycars_hw)


fit1 <- hw(mycars_ts, seasonal='additive')
fit2 <- hw(mycars_ts, seasonal='multiplicative')
accuracy(fit1) # seasonal ='additive'
accuracy(fit2) # seasonal ='multiplicative'

autoplot(mycars_ts) + autolayer(fit1, series ="HW Additive", PI=FALSE) +  autolayer(fit2, series="HW Multilpicative", PI=FALSE)

checkresiduals(fit2)

print("----------------------------------------ETS zzz??---------------------------------------------")
mycars_ets = ets(mycars_ts, model="MMM")
mycars_ets
forecast(mycars_ets,6)
round(accuracy(mycars_ets),2)
# plot(mycars_ets)
#Evaluation
qqnorm(mycars_ets$residuals)
qqline(mycars_ets$residuals)
round(accuracy(mycars_ets),2)
checkresiduals(mycars_ets)
Box.test(mycars_ets$residuals, type='Ljung-Box') #Here P value should be in-significant 

# check with ZZZ
mycars_etsZ = ets(mycars_ts, model="ZZZ") #goes for MAM
mycars_etsZ
forecast(mycars_etsZ,6)
round(accuracy(mycars_etsZ),2)

#Evaluation
qqnorm(mycars_etsZ$residuals)
qqline(mycars_etsZ$residuals)
round(accuracy(mycars_etsZ),2)
checkresiduals(mycars_etsZ)

Box.test(mycars_etsZ$residuals, type='Ljung-Box') #Here P value should be in-significant 

# Durbin watson for autocorrelation test or acf pacf?
# ARIMA PRELIM considerations and stationarity requirements check
print("----------------------------------------ARIMA---------------------------------------------")
ggtsdisplay(mycars_ts) # plots ts with acf and pacf

# we will normalize the variance by Box-Cox transformation i.e. log
mycars_ts2 <- log(mycars_ts)
ggtsdisplay(mycars_ts2)

# Manually checking for pdq 
ndiffs(mycars_ts2)

# differeneced it once
mycars_ts3<- diff(mycars_ts2)
ggtsdisplay(mycars_ts3)

# stationarity test - ok if sig
adf.test(mycars_ts3)

# # to make stationary 
# plot(mycars_ts)
# plot(diff(mycars_ts,differences = 1))

# shortcut to pdq but ont certain to produce best model as is not exhaustive
# auto.arima(mycars_ts) # automatically gives pdq possible values
# acf(mycars_ts)
# pacf(mycars_ts)

# Checking for non-seasonal ARIMA
arima1 = arima(mycars_ts, order=c(1,1,1))
arima1
Box.test(arima1$residuals, type='Ljung-Box') # checks for of the model with data : says autcorre of residuals are zero when insign
round(accuracy(arima1),2)
checkresiduals(arima1)

#model2
arima2 = arima(mycars_ts, order=c(1,1,2))
arima2
Box.test(arima2$residuals, type='Ljung-Box') # checks for of the model with data : says autcorre of residuals are zero when insign
round(accuracy(arima2),2)
checkresiduals(arima2)

#model3 3>2=1
arima3 = arima(mycars_ts, order=c(1,1,3))
arima3
Box.test(arima3$residuals, type='Ljung-Box') # checks for of the model with data : says autcorre of residuals are zero when insign
round(accuracy(arima3),2)
checkresiduals(arima3)

#model4 p=2 unacceptible as non-stationary/ so check p=4 ; 3 best 
arima4 = arima(mycars_ts, order=c(4,1,3))
arima4
Box.test(arima4$residuals, type='Ljung-Box') # checks for of the model with data : says autcorre of residuals are zero when insign
round(accuracy(arima4),2)
checkresiduals(arima4)

#model5
arima5 = arima(mycars_ts, order=c(1,0,1))
arima5
Box.test(arima5$residuals, type='Ljung-Box') # checks for of the model with data : says autcorre of residuals are zero when insign
round(accuracy(arima5),2)
checkresiduals(arima5)

# # checking for log transform
# adf.test(log(mycars_ts))# stationarity test - ok if sig
# ndiffs(log(mycars_ts))
# 
# auto.arima(log(mycars_ts)) # automatically gives pdq possible values
# acf(log(mycars_ts))
# pacf(log(mycars_ts))

# models check on residuals for fit and suitable:
# 1. normally distr about zero mean 2. autocorr of residuals be zero for every lag
#Evaluation
qqnorm(arima3$residuals)
qqline(arima3$residuals)

Box.test(car_r_arima$residuals, type='Ljung-Box') # checks for of the model with data : says autcorre of residuals are zero when insign
round(accuracy(car_r_arima),2)
checkresiduals(car_r_arima)

# Checking SARIMA models
arima2 = arima(car_regster, order=c(3,1,3), seasonal = c(1,2,2))
arima2
checkresiduals(arima2)

arima2 = arima(car_regster, order=c(3,1,4), seasonal = c(1,2,3))
checkresiduals(arima2)

arima = arima(car_regster, order=c(3,1,4), seasonal = c(1,2,4))
checkresiduals(arima2)
