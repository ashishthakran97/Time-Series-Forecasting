require(forecast)

#importing the fie
file <- readxl::read_excel('E:/ANALYTIXLABS/uk outward data.xlsx')
file <- file[-c(1,42,43),]

#declare the data as time series data
myts   <- ts(file$Ireland, start=c(1996, 1), end=c(2005,4), frequency=4) 
plot(myts)
plot(decompose(myts, type = c("multiplicative")))

#fit$win  #what does it give? s  t l- 511 7 5

#auto.arima
fit <- auto.arima(myts)
accuracy(fit$fitted,myts)
forecast(myts,4)
accuracy(forecast(myts,4))
plot(forecast(myts,4))       #mape=2.44

#ets()
fitt <- ets(myts)
accuracy(fitt$fitted,myts)
forecast(myts,4)
accuracy(forecast(myts,4))

myts1  <- ts(file$`Other EU,not Ireland`, start=c(1996, 1), end=c(2005,4), frequency=4) 
plot(myts1)
plot(decompose(myts1, type = c("multiplicative")))

fit1 <- ets(myts1)
accuracy(fit1$fitted,myts1)
forecast(myts1,4)
accuracy(forecast(myts1,4))
plot(forecast(myts1,4))         #mape=2.25



myts2  <- ts(file$`Rest of Europe and Med`, start=c(1996, 1), end=c(2005,4), frequency=4) 
plot(myts2)
plot(decompose(myts2, type = c("multiplicative")))

fit2 <- ets(myts2)
accuracy(fit2$fitted,myts2)
forecast(myts2,4)
accuracy(forecast(myts2,4))
plot(forecast(myts2,4))      #mape=2.85



myts3  <- ts(file$`Rest of World`, start=c(1996, 1), end=c(2005,4), frequency=4) 
plot(myts3)
plot(decompose(myts3, type = c("multiplicative")))

fit3 <- ets(myts3)
accuracy(fit3$fitted,myts3)
forecast(myts3,4)
accuracy(forecast(myts3,4))
plot(forecast(myts3,4))         #mape=7.88
summary(fit3)



myts4  <- ts(file$Total, start=c(1996, 1), end=c(2005,4), frequency=4) 
plot(myts4)
plot(decompose(myts4, type = c("multiplicative")))

fit4 <- ets(myts4)
#fit4 <- auto.arima(myts4)
accuracy(fit4$fitted,myts4)
forecast(myts4,4)
accuracy(forecast(myts4,4))         
plot(forecast(myts4,4))      #mape=1.75
summary(fit4)


#f <- forecast(myts4,2)
#accuracy(f$fitted,myts4)








