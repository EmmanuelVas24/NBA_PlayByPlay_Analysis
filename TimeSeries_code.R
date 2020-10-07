

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")

str(births)
head(births)

#Frequency = 12 for number of months
births_ts <- ts(births, frequency = 12, start = c(1946, 1))

str(births_ts)
head(births_ts)

births
births_ts

plot.ts(births_ts)

#Decomposing Seasonal Data

births_decomposed <- decompose(births_ts)
plot(births_decomposed)

#If you have a seasonal time series, you can seasonally adjust the series by estimating the seasonal component, and subtracting it from the original time series.
#We can see now that time time series simply consists of the trend and random components.
birthsSeasonAdj <- births_ts - births_decomposed$seasonal
plot(birthsSeasonAdj)

#-----------------
gift <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
gift_ts<- ts(gift, frequency=12, start=c(1987,1))

plot.ts(gift_ts)


#In this case, an additive model is not appropriate since the size of the seasonal and random fluctuations change over time 
#and the level of the time series. It is then appropriate to transform the time series so that we can model the data with a classic additive model


logGift <- log(gift_ts)
plot.ts(logGift)


#------------------
kings<-scan('http://robjhyndman.com/tsdldata/misc/kings.dat', skip=3)
kings

kings_ts <- ts(kings)
kings_ts

plot.ts(kings_ts)


#Decomposing non-Seasonal Data

#non-seasonal time series consist of a trend component and a random component. 
#Decomposing the time series involves tying to separate the time series into these individual components.

#smoothing method: simple moving average SMA


#install.packages("TTR")
library(TTR)

kingsSMA8 <- SMA(kings_ts, n=8)
plot.ts(kingsSMA8)



summary.aov(kingsSMA8)


#autocorrelation is the similarity between observations as a function of the time lag between them.


























