#Baptiste Vert



#Apple stock data science class
library(GGally)
library("zoo")
library(timeDate)
library(xts)
library(ggplot2)
library(PerformanceAnalytics)
library(tseries)
library("ggpubr")
# Get quantmod
library(lubridate, warn.conflicts = FALSE)
library(fastmit)
library(plotly)
library(TTR)
library("forecast")
library(lubridate)
library(TSstudio)
library(ggplotify)
library(rstatix)
Apple_stock <- read.csv("C:/Users/33782/Downloads/AAPL.csv")
str(Apple_stock)


Apple_stock_close<-Apple_stock[,"Close"]
Apple_stock_open<-Apple_stock[,"Open"]
Apple_stock_close_return <-diff(Apple_stock_close)
Apple_stock_close_Log.return <-diff(log(Apple_stock_close))

#Just the numbers
apple_stock_close<- as.numeric(Apple_stock_close)
apple_stock_close_return<-as.numeric(Apple_stock_close_return)
apple_stock_close_Log.return<- as.numeric(Apple_stock_close_Log.return)

par(mfrow=c(3,1))
plot(apple_stock_close, main= 'Apple stock close price')
plot(apple_stock_close_return, main= 'Apple stock close return')
plot(apple_stock_close_Log.return, main= 'Apple stock close log-return')

#Second part:
x <- apple_stock_close_Log.return[-length(apple_stock_close_Log.return)] ## last value of xt cannot be used for x
y <- apple_stock_close_Log.return[-1]^2 ## first value of xt cannot be used for y
par(mfrow=c(1,1))
plot(x, y)

Apple_close.loess <-loess(y~x)
( dgf<- Apple_close.loess$trace.hat)
Apple_close.ss <- smooth.spline(x,y, df=dgf)
n<-length(x)

h<- 0.04
ox <- order(x) ## save the ordering of x
Apple_close.ks <- ksmooth(x, y, kernel="normal", bandwidth=h, x.points=x)
## reverse back to the original time ordering:
Apple_close.ks$x <- Apple_close.ks$x[order(ox)]
Apple_close.ks$y <- Apple_close.ks$y[order(ox)]
## NW evaluated at x_out
x_out <- seq(min(x), max(x), length = n)
Apple_close.ksp <- ksmooth(x, y, kernel = "normal", bandwidth=h, x.points = x_out)

## plot 1: whole range of data first
par(mfrow = c(2,1))
plot(x,y, main="Fitting results (whole range of data)",cex = 0.6, col = "gray")
lines(x_out,Apple_close.ks$y,col = 1) ## plot ksmooth fit
lines(x_out, predict(Apple_close.loess, newdata = data.frame(x = x_out)),lty = 3, col = 3, cex = 0.5) ## plot loess fit
lines(x_out, predict(Apple_close.ss, x = x_out)$y, lty = 5, col = 4) ## plot smoothing splines fit

legend('topright',legend = c("NW","lp","ss"),lty = c(1,3,5), col = c(1,3,4), cex = 0.6)
## plot 2: fits for central region of data
x_window <- quantile(x, c(0.2, 0.8))
y_window <- c(0, quantile(y, 0.95))
 plot(x, y, main="Fitting results (central region of data)", cex = 0.6, col = "gray",
       xlim = x_window, ylim=y_window)
lines(x_out, Apple_close.ksp$y, col = 1)
lines(x_out, predict(Apple_close.loess, newdata = data.frame(x = x_out)),
        lty = 3, col = 3, cex = 0.5)
lines(x_out, predict(Apple_close.ss, x = x_out)$y, lty = 5, col = 4)

par(mfrow = c(3,1))
plot.ts(y-Apple_close.ks$y, main = "Residuals: ksmooth", ylim = c(-0.02, 0.02))
plot.ts(y-fitted(Apple_close.loess), main = "Residuals: local polynomial", ylim = c(-0.02, 0.02))
plot.ts(y-predict(Apple_close.ss,x=x)$y, main="Residuals: smoothing splines", ylim = c(-0.02, 0.02))


Apple_stock_without_date <- Apple_stock[,-c(1)]
pairs(Apple_stock_without_date)
par(mfrow = c(3,2))
hist(Apple_stock_without_date[,"Close"])
hist(Apple_stock_without_date[,"Open"])
hist(Apple_stock_without_date[,"High"])
hist(Apple_stock_without_date[,"Low"])
hist(Apple_stock_without_date[,"Volume"])
attributes(Apple_stock["Date"])
summary(Apple_stock)



byear<-ymd(Apple_stock$Date)
byear.final <-year(byear)

Apple_stock$Year<-byear.final
Apple_stock.mean<-aggregate(cbind(Open, High, Low, Close, Adj.Close, Volume)~Year, data = Apple_stock,FUN = mean)



Apple_stock.mean_.data_ggp <- data.frame(x = Apple_stock.mean$Year,                            # Reshape data frame
                       y = c(Apple_stock.mean$Open, Apple_stock.mean$High, Apple_stock.mean$Low,Apple_stock.mean$Close,Apple_stock.mean$Adj.Close),
                       group = c(rep("Open", nrow(Apple_stock.mean)),
                                 rep("High", nrow(Apple_stock.mean)),
                                 rep("Low", nrow(Apple_stock.mean)),
                                 rep("Close", nrow(Apple_stock.mean)),
                                 rep("Adj.Close", nrow(Apple_stock.mean)))) 

Apple_stock.mean_ggp <- ggplot(Apple_stock.mean_.data_ggp, aes(x, y, col = group, shape= group)) + geom_point() + labs(x="Years", y="mean Apple's Stock per year")          # Create ggplot2 plot
Apple_stock.mean_ggp



Dates_Apple_stock<-as.Date(Apple_stock$Date)
Apple_stock.tserie <- xts(Apple_stock[-c(1,8)], Dates_Apple_stock)
dat <- aggregate(Apple_stock.tserie ~ year(index(Apple_stock.tserie)), FUN = quantile, probs = c(0.05, 0.95))
colnames(dat)[1] <- "Year"



getSymbols("AAPL",src='yahoo')
candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white")
addSMA(n = 200)
candleChart(Apple_stock.tserie, up.col = "black", dn.col = "red", theme = "white")


df <- data.frame(Date=index(Apple_stock.tserie),coredata(Apple_stock.tserie))
df <- tail(df, 30)


data_log<-cbind(diff(log(Apple_stock.tserie$Close)),diff(log(Apple_stock.tserie$Open)),diff(log(Apple_stock.tserie$Low)),diff(log(Apple_stock.tserie$High)))

data_normal<-cbind(Apple_stock.tserie$Close,Apple_stock.tserie$Open,Apple_stock.tserie$Low,Apple_stock.tserie$High)


par(mfrow= c(1,2))
chart.Correlation(data_normal)
chart.Correlation(data_log)

acf(Apple_stock.tserie$Close, main = "Auto-covariance function of X")
ts.plot(diff(Apple_stock.tserie$Close))

SMA_Apple_stock_close_price<-SMA(Apple_stock.tserie$Close,n=200)
SMA_Apple_stock_close_price
plot.ts(SMA_Apple_stock_close_price)

Apple_stock_close.forecast <- HoltWinters(Apple_stock.tserie$Close, beta=FALSE, gamma=FALSE)
Apple_stock_close.forecast
plot(Apple_stock_close.forecast)



Apple_stock_close.forecasts2 <- forecast(Apple_stock_close.forecast, h=8)
Apple_stock_close.forecasts2

plot.ts(Apple_stock_close.forecasts2$residuals)
xlims <- c(floor_date(as.Date("2012-12-12"), "day"), ceiling_date(as.Date("2013-12-12"), "day"))
HW1.pred <- predict(Apple_stock_close.forecast, 744, prediction.interval = TRUE, level=0.95)
#Visually evaluate the prediction
xlims

lims<-as.POSIXct(strptime(c("2017-02-23", "2017-02-24"),format = "%Y-%m-%d "))
plot(Apple_stock.tserie$Close, ylab="Apple's stock close price",xlim=lims)
lines(Apple_stock_close.forecast$fitted[,1], lty=2, col="blue")
lines(HW1.pred[,1], col="red")
lines(HW1.pred[,2], lty=2, col="orange")
lines(HW1.pred[,3], lty=2, col="orange")


#res<-mi.test(Apple_stock.tserie$Open,Apple_stock.tserie$Close)

#Here we will work with the close price of Apple stock
Apple_close<-Apple_stock.tserie$Close
first_plot_close<- autoplot(Apple_close) +
  ggtitle("Apple's stock close price") +
  xlab("Year") +
  ylab("close price")

first_plot_close

Apple_close_first_period = window(Apple_stock.tserie$Close, start="1980-12-12", end="2003-12-12")
Apple_close_second_period = window(Apple_stock.tserie$Close, start="2004-01-01", end="2022-06-17")
par(mfrow= c(1,2))
plot(Apple_close_first_period)
plot(Apple_close_second_period)


Apple_close.seasonplot<-ggseasonplot(xts_to_ts(Apple_close_second_period, frequency = NULL, start = NULL), year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$") +
  ggtitle("Seasonal plot: Close price of Apple's stock")

Apple_close.seasonplot

Apple_close.seasonplot2<-ggseasonplot(xts_to_ts(Apple_close_second_period, frequency = NULL, start = NULL), polar = TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: Close price of Apple's stock")
Apple_close.seasonplot2

First_basic_autoplot<-autoplot(Apple_stock.tserie[,-c(6,5)], facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Apple's stock price")
First_basic_autoplot

#Here we study the correlation between the different prices of Apple's stock
GGally::ggpairs(as.data.frame(Apple_stock.tserie))

#Here we will compute the autocorrelation of the different prices
par(mfrow= c(1,2))
result_acf_close=acf(Apple_close,lag.max = 200)
result_acf_close$lag
result_pacf_close=pacf(Apple_close)



#Make the timeseries stationnary
dff<-adf.test(Apple_close) # p-value < 0.05 indicates the TS is stationary
kpss.test(Apple_close)
ndiffs(Apple_close)
stationaryTS.Apple_close <- diff(Apple_close, differences= 2)
plot(stationaryTS.Apple_close, type="l", main="Differenced and Stationary")

dss<-kpss.test(stationaryTS.Apple_close)

#compute the moving average with non-split data

#Split the data
n <- 11

#split data frame into n equal-sized data frames
Splitting_Apple_stock1<-split(Apple_stock, factor(sort(rank(row.names(Apple_stock))%%n)))
Splitting_Apple_stock2<-split(Apple_stock, factor(sort(rank(row.names(Apple_stock))%%n)))


#can be used to work with characters
c<-as.character(3)
c
cat(c, "\n")
# ABC 
ds<-sQuote(c, "useFancyQuotes")
g<-noquote(ds)

Dates_Apple_stock_split<-as.Date(Splitting_Apple_stock1$'0'$Date)
Splitting_Apple_stock2$'0'<- xts(Splitting_Apple_stock1$'0'[-c(1,8)], Dates_Apple_stock_split)

Dates_Apple_stock_split<-as.Date(Splitting_Apple_stock1$'1'$Date)
Splitting_Apple_stock2$'1'<- xts(Splitting_Apple_stock1$'1'[-c(1,8)], Dates_Apple_stock_split)

Dates_Apple_stock_split<-as.Date(Splitting_Apple_stock1$'2'$Date)
Splitting_Apple_stock2$'2'<- xts(Splitting_Apple_stock1$'2'[-c(1,8)], Dates_Apple_stock_split)

Dates_Apple_stock_split<-as.Date(Splitting_Apple_stock1$'3'$Date)
Splitting_Apple_stock2$'3'<- xts(Splitting_Apple_stock1$'3'[-c(1,8)], Dates_Apple_stock_split)

Dates_Apple_stock_split<-as.Date(Splitting_Apple_stock1$'4'$Date)
Splitting_Apple_stock2$'4'<- xts(Splitting_Apple_stock1$'4'[-c(1,8)], Dates_Apple_stock_split)

Dates_Apple_stock_split<-as.Date(Splitting_Apple_stock1$'5'$Date)
Splitting_Apple_stock2$'5'<- xts(Splitting_Apple_stock1$'5'[-c(1,8)], Dates_Apple_stock_split)

Dates_Apple_stock_split<-as.Date(Splitting_Apple_stock1$'6'$Date)
Splitting_Apple_stock2$'6'<- xts(Splitting_Apple_stock1$'6'[-c(1,8)], Dates_Apple_stock_split)

Dates_Apple_stock_split<-as.Date(Splitting_Apple_stock1$'7'$Date)
Splitting_Apple_stock2$'7'<- xts(Splitting_Apple_stock1$'7'[-c(1,8)], Dates_Apple_stock_split)

Dates_Apple_stock_split<-as.Date(Splitting_Apple_stock1$'8'$Date)
Splitting_Apple_stock2$'8'<- xts(Splitting_Apple_stock1$'8'[-c(1,8)], Dates_Apple_stock_split)

Dates_Apple_stock_split<-as.Date(Splitting_Apple_stock1$'9'$Date)
Splitting_Apple_stock2$'9'<- xts(Splitting_Apple_stock1$'9'[-c(1,8)], Dates_Apple_stock_split)

Dates_Apple_stock_split<-as.Date(Splitting_Apple_stock1$'10'$Date)
Splitting_Apple_stock2$'10'<- xts(Splitting_Apple_stock1$'10'[-c(1,8)], Dates_Apple_stock_split)

list.Apple_stock_Splitting<-list(Splitting_Apple_stock2$'1',Splitting_Apple_stock2$'2',Splitting_Apple_stock2$'3',Splitting_Apple_stock2$'4',Splitting_Apple_stock2$'5',Splitting_Apple_stock2$'6',Splitting_Apple_stock2$'7',Splitting_Apple_stock2$'8',Splitting_Apple_stock2$'9',Splitting_Apple_stock2$'10')

#Let us make the split data stationary

adf.test_split_Apple_stock<-c()
kpss.test_split_Apple_stock<-c()
ndiffs_split_Apple_stock<-c()
list_stationaryTS.Apple_close<-list()
par(mfrow= c(4,3))
for (i in 1:length(list.Apple_stock_Splitting)){
  adf.test_split_Apple_stock[i]<-adf.test(list.Apple_stock_Splitting[[i]]$Close)["p.value"] # p-value < 0.05 indicates the TS is stationary
  kpss.test_split_Apple_stock[i]<-kpss.test(list.Apple_stock_Splitting[[i]]$Close)["p.value"]
  ndiffs_split_Apple_stock[i]<-ndiffs(list.Apple_stock_Splitting[[i]]$Close)
  list_stationaryTS.Apple_close[[i]] <- diff(list.Apple_stock_Splitting[[i]]$Close, differences=ndiffs_split_Apple_stock[i] )
  plot(list_stationaryTS.Apple_close[[i]], type="l", main="Differenced and Stationary")
}
adf.test_split_Apple_stock_p.value<-as.numeric(adf.test_split_Apple_stock)
kpss.test_split_Apple_stock_p.value<-as.numeric(kpss.test_split_Apple_stock)
par(mfrow= c(4,3))
plot(list_stationaryTS.Apple_close[[1]], type="l", main="Differenced and Stationary")
plot(list_stationaryTS.Apple_close[[2]], type="l", main="Differenced and Stationary")
plot(list_stationaryTS.Apple_close[[3]], type="l", main="Differenced and Stationary")
plot(list_stationaryTS.Apple_close[[4]], type="l", main="Differenced and Stationary")
plot(list_stationaryTS.Apple_close[[5]], type="l", main="Differenced and Stationary")
plot(list_stationaryTS.Apple_close[[6]], type="l", main="Differenced and Stationary")
plot(list_stationaryTS.Apple_close[[7]], type="l", main="Differenced and Stationary")
plot(list_stationaryTS.Apple_close[[8]], type="l", main="Differenced and Stationary")
plot(list_stationaryTS.Apple_close[[9]], type="l", main="Differenced and Stationary")
plot(list_stationaryTS.Apple_close[[10]], type="l", main="Differenced and Stationary")
plot(adf.test_split_Apple_stock_p.value)
plot(adf.test_split_Apple_stock_p.value_2)

#Computation of the autocorrelation and partial autocorrelation

acf.split_Apple_Stock<-c()
pacf.split_Apple_Stock<-c()
par(mfrow= c(5,4))
for(i in 1:length(list.Apple_stock_Splitting)){
  acf.split_Apple_Stock[i]<-acf(list.Apple_stock_Splitting[[i]]$Close,lag.max = 200)
  pacf.split_Apple_Stock[i]<-pacf(list.Apple_stock_Splitting[[i]]$Close)
}

#Computation of the moving average 
moving_average.Split_Apple_stock<-list()
for(i in 1:length(list.Apple_stock_Splitting)){
  moving_average.Split_Apple_stock[[i]]<-rollmean(list.Apple_stock_Splitting[[i]]$Close, k = 5)
}



#Let us take a look at the correlation between the variables with our split data
data_normal_.Split_Apple_stock<-list()
data_log_.Split_Apple_stock<-list()

for(i in 1:length(list.Apple_stock_Splitting)){
  data_normal_.Split_Apple_stock[[i]]<-cbind(list.Apple_stock_Splitting[[i]]$Close,list.Apple_stock_Splitting[[i]]$Open,list.Apple_stock_Splitting[[i]]$Low,list.Apple_stock_Splitting[[i]]$High)
  data_log_.Split_Apple_stock[[i]]<-cbind(diff(log(list.Apple_stock_Splitting[[i]]$Close)),diff(log(list.Apple_stock_Splitting[[i]]$Open)),diff(log(list.Apple_stock_Splitting[[i]]$Low)),diff(log(list.Apple_stock_Splitting[[i]]$High)))
}
par(mfrow=c(4,4))

chart.Correlation(data_normal_.Split_Apple_stock[[1]])
chart.Correlation(data_log_.Split_Apple_stock[[1]],method = "pearson")
chart.Correlation(data_normal_.Split_Apple_stock[[2]])
chart.Correlation(data_log_.Split_Apple_stock[[2]])
chart.Correlation(data_normal_.Split_Apple_stock[[9]])
chart.Correlation(data_log_.Split_Apple_stock[[9]])
chart.Correlation(data_normal_.Split_Apple_stock[[10]])
chart.Correlation(data_log_.Split_Apple_stock[[10]])


Season_plot.Split_Apple_stock<-list()
par(mfrow=c(4,3))
for(i in 1:length(list.Apple_stock_Splitting)){
   Season_plot.Split_Apple_stock[[i]]<-ggseasonplot(xts_to_ts(list.Apple_stock_Splitting[[i]]$Close, frequency = NULL, start = NULL), year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: Close price of Apple's stock" )
   Season_plot.Split_Apple_stock[[i]]
}
Split_number <- c("1", "2", "3", "4", "5", "6","7","8","9","10")

# create a list with a specific length 
season_plot_lst <- vector("list", length = length(list.Apple_stock_Splitting))

for (i in 1:length(list.Apple_stock_Splitting)) {
  season_plot_lst[[i]] <- Season_plot.Split_Apple_stock[[i]]
}

# Combine all plots
cowplot::plot_grid(plotlist = season_plot_lst, nrow = 5)



#Create the first plot with the split data
first_plot_lst <- vector("list", length = length(list.Apple_stock_Splitting))

for (i in 1:4) {
  first_plot_lst[[i]] <- autoplot(list.Apple_stock_Splitting[[i]][,-c(6)]) +
    ggtitle("Apple's stock  prices") +
    xlab("Year") +
    ylab("stock prices")
}

# Combine all plots
cowplot::plot_grid(plotlist = season_plot_lst, nrow = 5)
cowplot::plot_grid(plotlist = first_plot_lst, nrow = 2)

autoplot(list.Apple_stock_Splitting[[6]][,-c(6)])
list.Apple_stock_Splitting[[6]][,-c(6)]

#let us work with the log-transformed data
shapiro.test(data_log_.Split_Apple_stock[[1]])

ddw<-as.data.frame(as.numeric(data_log_.Split_Apple_stock[[1]]$Close))[-1,]
shapiro.test(ddw)
ggqqplot(ddw)


test_normality.data_log_.Split_Apple_stock<-c()
test_normality.data_log_.Split_Apple_stock$"1"<-as.data.frame(as.numeric(data_log_.Split_Apple_stock[[1]]$Close))[-1,]
test_normality.data_log_.Split_Apple_stock$"3"<-as.data.frame(as.numeric(data_log_.Split_Apple_stock[[3]]$Close))[-1,]
test_normality.data_log_.Split_Apple_stock$"5"<-as.data.frame(as.numeric(data_log_.Split_Apple_stock[[5]]$Close))[-1,]
test_normality.data_log_.Split_Apple_stock$"7"<-as.data.frame(as.numeric(data_log_.Split_Apple_stock[[7]]$Close))[-1,]
test_normality.data_log_.Split_Apple_stock$"9"<-as.data.frame(as.numeric(data_log_.Split_Apple_stock[[9]]$Close))[-1,]
test_normality.data_log_.Split_Apple_stock$"10"<-as.data.frame(as.numeric(data_log_.Split_Apple_stock[[10]]$Close))[-1,]
plot(test_normality.data_log_.Split_Apple_stock$"10")

chat<-c()
chat<-append(chat,shapiro.test(test_normality.data_log_.Split_Apple_stock$"1")["p.value"])
chat<-append(chat,shapiro.test(test_normality.data_log_.Split_Apple_stock$"3")["p.value"])
chat<-append(chat,shapiro.test(test_normality.data_log_.Split_Apple_stock$"5")["p.value"])
chat<-append(chat,shapiro.test(test_normality.data_log_.Split_Apple_stock$"7")["p.value"])
chat<-append(chat,shapiro.test(test_normality.data_log_.Split_Apple_stock$"9")["p.value"]) 
chat<-append(chat,shapiro.test(test_normality.data_log_.Split_Apple_stock$"10")["p.value"])
 

par(mfrow=c(3,2))
ggqqplot(test_normality.data_log_.Split_Apple_stock$"1")
ggqqplot(test_normality.data_log_.Split_Apple_stock$"3")
ggqqplot(test_normality.data_log_.Split_Apple_stock$"5")
ggqqplot(test_normality.data_log_.Split_Apple_stock$"7")
ggqqplot(test_normality.data_log_.Split_Apple_stock$"9")
ggqqplot(test_normality.data_log_.Split_Apple_stock$"10")


first_plot_lst_2 <- vector("list", length = 6)


first_plot_lst_2[[1]] <- ggqqplot(test_normality.data_log_.Split_Apple_stock$"1")
first_plot_lst_2[[2]] <- ggqqplot(test_normality.data_log_.Split_Apple_stock$"3")
first_plot_lst_2[[3]] <- ggqqplot(test_normality.data_log_.Split_Apple_stock$"5")
first_plot_lst_2[[4]] <- ggqqplot(test_normality.data_log_.Split_Apple_stock$"7")
first_plot_lst_2[[5]] <- ggqqplot(test_normality.data_log_.Split_Apple_stock$"9")
first_plot_lst_2[[6]] <- ggqqplot(test_normality.data_log_.Split_Apple_stock$"10")

cowplot::plot_grid(plotlist = first_plot_lst_2, nrow = 3)
acf.split_Apple_Stock_log<-c()
pacf.split_Apple_Stock_log<-c()
par(mfrow= c(2,2))
for(i in 1:4){
  acf.split_Apple_Stock_log[i]<-acf(data_log_.Split_Apple_stock[[i]]$Close[-1,],lag.max = 200)
}
acf.split_Apple_Stock_log[i]<-acf(data_log_.Split_Apple_stock[[1]]$Close,lag.max = 200)
data_log_.Split_Apple_stock[[4]]$Close[-1,]


adf.test_split_Apple_stock_log<-c()
adf.test_split_Apple_stock_log_2<-c()
kpss.test_split_Apple_stock_log<-c()
ndiffs_split_Apple_stock_log<-c()
list_stationaryTS.Apple_close_log<-list()

for (i in 1:length(list.Apple_stock_Splitting)){
  adf.test_split_Apple_stock_log[i]<-adf.test(data_log_.Split_Apple_stock[[i]]$Close[-1,])["p.value"] # p-value < 0.05 indicates the TS is stationary
  kpss.test_split_Apple_stock_log[i]<-kpss.test(data_log_.Split_Apple_stock[[i]]$Close[-1,])["p.value"]
  ndiffs_split_Apple_stock_log[i]<-ndiffs(data_log_.Split_Apple_stock[[i]]$Close[-1,])
  list_stationaryTS.Apple_close_log[[i]] <- diff(data_log_.Split_Apple_stock[[i]]$Close[-1,], differences=1 )
  plot(list_stationaryTS.Apple_close_log[[i]], type="l", main="Differenced and Stationary")
}
kpss.test_split_Apple_stock_log
par(mfrow= c(1,2))
adf.test_split_Apple_stock_p.value_log<-as.numeric(adf.test_split_Apple_stock_log)
kpss.test_split_Apple_stock_p.value_log<-as.numeric(kpss.test_split_Apple_stock_log)
plot(adf.test_split_Apple_stock_p.value_log)
plot(kpss.test_split_Apple_stock_p.value_log)
for (i in 1:length(list.Apple_stock_Splitting)){
  adf.test_split_Apple_stock_log_2[i]<-adf.test(data_log_.Split_Apple_stock[[i]]$Close[-1,])["p.value"] # p-value < 0.05 indicates the TS is stationary
}
adf.test_split_Apple_stock_p.value_log_2<-as.numeric(adf.test_split_Apple_stock_log_2)
plot(adf.test_split_Apple_stock_p.value_log_2)

adf.test_split_Apple_stock_2<-c()
kpss.test_split_Apple_stock_2<-c()
ndiffs_split_Apple_stock<-c()
list_stationaryTS.Apple_close<-list()
par(mfrow= c(4,3))
for (i in 1:length(list.Apple_stock_Splitting)){
  adf.test_split_Apple_stock[i]<-adf.test(list.Apple_stock_Splitting[[i]]$Close)["p.value"] # p-value < 0.05 indicates the TS is stationary
  kpss.test_split_Apple_stock[i]<-kpss.test(list.Apple_stock_Splitting[[i]]$Close)["p.value"]
  ndiffs_split_Apple_stock[i]<-ndiffs(list.Apple_stock_Splitting[[i]]$Close)
  list_stationaryTS.Apple_close[[i]] <- diff(list.Apple_stock_Splitting[[i]]$Close, differences=ndiffs_split_Apple_stock[i] )
  plot(list_stationaryTS.Apple_close[[i]], type="l", main="Differenced and Stationary")
  adf.test_split_Apple_stock_2[i]<-adf.test(list_stationaryTS.Apple_close[[i]][-1,])["p.value"] # p-value < 0.05 indicates the TS is stationary
  kpss.test_split_Apple_stock_2[i]<-kpss.test(list_stationaryTS.Apple_close[[i]][-1,])["p.value"]
}
par(mfrow= c(1,2))
adf.test_split_Apple_stock_2
adf.test_split_Apple_stock_p.value_2<-as.numeric(adf.test_split_Apple_stock_2)
kpss.test_split_Apple_stock_p.value_2<-as.numeric(kpss.test_split_Apple_stock_2)
kpss.test_split_Apple_stock_p.value_2
plot(kpss.test_split_Apple_stock_p.value_2)
plot(adf.test_split_Apple_stock_p.value_2)