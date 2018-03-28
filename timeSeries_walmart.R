#**********************************************************************************
# Problem Statement - Walmart Sales Prediction.
# Method Used       - Time Series
#**********************************************************************************

#install.packages('forecast')
library(forecast)
#install.packages('tseries')
library(tseries)
#install.packages('zoo')
library(zoo)
#install.packages('nortest')
library(nortest)


# Loading the train.csv file
# Set the path as required
dat_train <- read.csv("C:/Users/pmutreja/Downloads/train.csv", header = T)

print("Welcome to the Walmart Sales Predictor")

#Accepting User input for the model
svar <- readline(prompt = "Enter the Store Number (1-45): ")
pvar <- readline(prompt = "Enter the Number of predictions to be made: ")
#OR use below inputs by unmcommenting 
#svar <- 1
#pvar <- 2


svardf <- as.data.frame(svar)
colnames(svardf) <- c("Store")

cat('Extracting data for Store: ',svar)
store_dat <- merge(dat_train, svardf, by=c("Store"))

fit_ts <- function(dat){
#***********************************************************************
# Function to process and Clean data to remove negative and zero values
# and fit a seasonal arima Model.
#
# Args:
#    dat: an atomic variable containing the name of the data variable
#
# Returns:
#    the arima model
#***********************************************************************
  
  reqd.dat <- as.data.frame(get(dat))
  for (j in 1:nrow(reqd.dat)) {
    if (reqd.dat[j,4] < 0) {
      reqd.dat$Weekly_Sales[reqd.dat$Weekly_Sales <= 0] = median(reqd.dat$Weekly_Sales)
    }
  }
  logSales <- log10(reqd.dat$Weekly_Sales)
  return(Arima(logSales, order = c(6,0,0), seasonal = list(order =c(1,1,0), period = 52), method ="CSS"))
}

final.pred <- NULL
final.file <- NULL

# list of available Departments in Stores
uniqsd <- unique(store_dat[c("Store","Dept")])
for (i in 1:nrow(uniqsd)) {
    varnam <- paste("s",svar,"d",uniqsd[i,2], sep = "")
    assign(varnam, merge(store_dat, uniqsd[i,], by=c("Store","Dept")))
    
    if(nrow(get(varnam)) < 104){
      cat("Insufficient data for Department: ",uniqsd[i,2])
    }
    else{
      varfit <- paste("ars",svar,"d",uniqsd[i,2],sep = "")
      assign(varfit, fit_ts(varnam))
      varpred <- paste("pred",svar,"d",i,sep = "")
      assign(varpred, forecast(get(varfit), h=pvar))
      pred <- get(varpred)
      #summary(pred)
      pred$x <- 10^pred$x
      pred$mean <- 10^pred$mean
      #pred$lower <- 10^pred$lower
      #pred$upper <- 10^pred$upper
      final.pred <- data.frame(Store = rep(svar,pvar), Dept = rep(uniqsd[i,2],pvar), Forecasted = as.numeric(pred$mean ))
      final.file <- rbind(as.data.frame(final.file), final.pred)
    }
}

# Save the output in a file
#options(max.print = 5.5E5)
#z <- gzfile("submission.csv.gz")
#write.csv(final.file, z)

######### PLOTTING STARTS HERE ############

#Viewing dataset and its summary
head(dat_train)
#View(dat_train)
summary(dat_train)

#Data Visualization
#Plotting weekly sales
plot(dat_train$Weekly_Sales, xlim=c(0,140), ylim = c(0,60000), ylab = 'weekly sales', xlab = 'week')

#Distribution of sales without any transformation
hist(dat_train$Weekly_Sales)

#Checking stationarity by plotting sales time series
plot.ts(dat_train$Weekly_Sales, xlim=c(0,140), ylim = c(0,60000), ylab = 'weekly sales', xlab = 'time')

#Time series plot for store1 dptmnt 1
plot.ts(s1d1$Weekly_Sales, xlim=c(0,140), ylim = c(10000,60000), ylab = 'store 1 dpt 1 weekly sales', xlab = 'time')

#Time series plot for store1 deptmnt 2
plot.ts(s1d2$Weekly_Sales, xlim=c(0,140), ylim = c(40000,60000), ylab = 'store 1 dpt 2 weekly sales', xlab = 'time')

#Time series plot for store1 deptmnt 93
plot.ts(s1d93$Weekly_Sales, xlim=c(0,140), ylim = c(50000,95000), ylab = 'store 1 dpt 93 weekly sales', xlab = 'time')

#Treating 0 and negative values to prevent NAs due to log
dat_train$Weekly_Sales[dat_train$Weekly_Sales <= 0] = median(dat_train$Weekly_Sales)

#Tranforming data and taking log
logSales <- log10(dat_train$Weekly_Sales)

#Distribution of sales with log transformation
hist(logSales)

#Time Series plots after log transformation
plot.ts(log10(s1d1$Weekly_Sales), xlim=c(0,140), ylim = c(4.0,4.8), ylab = 'log(s1d1 weekly sales)', xlab = 'Time')

plot.ts(log10(s1d2$Weekly_Sales), xlim=c(0,140), ylim = c(4.55,4.8), ylab = 'log(s1d2 weekly sales)', xlab = 'Time')

plot.ts(log10(s1d93$Weekly_Sales), xlim=c(0,140), ylim = c(4.75,5.0), ylab = 'log(s1d93 weekly sales)', xlab = 'Time')

#differncing for store 1 dept 1
temps1d1 <- s1d1[order(s1d1$Date),c("Weekly_Sales")]
tss1d1 <- ts(log10(temps1d1))
datdiffs1d1 <- diff(tss1d1, differences = 52)
plot.ts(datdiffs1d1)

#differncing for store 1 dept 2
temps1d2 <- s1d2[order(s1d2$Date),c("Weekly_Sales")]
tss1d2 <- ts(log10(temps1d2))
datdiffs1d2 <- diff(tss1d2, differences = 52)
plot.ts(datdiffs1d2)

#differncing for store 1 dept 93
temps1d93 <- s1d93[order(s1d93$Date),c("Weekly_Sales")]
tss1d93 <- ts(log10(temps1d93))
datdiffs1d93 <- diff(tss1d93, differences = 52)
plot.ts(datdiffs1d93)

#Analyzing acf & pcaf plots
acf(temps1d1, lag.max = 140)
pacf(temps1d1, lag.max = 140)
acf(temps1d2, lag.max = 140)
pacf(temps1d2, lag.max = 140)
acf(temps1d93, lag.max = 140)
pacf(temps1d93, lag.max = 140)

#AFTER applying ARIMA
acf(ars1d1$residuals, xlim=c(0,140))
pacf(ars1d1$residuals, xlim=c(0,140))
acf(ars1d2$residuals, xlim=c(0,140))
pacf(ars1d2$residuals, xlim=c(0,140))
acf(ars1d93$residuals, xlim=c(0,140))
pacf(ars1d93$residuals, xlim=c(0,140))

#Forecasting 
plot(pred1d1)
plot(pred1d2)
plot(pred1d3)





