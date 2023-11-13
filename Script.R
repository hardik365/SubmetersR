install.packages("tidyverse")
install.packages("lubridate")
install.packages("RMariaDB")

library(RMariaDB)
library(lubridate)
library(dplyr)
library(hablar)
library(plotly)
library(ggplot2)
library(ggfortify)
library(forecast)

## Create a database connection 
con = dbConnect(MariaDB(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')


dbListTables(con)

## Lists attributes contained in a table
dbListFields(con,'iris')
## Use asterisk to specify all attributes for download
irisALL <- dbGetQuery(con, "SELECT * FROM iris")

## Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

#Now lets do this for our project Data

dbListFields(con,'yr_2006')
#Downloading each dataset
yr_2006 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3  FROM yr_2006")
yr_2007 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3  FROM yr_2007")
yr_2008 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3  FROM yr_2008")
yr_2009 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3  FROM yr_2009")
yr_2010 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3  FROM yr_2010")

#Using str in each
str(yr_2006) 
str(yr_2007) 
str(yr_2008)
str(yr_2009) 
str(yr_2010) 

#using summary in each
summary(yr_2006)
summary(yr_2007)
summary(yr_2008)
summary(yr_2009)
summary(yr_2010)

#lets see the head and tail
head(yr_2006)
tail(yr_2006)
#I only see from 16 dec to 31 dec

head(yr_2007)
tail(yr_2007)

head(yr_2008)
tail(yr_2008)

head(yr_2009)
tail(yr_2009)

head(yr_2010)
tail(yr_2010)
#I only see data from 1st jan to 26 nov

#combining the data is for a complete year
masterData <- bind_rows(yr_2007, yr_2008, yr_2009)

str(masterData)
summary(masterData)


head(masterData)
tail(masterData)

## Combine Date and Time attribute values in a new attribute column
masterData <-cbind(masterData,paste(masterData$Date,masterData$Time), stringsAsFactors=FALSE)


## Give the new attribute in the 6th column a header name 
colnames(masterData)[6] <-"DateTime"

## Move the DateTime attribute within the dataset
masterData <- masterData[,c(ncol(masterData), 1:(ncol(masterData)-1))]
head(masterData)

## Convert DateTime from character to POSIXct 
masterData$DateTime <- as.POSIXct(masterData$DateTime, "%Y/%m/%d %H:%M:%S")

## Add the time zone
attr(masterData$DateTime, "tzone") <- "Europe/Paris"

## Inspect the data types
str(masterData)
#Values are not in the correct year, should have 2007, yet shows 1970

## Create "year" attribute with lubridate
masterData$year <- year(masterData$DateTime)
masterData$quarter <- quarter(masterData$DateTime)
masterData$month <- month(masterData$DateTime)
masterData$day <- day(masterData$DateTime)
masterData$hour <- hour(masterData$DateTime)
masterData$minute <- minute(masterData$DateTime)
masterData$week <- week(masterData$DateTime)
masterData$weekday <- wday(masterData$DateTime)


head(masterData)
summary(masterData)



write.csv(masterData, "masterData.csv", row.names=FALSE)
masterData = read.csv("masterData.csv") 

#General plot for submeter 1
plot(masterData$Sub_metering_1)


## Subset the second week of 2008 - All Observations
houseWeek <- filter(masterData, year == 2008 & week == 2)
## Plot subset houseWeek
plot(houseWeek$Sub_metering_1)

## Subset the 9th day of January 2008 - All observations
houseDay <- filter(masterData, year == 2008 & month == 1 & day == 9)
## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen (Submeter 1)', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room (Submeter 2)', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC (Submeter 3)', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(masterData, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen (Submeter 1)', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room (Submeter 2)', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC (Submeter 3)', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Plot subset houseWeek
plot(houseWeek$Sub_metering_1)

##week 2 plot better
plot_ly(houseWeek, x = ~houseWeek$DateTime, y = ~houseWeek$Sub_metering_1, name = 'Kitchen (Submeter 1)', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek$Sub_metering_2, name = 'Laundry Room (Submeter 2)', mode = 'lines') %>%
  add_trace(y = ~houseWeek$Sub_metering_3, name = 'Water Heater & AC (Submeter 3)', mode = 'lines') %>%
  layout(title = "Power Consumption of Week 2 in 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

##work on this
##houseweekFreq <- filter(masterData, year == 2008 & month == 1 & week == 2 & (hour == 0 | hour == 2 | hour == 4 | hour == 6 | hour == 8 | hour == 10  | hour == 12 | hour == 14 | hour == 16 | hour == 18 | hour == 20 || hour == 22))  

houseweekFreq <- filter(masterData, year == 2008 & month == 1 & week == 2 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

plot_ly(houseweekFreq, x = ~houseweekFreq$DateTime, y = ~houseweekFreq$Sub_metering_1, name = 'Kitchen (Submeter 1)', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweekFreq$Sub_metering_2, name = 'Laundry Room (Submeter 2)', mode = 'lines') %>%
  add_trace(y = ~houseweekFreq$Sub_metering_3, name = 'Water Heater & AC (Submeter 3)', mode = 'lines') %>%
  layout(title = "Power Consumption Week 2",
         xaxis = list(title = "Date"),
         yaxis = list (title = "Power (watt-hours)"))


houseyear <- filter(masterData, year == 2008  & hour == 12 & minute == 0)

plot_ly(houseyear, x = ~houseyear$DateTime,y = ~houseyear$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption (Water Heater & AC) for 2008",
         xaxis = list(title = "Date"),
         yaxis = list (title = "Power (watt-hours)"))



#plot_ly(houseyear, x = ~houseyear$DateTime, y = ~houseyear$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
 # add_trace(y = ~houseyear$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
#  add_trace(y = ~houseyear$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
 # layout(title = "Power Consumption for 2008",
#         xaxis = list(title = "Date"),
#         yaxis = list (title = "Power (watt-hours)"))

## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(masterData, weekday == 2 & hour == 20 & minute == 1)

## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, start=c(2007,1))

## Plot sub-meter 3 with autoplot (you may need to install these packages)

autoplot(tsSM3_070809weekly)

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")

## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly)

##filtering hourly time
houseSub1  <- filter(masterData, (minute == 0  | minute == 30)  & year == 2008 & week  == 2)
houseSub2 <- filter(masterData, (minute == 0  | minute == 30)  & year == 2008 & month == 2)

#time series for sub meter 1
tsSub1 <- filter(masterData,hour == 12, minute == 1)
ts_Sub1 <- ts(tsSub1$Sub_metering_1, frequency = 365, start=c(2007,1))
autoplot(ts_Sub1, ts.colour = 'blue', xlab = "Date", ylab = "Watt Hours", main = "Sub-meter 1")

#time series for submeter 2
tsSub2<-filter(masterData,hour == 12, minute == 1)
ts_Sub2 <- ts(tsSub1$Sub_metering_2, frequency = 365, start=c(2007,1))
autoplot(ts_Sub1, ts.colour = 'green', xlab = "Date", ylab = "Watt Hours", main = "Sub-meter 2")

#time series for submeter 1
plot_ly(houseSub1, x = ~houseSub1$DateTime, y = ~houseSub1$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
layout(title = "Power Consumption of Submeter 1, Week 2, Year 2008",
       xaxis = list(title = "Date"),
       yaxis = list (title = "Power (watt-hours)"))

#time series for submeter 2
plot_ly(houseSub2, x = ~houseSub2$DateTime, y = ~houseSub2$Sub_metering_2, name = 'Laundry Room', type = 'scatter', mode = 'lines') %>%
layout(title = "Power Consumption of Submeter 2, Feburary 2008",
       xaxis = list(title = "Date"),
       yaxis = list (title = "Power (watt-hours)"))

## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 



## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=20)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")

fitSM1 <- tslm(ts_Sub1 ~ trend + season)
forecastfitSM1 <- forecast(fitSM1, h = 20, level = c(80,90))
plot(forecastfitSM1, ylim = c(0, 40), ylab= "Watt-Hours", xlab="Time")


fitSM2 <- tslm(ts_Sub2 ~ trend + season)
forecastfitSM2 <- forecast(fitSM2, h = 20, level = c(80,90))
plot(forecastfitSM2, ylim = c(0, 70), ylab= "Watt-Hours", xlab="Time")

## Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
## Plot decomposed sub-meter 3 
plot(components070809SM3weekly)
## Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly)


compSm1 <- decompose(ts_Sub1)
plot(compSm1)
summary(compSm1)


compSm2 <- decompose(ts_Sub2)
plot(compSm2)
summary(compSm2)


## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))

## HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")

## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))


tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)
plot(decompose(tsSM3_070809Adjusted))
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))




ts_Sub1Adjusted <- ts_Sub1 - compSm1$seasonal
autoplot(ts_Sub1Adjusted)
plot(decompose(ts_Sub1Adjusted))
tsSM1_Holt <- HoltWinters(ts_Sub1Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM1_Holt, ylim = c(0, 40))
tsSM1_HoltFor <- forecast(tsSM1_Holt, h=25)
plot(tsSM1_HoltFor, ylim = c(0, 40), ylab= "Watt-Hours", xlab="Time - Sub-meter 1")
tsSM1_HoltForC <- forecast(tsSM1_Holt, h=25, level=c(10,25))
plot(tsSM1_HoltForC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1", start(2010))

ts_Sub2Adjusted <- ts_Sub2 - compSm2$seasonal
autoplot(ts_Sub2Adjusted)
plot(decompose(ts_Sub2Adjusted))
tsSM2_Holt <- HoltWinters(ts_Sub2Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM2_Holt, ylim = c(0, 25))
tsSM2_HoltFor <- forecast(tsSM2_Holt, h=25)
plot(tsSM2_HoltFor, ylim = c(0, 40), ylab= "Watt-Hours", xlab="Time - Sub-meter 2")
tsSM2_HoltForC <- forecast(tsSM2_Holt, h=25, level=c(10,25))
plot(tsSM2_HoltForC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2", start(2010))

