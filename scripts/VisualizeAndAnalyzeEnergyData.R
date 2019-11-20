####### IoT analytics - Task 2
####### Visualize and analyze energy data

#### Install and load packages and set seed ####
if (require('pacman') == 'FALSE') {
  install.packages('pacman')
}

pacman::p_load(readr, 
               caret, 
               dplyr,
               tidyr,
               ggplot2, 
               DataExplorer, 
               RMySQL,
               lubridate,
               plotly,
               ggfortify,
               forecast,
               jsonlite,
               reshape2)
set.seed(123)

#### Conect to MySQL ####
con = dbConnect(MySQL(), 
                user='deepAnalytics', 
                password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
dbListTables(con)
dbListFields(con, 'yr_2006')

#### Create dataframes ####
yr_2006 <- dbGetQuery(con, 
                      'select * from yr_2006')
yr_2007 <- dbGetQuery(con, 
                      'select * from yr_2007')
yr_2008 <- dbGetQuery(con, 
                      'select * from yr_2008')
yr_2009 <- dbGetQuery(con, 
                      'select * from yr_2009')
yr_2010 <- dbGetQuery(con, 
                      'select * from yr_2010')

#### Get to know the data ####
# 2006 - After 2006-12-16 17:24
str(yr_2006)
summary(yr_2006)
head(yr_2006)
tail(yr_2006)

# 2007
str(yr_2007)
summary(yr_2007)
head(yr_2007)
tail(yr_2007)

# 2008
str(yr_2008)
summary(yr_2008)
head(yr_2008)
tail(yr_2008)

# 2009
str(yr_2009)
summary(yr_2009)
head(yr_2009)
tail(yr_2009)

# 2010 - Until 2010-11-26 21:02
str(yr_2010)
summary(yr_2010)
head(yr_2010)
tail(yr_2010)

#### Transform the data ####
# Create a new data frame with the information about the entire year of 2007, 2008 and 2009
entireYear <- bind_rows(yr_2007, 
                        yr_2008, 
                        yr_2009)
str(entireYear)
summary(entireYear)
head(entireYear)
tail(entireYear)

# Create a new column with date and time
entireYear$dateTime <- paste(entireYear$Date, 
                             entireYear$Time)
entireYear <- entireYear[, c(ncol(entireYear), 
                             1:(ncol(entireYear)-1))]
entireYear$dateTime <- as.POSIXct(entireYear$dateTime, 
                                  '%Y-%m-%d %H:%M:%S')
attr(entireYear$dateTime, 'tzone') <- 'UTC'
str(entireYear)

# Create attributes for year, quarter, month, week, weekday, day, hour and minute
entireYear$year <- year(entireYear$dateTime)
entireYear$quarter <- quarter(entireYear$dateTime)
entireYear$month <- month(entireYear$dateTime)
entireYear$week <- week(entireYear$dateTime)
entireYear$weekday <- weekdays(entireYear$dateTime)
entireYear$day <- day(entireYear$dateTime)
entireYear$hour <- hour(entireYear$dateTime)
entireYear$minute <- minute(entireYear$dateTime)

summary(entireYear)

# Create and treat data frame with all data
completeData <- bind_rows(yr_2007,
                          yr_2008,
                          yr_2009,
                          yr_2010)
completeData$dateTime <- paste(completeData$Date, 
                               completeData$Time)
completeData <- completeData[, c(ncol(completeData), 
                             1:(ncol(completeData)-1))]
completeData$dateTime <- as.POSIXct(completeData$dateTime, 
                                  '%Y-%m-%d %H:%M:%S')
attr(completeData$dateTime, 'tzone') <- 'UTC'
completeData$year <- year(completeData$dateTime)
completeData$quarter <- quarter(completeData$dateTime)
completeData$month <- month(completeData$dateTime)
completeData$week <- week(completeData$dateTime)
completeData$weekday <- weekdays(completeData$dateTime)
completeData$day <- day(completeData$dateTime)
completeData$hour <- hour(completeData$dateTime)
completeData$minute <- minute(completeData$dateTime)

completePerMonthGlobal <- completeData %>%
  select(year, month, Global_active_power) %>%
  group_by(year, month) %>%
  summarise(Global = sum(Global_active_power, na.rm = T)/60)

completePerWeekGlobal <- completeData %>%
  select(year, week, Global_active_power) %>%
  group_by(year, week) %>%
  summarise(Global = sum(Global_active_power, na.rm = T)/60)
View(completePerWeekGlobal)

completePerWeek <- completeData %>%
  select(year, 
         week, 
         Global_active_power, 
         Sub_metering_1, 
         Sub_metering_2, 
         Sub_metering_3) %>%
  group_by(year, week) %>%
  summarise(Global = sum(Global_active_power, na.rm = T)/60,
            S1 = sum(Sub_metering_1, na.rm = T)/1000,
            S2 = sum(Sub_metering_2, na.rm = T)/1000,
            S3 = sum(Sub_metering_3, na.rm = T)/1000,
            other = (Global - S1 - S2 - S3))
View(completePerWeek)

completePerMonth <- completeData %>%
  select(year, 
         month, 
         Global_active_power, 
         Sub_metering_1, 
         Sub_metering_2, 
         Sub_metering_3) %>%
  group_by(year, month) %>%
  summarise(Global = sum(Global_active_power, na.rm = T)/60,
            S1 = sum(Sub_metering_1, na.rm = T)/1000,
            S2 = sum(Sub_metering_2, na.rm = T)/1000,
            S3 = sum(Sub_metering_3, na.rm = T)/1000,
            other = (Global - S1 - S2 - S3))
View(completePerMonth)

# Subset per month
perMonth <- entireYear %>%
  gather(submeter, kWh, 'Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3') %>%
  group_by(submeter, year, month) %>%
  summarise(kWh = sum(kWh, na.rm = T)/1000)
summary(perMonth)
View(perMonth)

# Subset per month - Global
perMonthGlobal <- entireYear %>%
  select(year, month, Global_active_power) %>%
  group_by(year, month) %>%
  summarise(Global = sum(Global_active_power, na.rm = T)/60)
summary(perMonthGlobal)
View(perMonthGlobal)

# Subset per week
perWeek <- entireYear %>%
  gather(submeter, kWh, 'Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3') %>%
  group_by(submeter, year, week) %>%
  summarise(kWh = sum(kWh, na.rm = T)/1000)
summary(perWeek)
View(perWeek)

# Subset per week - Global
perWeekGlobal <- entireYear %>%
  select(year, week, Global_active_power) %>%
  group_by(year, week) %>%
  summarise(Global = sum(Global_active_power, na.rm = T)/60)
summary(perWeekGlobal)
View(perWeekGlobal)

# Subset per day
perDay <- entireYear %>%
  gather(submeter, kWh, 'Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3') %>%
  group_by(submeter, year, month, day) %>%
  summarise(kWh = sum(kWh, na.rm = T)/1000)
summary(perDay)
View(perDay)

# Treat 2010 to compare with the forecasting
yr_2010$dateTime <- paste(yr_2010$Date, 
                          yr_2010$Time)
yr_2010 <- yr_2010[, c(ncol(yr_2010), 
                             1:(ncol(yr_2010)-1))]
yr_2010$dateTime <- as.POSIXct(yr_2010$dateTime, 
                                  '%Y-%m-%d %H:%M:%S')
attr(yr_2010$dateTime, 'tzone') <- 'UTC'
yr_2010$year <- year(yr_2010$dateTime)
yr_2010$quarter <- quarter(yr_2010$dateTime)
yr_2010$month <- month(yr_2010$dateTime)
yr_2010$week <- week(yr_2010$dateTime)
yr_2010$weekday <- weekdays(yr_2010$dateTime)
yr_2010$day <- day(yr_2010$dateTime)
str(yr_2010)

perWeek2010 <- yr_2010 %>%
  gather(submeter, kWh, 'Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3') %>%
  group_by(submeter, year, week) %>%
  summarise(kWh = sum(kWh, na.rm = T)/1000)
summary(perWeek2010)
View(perWeek2010)

perWeek2010Global <- yr_2010 %>%
  select(week, Global_active_power) %>%
  group_by(week) %>%
  summarise(Global = sum(Global_active_power, na.rm = T)/60)
summary(perWeek2010Global)
View(perWeek2010Global)

perMonth2010Global <- yr_2010 %>%
  select(year, month, Global_active_power) %>%
  group_by(year, month) %>%
  summarise(Global = sum(Global_active_power, na.rm = T)/60)
summary(perMonth2010Global)
View(perMonth2010Global)

#### Plots ####
plot(entireYear$Sub_metering_1)
plot(entireYear$Sub_metering_2)
plot(entireYear$Sub_metering_3)

# Histogram
hist(entireYear$Sub_metering_1)
hist(entireYear$Sub_metering_2)
hist(entireYear$Sub_metering_3)

# Subset a day data
houseDay <- filter(entireYear, year == 2008 & month == 1 & day == 9)
plot_ly(houseDay, 
        x = ~houseDay$dateTime,
        y = ~houseDay$Sub_metering_1,
        name = 'Kitchen',
        type = 'scatter',
        mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2,
            name = 'Laundry',
            mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3,
            name = 'Water Heater & AC',
            mode = 'lines') %>%
  layout(title = 'Power Consumption January 9th, 2008',
         xaxis = list(title = 'Time'),
         yaxis = list(title = 'Power (kWh)'))

# Subset a day data - 10 min frequency
houseDay10 <- filter(entireYear,
                     year == 2008 &
                       month == 1 &
                       day == 9 &
                       (minute == 0 |
                        minute == 10 |
                        minute == 20 |
                        minute == 30 |
                        minute == 40 |
                        minute == 50))
plot_ly(houseDay10, 
        x = ~houseDay10$dateTime,
        y = ~houseDay10$Sub_metering_1,
        name = 'Kitchen',
        type = 'scatter',
        mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2,
            name = 'Laundry',
            mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3,
            name = 'Water Heater & AC',
            mode = 'lines') %>%
  layout(title = 'Power Consumption January 9th, 2008 - 10 min frequency',
         xaxis = list(title = 'Time'),
         yaxis = list(title = 'Power (kWh)'))

# Subset a week data - 30 min frequency
houseWeek30 <- filter(entireYear,
                      year == 2008 &
                       week == 50 &
                       (minute == 0 |
                          minute == 30))
plot_ly(houseWeek30, 
        x = ~houseWeek30$dateTime,
        y = ~houseWeek30$Sub_metering_1,
        name = 'Kitchen',
        type = 'scatter',
        mode = 'lines') %>%
  add_trace(y = ~houseWeek30$Sub_metering_2,
            name = 'Laundry',
            mode = 'lines') %>%
  add_trace(y = ~houseWeek30$Sub_metering_3,
            name = 'Water Heater & AC',
            mode = 'lines') %>%
  layout(title = 'Power Consumption week 50 2008 - 30 min frequency',
         xaxis = list(title = 'Time'),
         yaxis = list(title = 'Power (kWh)'))

# Subset with mondays at 8 pm
houseMonday8 <- filter(entireYear,
                        weekday == 'segunda' &
                         hour == 20 &
                         minute == 1)
houseMonday8TimeSerie <- ts(houseMonday8$Sub_metering_3,
                            frequency = 52,
                            start = c(2007,1))
autoplot(houseMonday8TimeSerie,
         ts.colour = 'blue',
         xlab = 'Time',
         ylab = 'Sub meter 3 - Power (kWh)',
         main = 'Sub-meter 3')
plot.ts(houseMonday8TimeSerie)

# Time series - Month - Sub meter 1
monthS1 <- filter(perMonth, submeter == 'Sub_metering_1')
monthTsS1 <- ts(monthS1$kWh,
                frequency = 12,
                start = c(2007,1))
autoplot(monthTsS1,
         ts.colour = 'blue',
         xlab = 'Time',
         ylab = 'Sub meter 1 - Power (kWh)',
         main = 'Sub-meter 1')
plot.ts(monthTsS1)

# Time series - Month - Sub meter 2
monthS2 <- filter(perMonth, submeter == 'Sub_metering_2')
monthTsS2 <- ts(monthS2$kWh,
                frequency = 12,
                start = c(2007,1))
autoplot(monthTsS2,
         ts.colour = 'blue',
         xlab = 'Time',
         ylab = 'Sub meter 2 - Power (kWh)',
         main = 'Sub-meter 2')
plot.ts(monthTsS2)

# Time series - Month - Sub meter 3
monthS3 <- filter(perMonth, submeter == 'Sub_metering_3')
monthTsS3 <- ts(monthS3$kWh,
                frequency = 12,
                start = c(2007,1))
autoplot(monthTsS3,
         ts.colour = 'blue',
         xlab = 'Time',
         ylab = 'Sub meter 3 - Power (kWh)',
         main = 'Sub-meter 3')
plot.ts(monthTsS3)

# Time series - Month - Global
monthTs <- ts(perMonthGlobal$Global,
                frequency = 12,
                start = c(2007,1))
autoplot(monthTs,
         ts.colour = 'blue',
         xlab = 'Time',
         ylab = 'Global - Power (kWh)',
         main = 'Global')
plot.ts(monthTs)

# Time series - Month - Global
completeMonthTs <- ts(completePerMonth$Global,
              frequency = 12,
              start = c(2007,1))
autoplot(completeMonthTs,
         ts.colour = 'blue',
         xlab = 'Time',
         ylab = 'Global - Power (kWh)',
         main = 'Global')
plot.ts(completeMonthTs)

# Time series - Month - Global - 2010
monthTs2010 <- ts(perMonth2010Global$Global,
              frequency = 12,
              start = c(2010,1))
autoplot(monthTs2010,
         ts.colour = 'blue',
         xlab = 'Time',
         ylab = 'Global - Power (kWh)',
         main = 'Global')
plot.ts(monthTs2010)

# Time series - Week - Sub meter 1
weekS1 <- filter(perWeek, submeter == 'Sub_metering_1')
weekTsS1 <- ts(weekS1$kWh,
                frequency = 52,
                start = c(2007,1))
autoplot(weekTsS1,
         ts.colour = 'blue',
         xlab = 'Time',
         ylab = 'Sub meter 1 - Power (kWh)',
         main = 'Sub-meter 1')
plot.ts(weekTsS1)

# Time series - Week - Sub meter 2
weekS2 <- filter(perWeek, submeter == 'Sub_metering_2')
weekTsS2 <- ts(weekS2$kWh,
               frequency = 52,
               start = c(2007,1))
autoplot(weekTsS2,
         ts.colour = 'blue',
         xlab = 'Time',
         ylab = 'Sub meter 2 - Power (kWh)',
         main = 'Sub-meter 2')
plot.ts(weekTsS2)

# Time series - Week - Sub meter 3
weekS3 <- filter(perWeek, submeter == 'Sub_metering_3')
weekTsS3 <- ts(weekS3$kWh,
               frequency = 52,
               start = c(2007,1))
autoplot(weekTsS3,
         ts.colour = 'blue',
         xlab = 'Time',
         ylab = 'Sub meter 3 - Power (kWh)',
         main = 'Sub-meter 3')
plot.ts(weekTsS3)

# Time series - Week - Global
weekTs <- ts(perWeekGlobal$Global,
             frequency = 53,
             start = c(2007,1),
             end=c(2009, 53))
autoplot(weekTs,
         ts.colour = 'blue',
         xlab = 'Time',
         ylab = 'Global - Power (kWh)',
         main = 'Global')
plot.ts(weekTs)

# Time series - Week - Global - Complete
completeWeekTs <- ts(completePerWeek$Global,
                     frequency = 53,
                     start = c(2007,1),
                     end=c(2010, 48))
autoplot(completeWeekTs,
         ts.colour = 'blue',
         xlab = 'Time',
         ylab = 'Global - Power (kWh)',
         main = 'Global')
plot.ts(completeWeekTs)

# Time series - Week - Sub meter 1 - 2010
weekS12010 <- filter(perWeek2010, submeter == 'Sub_metering_1')
weekTsS12010 <- ts(weekS12010$kWh,
               frequency = 48,
               start = c(2010,1))
autoplot(weekTsS12010,
         ts.colour = 'blue',
         xlab = 'Time',
         ylab = 'Sub meter 1 - Power (kWh)',
         main = 'Sub-meter 1')
plot.ts(weekTsS12010)

# Time series - Week - Sub meter 2 - 2010
weekS22010 <- filter(perWeek2010, submeter == 'Sub_metering_2')
weekTsS22010 <- ts(weekS22010$kWh,
                   frequency = 48,
                   start = c(2010,1))
autoplot(weekTsS22010,
         ts.colour = 'blue',
         xlab = 'Time',
         ylab = 'Sub meter 2 - Power (kWh)',
         main = 'Sub-meter 2')
plot.ts(weekTsS22010)

# Time series - Week - Sub meter 3 - 2010
weekS32010 <- filter(perWeek2010, submeter == 'Sub_metering_3')
weekTsS32010 <- ts(weekS32010$kWh,
                   frequency = 48,
                   start = c(2010,1))
autoplot(weekTsS32010,
         ts.colour = 'blue',
         xlab = 'Time',
         ylab = 'Sub meter 3 - Power (kWh)',
         main = 'Sub-meter 3')
plot.ts(weekTsS32010)

# Time series - Week - Global - 2010
weekTs2010 <- ts(perWeek2010Global$Global,
                 frequency = 53,
                 start = c(2010,1),
                 end=c(2010, 48))
autoplot(weekTs2010,
         ts.colour = 'blue',
         xlab = 'Time',
         ylab = 'Global - Power (kWh)',
         main = 'Global')
plot.ts(weekTs2010)

# Time series - Day - Sub meter 1
dayS1 <- filter(perDay, submeter == 'Sub_metering_1')
dayTsS1 <- ts(dayS1$kWh,
               frequency = 365,
               start = c(2007,1))
autoplot(dayTsS1,
         ts.colour = 'blue',
         xlab = 'Time',
         ylab = 'Sub meter 1 - Power (kWh)',
         main = 'Sub-meter 1')
plot.ts(dayTsS1)

# Time series - Day - Sub meter 2
dayS2 <- filter(perDay, submeter == 'Sub_metering_2')
dayTsS2 <- ts(dayS2$kWh,
              frequency = 365,
              start = c(2007,1))
autoplot(dayTsS2,
         ts.colour = 'blue',
         xlab = 'Time',
         ylab = 'Sub meter 2 - Power (kWh)',
         main = 'Sub-meter 2')
plot.ts(dayTsS2)

# Time series - Day - Sub meter 3
dayS3 <- filter(perDay, submeter == 'Sub_metering_3')
dayTsS3 <- ts(dayS3$kWh,
              frequency = 365,
              start = c(2007,1))
autoplot(dayTsS3,
         ts.colour = 'blue',
         xlab = 'Time',
         ylab = 'Sub meter 3 - Power (kWh)',
         main = 'Sub-meter 3')
plot.ts(dayTsS3)

#### Forecasting ####
# Linear Model to forecast 20 periods of the time series with mondays at 8 pm
fitLmS3 <- tslm(houseMonday8TimeSerie ~ trend + season)
summary(fitLmS3)
forecastLmS3 <- forecast(fitLmS3, 
                         h=20, 
                         level = c(80,90))
plot(forecastLmS3,
     ylim = c(0,30),
     ylab = 'kWh',
     xlab = 'Time')

# Decompose the time series with mondays at 8 pm
componentsS3 <- decompose(houseMonday8TimeSerie)
sum(componentsS3$random, na.rm = T)/sum(componentsS3$x)
plot(componentsS3)
summary(componentsS3)

# Decompose - Month - S1
cMonthS1 <- decompose(monthTsS1)
sum(cMonthS1$random, na.rm = T)/sum(cMonthS1$x)
plot(cMonthS1)

# Decompose - Month - S2
cMonthS2 <- decompose(monthTsS2)
sum(cMonthS2$random, na.rm = T)/sum(cMonthS2$x)
plot(cMonthS2)

# Decompose - Month - S3
cMonthS3 <- decompose(monthTsS3)
sum(cMonthS3$random, na.rm = T)/sum(cMonthS3$x)
plot(cMonthS3)

# Decompose - Week - S1
cWeekS1 <- decompose(weekTsS1)
sum(cWeekS1$random, na.rm = T)/sum(cWeekS1$x)
plot(cWeekS1)

# Decompose - Week - S2
cWeekS2 <- decompose(weekTsS2)
sum(cWeekS2$random, na.rm = T)/sum(cWeekS2$x)
plot(cWeekS2)

# Decompose - Week - S3
cWeekS3 <- decompose(weekTsS3)
sum(cWeekS3$random, na.rm = T)/sum(cWeekS3$x)
plot(cWeekS3)

# Decompose - Day - S1
cDayS1 <- decompose(dayTsS1)
sum(cDayS1$random, na.rm = T)/sum(cDayS1$x)
plot(cDayS1)

# Decompose - Day - S2
cDayS2 <- decompose(dayTsS2)
sum(cDayS2$random, na.rm = T)/sum(cDayS2$x)
plot(cDayS2)

# Decompose - Day - S3
cDayS3 <- decompose(dayTsS3)
sum(cDayS3$random, na.rm = T)/sum(cDayS3$x)
plot(cDayS3)

# Decompose with STL
stlComponetsS3 <- stl(houseMonday8TimeSerie, 'periodic')
plot(stlComponetsS3)
summary(stlComponetsS3)

# Remove seasonal component from decompose
mondayTsAjusted <- houseMonday8TimeSerie - componentsS3$seasonal
autoplot(mondayTsAjusted)
plot(decompose(mondayTsAjusted))

# Use Holt Winters simple exponential smoothing
hwS3 <- HoltWinters(mondayTsAjusted, 
                    beta = F,
                    gamma = T)

# Forecast holt winter
forecastHwS3 <- forecast(hwS3, 
                         h=25,
                         level = c(10,25))
plot(forecastHwS3,
     ylim = c(0,30),
     ylab = 'kWh',
     xlab = 'Time',
     start(2010))

# Holt Winter - Month - S1
hwMonthS1 <- HoltWinters(monthTsS1,
                         beta = F,
                         gamma = T)
forecastHwMonthS1 <- forecast(hwMonthS1, 
                         h=11,
                         level = c(10,25))
plot(forecastHwMonthS1,
     ylim = c(0,80),
     ylab = 'kWh',
     xlab = 'Time')

# Holt Winter - Month - S2
hwMonthS2 <- HoltWinters(monthTsS2,
                         beta = F,
                         gamma = T)
forecastHwMonthS2 <- forecast(hwMonthS2, 
                              h=11,
                              level = c(10,25))
plot(forecastHwMonthS2,
     ylim = c(0,110),
     ylab = 'kWh',
     xlab = 'Time')

# Holt Winter - Month - S3
hwMonthS3 <- HoltWinters(monthTsS3,
                         beta = F,
                         gamma = T)
forecastHwMonthS3 <- forecast(hwMonthS3, 
                              h=11,
                              level = c(10,25))
plot(forecastHwMonthS3,
     ylim = c(0,400),
     ylab = 'kwh',
     xlab = 'Time')

# Holt Winter - Month - Global
hwMonth <- HoltWinters(monthTs,
                         beta = F,
                         gamma = T)
forecastHwMonth <- forecast(hwMonth, 
                              h=11,
                              level = c(10,25))
plot(forecastHwMonth,
     ylim = c(0,1500),
     ylab = 'kWh',
     xlab = 'Time')
lines(monthTs2010)

# Holt Winter - Week - S1
hwWeekS1 <- HoltWinters(weekTsS1,
                         beta = F,
                         gamma = T)
forecastHwWeekS1 <- forecast(hwWeekS1, 
                              h=48,
                              level = c(10,25))
plot(forecastHwWeekS1,
     ylim = c(0,25),
     ylab = 'kWh',
     xlab = 'Time')
lines(weekTsS12010)

# Holt Winter - Week - S2
hwWeekS2 <- HoltWinters(weekTsS2,
                        beta = F,
                        gamma = T)
forecastHwWeekS2 <- forecast(hwWeekS2, 
                             h=48,
                             level = c(10,25))
plot(forecastHwWeekS2,
     ylim = c(0,35),
     ylab = 'kWh',
     xlab = 'Time')
lines(weekTsS22010)

# Holt Winter - Week - S3
hwWeekS3 <- HoltWinters(weekTsS3,
                        beta = F,
                        gamma = T)
forecastHwWeekS3 <- forecast(hwWeekS3, 
                             h=48,
                             level = c(10,25))
plot(forecastHwWeekS3,
     ylim = c(0,100),
     ylab = 'kWh',
     xlab = 'Time')
lines(weekTsS32010)

# Holt Winter - Week - Global
hwWeek <- HoltWinters(weekTs,
                        beta = F,
                        gamma = T)
forecastHwWeek <- forecast(hwWeek, 
                             h=48,
                             level = c(10,25))
plot(forecastHwWeek,
     ylim = c(0,320),
     ylab = 'kWh',
     xlab = 'Time')
lines(weekTs2010)
accuracy(forecastHwWeek, weekTs2010)

# Holt Winter - Week - Global - Complete
completeHwWeek <- HoltWinters(completeWeekTs,
                              beta = F,
                              gamma = T)
completeForecastHwWeek <- forecast(completeHwWeek, 
                           h=58,
                           level = c(10,25))
plot(completeForecastHwWeek,
     ylim = c(0,330),
     ylab = 'kWh',
     xlab = 'Time')
lines(weekTs2010)

# Holt Winter - Day - S1
hwDayS1 <- HoltWinters(dayTsS1,
                        beta = F,
                        gamma = T)
forecastHwDayS1 <- forecast(hwDayS1, 
                             h=340,
                             level = c(10,25))
plot(forecastHwDayS1,
     ylim = c(0,15),
     ylab = 'kWh',
     xlab = 'Time')

# Holt Winter - Day - S2
hwDayS2 <- HoltWinters(dayTsS2,
                       beta = F,
                       gamma = T)
forecastHwDayS2 <- forecast(hwDayS2, 
                            h=340,
                            level = c(10,25))
plot(forecastHwDayS2,
     ylim = c(0,15),
     ylab = 'kWh',
     xlab = 'Time')

# Holt Winter - Day - S3
hwDayS3 <- HoltWinters(dayTsS3,
                       beta = F,
                       gamma = T)
forecastHwDayS3 <- forecast(hwDayS3, 
                            h=340,
                            level = c(10,25))
plot(forecastHwDayS3,
     ylim = c(0,25),
     ylab = 'kWh',
     xlab = 'Time')

# Arima - Week - S1
arimaWeekS1 <- auto.arima(weekTsS1)
forecastArimaWeekS1 <- forecast(arimaWeekS1,
                                h=48,
                                level = c(10,25))
plot(forecastArimaWeekS1,
     ylim = c(0,25),
     ylab = 'kWh',
     xlab = 'Time')

# Arima - Week - S2
arimaWeekS2 <- auto.arima(weekTsS2)
forecastArimaWeekS2 <- forecast(arimaWeekS2,
                                h=48,
                                level = c(10,25))
plot(forecastArimaWeekS2,
     ylim = c(0,35),
     ylab = 'kWh',
     xlab = 'Time')

# Arima - Week - S3
arimaWeekS3 <- auto.arima(weekTsS3)
forecastArimaWeekS3 <- forecast(arimaWeekS3,
                                h=48,
                                level = c(10,25))
plot(forecastArimaWeekS3,
     ylim = c(0,100),
     ylab = 'kWh',
     xlab = 'Time')

# Arima - Week - Global
arimaWeek <- auto.arima(weekTs)
forecastArimaWeek <- forecast(arimaWeek,
                              h=48,
                              level = c(10,25))
plot(forecastArimaWeek,
     ylim = c(0,400),
     ylab = 'kWh',
     xlab = 'Time')
lines(weekTs2010)
accuracy(forecastArimaWeek, weekTs2010)

# Arima - Week - Global - Complete
completeArimaWeek <- auto.arima(completeWeekTs)
completeForecastArimaWeek <- forecast(completeArimaWeek,
                              h=58,
                              level = c(10,25))
plot(completeForecastArimaWeek,
     ylim = c(0,400),
     ylab = 'kWh',
     xlab = 'Time')
lines(weekTs2010)
completeForecastArimaWeekDF <- as.data.frame(completeForecastArimaWeek)
summary(completeForecastArimaWeekDF)

# Arima - Month - Global
arimaMonth <- auto.arima(monthTs)
forecastArimaMonth <- forecast(arimaMonth,
                              h=11,
                              level = c(10,25))
plot(forecastArimaMonth,
     ylim = c(0,1500),
     ylab = 'kWh',
     xlab = 'Time')
lines(monthTs2010)

# Arima - Month - Global - Complete
completeArimaMonth <- auto.arima(completeMonthTs)
completeForecastArimaMonth <- forecast(completeArimaMonth,
                                      h=13,
                                      level = c(10,25))
plot(completeForecastArimaMonth,
     ylim = c(0,1300),
     ylab = 'kWh',
     xlab = 'Time')
lines(monthTs2010)
completeForecastArimaMonthDF <- as.data.frame(completeForecastArimaMonth)
summary(completeForecastArimaMonthDF)

#### Export data to dashboard ####
# JSON
write_json(weekTs2010, 'testTsWeek2010.json')
write_json(perWeek2010Global, 'testWeek2010.json')

# CSV
write.csv(perWeek2010Global, file = 'perWeek2010Global.csv')
write.csv(perWeekGlobal, file = 'perWeekGlobal.csv')
write.csv(perWeek2010, file = 'perWeek2010.csv')
write.csv(perWeek, file = 'perWeek.csv')
write.csv(perMonth, file = 'perMonth.csv')
write.csv(perDay, file = 'perDay.csv')
write.csv(completeForecastArimaMonthDF, file = 'completeForecastArimaMonthDF.csv')
write.csv(completeForecastArimaWeekDF, file = 'completeForecastArimaWeekDF.csv')
write.csv(completePerWeek, file = 'completePerWeek.csv')
write.csv(completePerMonth, file = 'completePerMonth.csv')

roundCompletePerWeek <- round(completePerWeek)
roundCompletePerMonth <- round(completePerMonth)
roundCompleteForecastArimaMonthDF <- round(completeForecastArimaMonthDF)
roundCompleteForecastArimaWeekDF <- round(completeForecastArimaWeekDF)

write.csv(roundCompletePerWeek, file = 'roundCompletePerWeek.csv')
write.csv(roundCompletePerMonth, file = 'roundCompletePerMonth.csv')
write.csv(roundCompleteForecastArimaMonthDF, file = 'roundCompleteForecastArimaMonthDF.csv')
write.csv(roundCompleteForecastArimaWeekDF, file = 'roundCompleteForecastArimaWeekDF.csv')


meltData <- melt(roundCompletePerWeek, id.vars = c('year','week'), measure.vars = c('Global',
                                                                                    'S1',
                                                                                    'S2',
                                                                                    'S3',
                                                                                    'other'))
View(meltData)

write.csv(meltData, file = 'melt.csv')
