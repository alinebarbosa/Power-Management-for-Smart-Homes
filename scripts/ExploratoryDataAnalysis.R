####### IoT analytics - Task 1
####### Exploratory Data Analysis

#### Install and load packages and set seed ####
if (require('pacman') == 'FALSE') {
  install.packages('pacman')
}

pacman::p_load(readr, 
               caret, 
               dplyr,
               ggplot2, 
               DataExplorer, 
               RMySQL,
               lubridate)
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
                      'select date, time, sub_metering_1, sub_metering_2, sub_metering_3 
                      from yr_2006')
yr_2007 <- dbGetQuery(con, 
                      'select date, time, sub_metering_1, sub_metering_2, sub_metering_3 
                      from yr_2007')
yr_2008 <- dbGetQuery(con, 
                      'select date, time, sub_metering_1, sub_metering_2, sub_metering_3 
                      from yr_2008')
yr_2009 <- dbGetQuery(con, 
                      'select date, time, sub_metering_1, sub_metering_2, sub_metering_3 
                      from yr_2009')
yr_2010 <- dbGetQuery(con, 
                      'select date, time, sub_metering_1, sub_metering_2, sub_metering_3 
                      from yr_2010')

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
entireYear$dateTime <- paste(entireYear$date, 
                             entireYear$time)
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

#### Plots ####
# Histogram
hist(entireYear$sub_metering_1)
hist(entireYear$sub_metering_2)
hist(entireYear$sub_metering_3)

#### Entire data set ####
# Create data frame from 2007, 2008 and 2009
yr_2007_complete <- dbGetQuery(con, 
                      'select * from yr_2007')
yr_2008_complete <- dbGetQuery(con, 
                      'select * from yr_2008')
yr_2009_complete <- dbGetQuery(con, 
                      'select * from yr_2009')
entireYearComplete <- bind_rows(yr_2007_complete,
                                yr_2008_complete,
                                yr_2009_complete)

# Transform complete data
entireYearComplete$dateTime <- paste(entireYearComplete$Date,
                                     entireYearComplete$Time)
entireYearComplete <- entireYearComplete[, c(ncol(entireYearComplete),
                                             1:(ncol(entireYearComplete)-1))]
entireYearComplete$dateTime <- as.POSIXct(entireYearComplete$dateTime, 
                                  '%Y-%m-%d %H:%M:%S')
attr(entireYearComplete$dateTime, 'tzone') <- 'UTC'
entireYearComplete$year <- year(entireYearComplete$dateTime)
entireYearComplete$quarter <- quarter(entireYearComplete$dateTime)
entireYearComplete$month <- month(entireYearComplete$dateTime)
entireYearComplete$week <- week(entireYearComplete$dateTime)
entireYearComplete$weekday <- weekdays(entireYearComplete$dateTime)
entireYearComplete$day <- day(entireYearComplete$dateTime)
entireYearComplete$hour <- hour(entireYearComplete$dateTime)
entireYearComplete$minute <- minute(entireYearComplete$dateTime)

# Get to know the complete data
str(entireYearComplete)
summary(entireYearComplete)
sum(is.na(entireYearComplete))

#### Trying things ------------------------------------------------------------

d <- entireYearComplete %>%
  select(Global_active_power, 
         Global_reactive_power, 
         Global_intensity, 
         Voltage,
         Sub_metering_1,
         Sub_metering_2,
         Sub_metering_3,
         month,
         year) %>%
  mutate(powerkW = Global_intensity * Voltage / 1000,
         p1 = Sub_metering_1/(Sub_metering_1+Sub_metering_2+Sub_metering_3),
         p2 = Sub_metering_2/(Sub_metering_1+Sub_metering_2+Sub_metering_3),
         p3 = Sub_metering_3/(Sub_metering_1+Sub_metering_2+Sub_metering_3)) %>%
  group_by(month) %>%
  summarise(mean_p1 = mean(p3))

summary(d)
head(d)
View(d)

perMonth <- entireYearComplete %>%
  gather(submeter, wattHour, 'Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3') %>%
  group_by(submeter, month) %>%
  summarise(meanWattHour = mean(wattHour, na.rm = T))

ggplot(MonthCons, 
       aes(x=factor(perMonth$month), 
           y=perMonth$meanWattHour, 
           fill=perMonth$submeter)) +
  geom_bar(stat = "identity", col = "black") +
  scale_x_discrete(labels = c('Jan',
                              'Feb',
                              'Mar',
                              'Apr',
                              'May',
                              'Jun',
                              'Jul',
                              'Aug',
                              'Sep',
                              'Oct',
                              'Nov',
                              'Dec')) +
  scale_fill_discrete(name = "Submeters", 
                      labels = c("1", "2", "3")) +
  labs(x='Month',
       y='Measure (W/h)') +
  ggtitle('Consum grouped by month') +
  theme_bw()

perHour <- entireYearComplete %>%
  gather(submeter, wattHour, 'Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3') %>%
  group_by(submeter, hour) %>%
  summarise(meanWattHour = mean(wattHour, na.rm = T))

ggplot(MonthCons, 
       aes(x=factor(perHour$hour), 
           y=perHour$meanWattHour, 
           fill=perHour$submeter)) +
  geom_bar(stat = "identity", col = "black") +
  scale_fill_discrete(name = "Submeters", 
                      labels = c("1", "2", "3")) +
  labs(x='Hour',
       y='Measure (W/h)') +
  ggtitle('Consum grouped by hour') +
  theme_bw()

View(perHour)
