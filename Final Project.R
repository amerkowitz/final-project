install.packages(c("lubridate", "dplyr","ggplot2", "olsrr", "PerformanceAnalytics", "Reshape2"))
library(lubridate)
library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)
library(reshape2)

#data set for sea levels - Boston
sealevels <- read.csv("/cloud/project/Sea Levels - Sheet1.csv")

names <- read.csv("/cloud/project/8443970_meantrend (2).csv",
                      nrow=5,
                      header=FALSE)

# sea levels date
sealevels$date <- sealevels$Year+sealevels$Month/12

sealevels2 <- sealevels %>%
  filter(date <= 2024) %>%
  filter(date >= 1970)

#plot rising sea levels over time
ggplot(data = sealevels2, # data for plot
       aes(x = date, y=Monthly_MSL ) )+ # aes, x and y
  geom_line()+ # use lines to connect data points
  labs(x="date", y="Monthly Mean Sea Level (meters)") # make axis labels

#make time series data
sealevels_ts <- ts(sealevels2$Monthly_MSL, # data
              start = c(1970,1), #start year 1984, month 1
              frequency= 12) 

# decompose time series
sealevels_dec <- decompose(sealevels_ts)
# plot decomposition
plot(sealevels_dec)

#add trend to temperatures3 data
sealevels2$sltrend <- sealevels_dec$trend 
print(sealevels2$sltrend)

#precipitation data set
precip <- read.csv("3879021.csv")
# parse date
exampleDate <- c("1970-01-01")
ymd(exampleDate)
precip$dateF <- ym(precip$DATE)
precip$Year <- year(precip$dateF)
precip$Month <- month(precip$dateF)
precip$date <- precip$Year+precip$Month/12

precip2 <- precip %>%
  filter(date <= 2024) %>%
  filter(date > 1970)
#rename wind column
colnames(precip2)[4] <- "Wind"

#temperatures data set
temperatures <- read.csv("/cloud/project/Temperatures - Sheet1.csv",
              )
#temperatures date
temperature2 <- melt(temperatures, id="Year")
temperature2$month <- as.numeric(gsub("X","",temperature2$variable))
temperature2$date <- temperature2$Year+temperature2$month/12

#Convert temperature to numeric
temperature2$temp <- as.numeric(temperature2$value)

#filter years
temperature3 <- temperature2 %>%
  filter(date <= 2024) %>%
  filter(date > 1970)
temperature3 <- temperature3[order(temperature3$date),]

#make time series data
temp_ts <- ts(temperature3$temp, # data
                start = c(1970,1), #start year 1970, month 1
                frequency= 12) 

# decompose time series
temp_dec <- decompose(temp_ts)
# plot decomposition
plot(temp_dec)

#add trend to temperatures3 data
temperature3$temptrend <- temp_dec$trend 
# join sea levels and temps datasets
joined <- full_join(sealevels2, # left table
                    temperature3, # right table
                    by="date") # common identifier
head(joined)


#plot temps and sea levels

ggplot(data = joined, # data for plot
      aes(x=temptrend,  y=sltrend,) )+ # aes, x and y
  geom_point()+ 
  labs(x="Monthly Average Temperature (degrees F)", y="Monthly Mean Sea Level (meters)") # make axis labels

#join precip
joined2 <- full_join(joined, # left table
                    precip2, # right table
                    by="date") # common identifier

plot(joined2$PRCP, # x data
     joined2$sltrend, # y data
     type = "p", 
     pch = 19, # symbol shape
     ylab = "Monthly Mean Sea Level (meters)", #y axis label
     xlab = "precipitation") #x axis label

#storm events data
stormevents  <- read.csv("/cloud/project/Storm events - Sheet1.csv")

#parse date
exampleDate <- mdy("09/18/1996")
stormevents$dateF <- mdy(stormevents$BEGIN_DATE)
stormevents$Year <- year(stormevents$dateF)
stormevents$month <- month(stormevents$dateF)
stormevents$date <- stormevents$Year+stormevents$month/12

#filter data
stormevents2 <- stormevents %>%
  filter(Year < 2023)

#join storm events
joined3 <- full_join(joined2, # left table
                     stormevents2, # right table
                     by="date") # common identifier
#create binary variable
joined3$floodevent <- ifelse(is.na(joined3$EVENT_TYPE), 0, 1)

joined4 <- joined3 %>%
  filter(date<=2023.34)

#make plot for wind with filtered data to exclude rows where there is no wind data (before 1983)
joined5 <- joined4 %>%
  filter(date>=1983)
plot(joined4$Wind, # x data
       joined4$sltrend, # y data
       type = "p", 
       pch = 19, # symbol shape
       ylab = "Monthly Mean Sea Level (meters)", #y axis label
       xlab = "Moonthly Average Wind Speed") #x axis label
  
#filter out unnecessary columns
joined4$Year.x.y <- NULL  
joined4$Year.y.x <- NULL
joined4$Year.y.y <- NULL
joined4$Year.x <- NULL
joined4$Year.y <- NULL
joined4$month.y <- NULL
joined4$month.x <- NULL
joined4$month <- NULL
joined4$Month.x <- NULL
joined4$Month.y <- NULL

# multiple regression

# creates a model object
mod.full <- lm(sltrend ~
                 PRCP+temptrend+Wind+floodevent,
                 data=joined4)
summary(mod.full)

#checking assumptions
res.full <- rstandard(mod.full)
fit.full <- fitted.values(mod.full)

#qq plot
qqnorm(res.full, pch=19, col="grey50")
qqline(res.full)

plot(fit.full,res.full, pch=19, col="grey50")
abline(h=0)

#distribution slightly uneven, eliminate flood event variable
mod.full2 <- lm(sltrend ~
                 PRCP+temptrend+Wind,
               data=joined4)
summary(mod.full2)

#checking assumptions
res.full2 <- rstandard(mod.full2)
fit.full2 <- fitted.values(mod.full2)

#qq plot
qqnorm(res.full2, pch=19, col="grey50")
qqline(res.full2)


#data follows line


plot(fit.full2,res.full2, pch=19, col="grey50")
abline(h=0)

#good distribution

reg.data <- data.frame(joined4$PRCP,
                       joined4$temptrend,
                       joined4$Wind)

# make a correlation matrix 
chart.Correlation(reg.data, histogram=TRUE, pch=19)

#no multicollinearity, all correlation coefficients are below 0.7

# run stepwise
full.step <- ols_step_forward_aic(mod.full2)
# view table
full.step 

# check full model
full.step$model

# plot AIC over time
plot(full.step)

#greatest influence on model is temperature trend, followed by average wind speed

#key west model

sealevelskey <- read.csv("/cloud/project/Key West Sea Levels - Sheet1 2.csv")

# sea levels date
sealevelskey$date <- sealevelskey$Year+sealevelskey$Month/12

sealevelskey2 <- sealevelskey %>%
  filter(date <= 2024) %>%
  filter(date >= 1970)

#plot rising sea levels over time
ggplot(data = sealevelskey2, # data for plot
       aes(x = date, y=Monthly_MSL ) )+ # aes, x and y
  geom_line()+ # use lines to connect data points
  labs(x="date", y="Monthly Mean Sea Level (meters)") # make axis labels

#make time series data
sealevelskey_ts <- ts(sealevelskey2$Monthly_MSL, # data
                   start = c(1970,1), #start year 1970, month 1
                   frequency= 12) 

#decompose time series
sealevelskey_dec <- decompose(sealevelskey_ts)
#plot decomposition
plot(sealevelskey_dec)

#add trend to sheet
sealevelskey2$sltrend <- sealevelskey_dec$trend 

#precipitation dataset
precipkey <- read.csv("/cloud/project/3878910.csv")

#parse date
exampleDate <- c("1970-01-01")
ymd(exampleDate)
precipkey$dateF <- ym(precipkey$DATE)
precipkey$Year <- year(precipkey$dateF)
precipkey$Month <- month(precipkey$dateF)
precipkey$date <- precipkey$Year+precipkey$Month/12

precipkey2 <- precipkey %>%
  filter(date <= 2024) %>%
  filter(date >= 1970)
precipkey3 <- precipkey2[precipkey2$STATION == "USW00012836", ]

#temperatures data set
temperatureskey <- read.csv("/cloud/project/Key West Temperatures - Sheet1.csv",
)
#temperatures date
temperaturekey2 <- melt(temperatureskey, id="Year")
temperaturekey2$month <- as.numeric(gsub("X","",temperaturekey2$variable))
temperaturekey2$date <- temperaturekey2$Year+temperaturekey2$month/12

#Convert temperature to numeric
temperaturekey2$temp <- as.numeric(temperaturekey2$value)

#filter years
temperaturekey3 <- temperaturekey2 %>%
  filter(date <= 2024) %>%
  filter(date >= 1970)
temperaturekey3 <- temperaturekey3[order(temperaturekey3$date),]

#make time series data
tempkey_ts <- ts(temperaturekey3$temp, # data
              start = c(1970,1), #start year 1970, month 1
              frequency= 12) 

# decompose time series
tempkey_dec <- decompose(tempkey_ts)

#add trend to temperatures3 data
temperaturekey3$temptrend <- tempkey_dec$trend 
# join sea levels and temps datasets
joinedkey <- full_join(sealevelskey2, # left table
                    temperaturekey3, # right table
                    by="date") # common identifier
head(joined)


#plot temps and sea levels

ggplot(data = joinedkey, # data for plot
       aes(x = temptrend, y=sltrend) )+ # aes, x and y
  geom_point()+ 
  labs(x="Monthly Average Temperature (degrees F)", y="Monthly Mean Sea Level (meters)") # make axis labels

#join precip
joinedkey2 <- full_join(joinedkey, # left table
                     precipkey3, # right table
                     by="date") # common identifier

plot(joinedkey2$PRCP, # x data
     joinedkey2$sltrend, # y data
     type = "p", 
     pch = 19, # symbol shape
     ylab = "Monthly Mean Sea Level (meters)", #y axis label
     xlab = "precipitation") #x axis label

#storm events data
stormeventskey <- read.csv("/cloud/project/storm_data_search_results 2.csv")

#parse date
exampleDate <- mdy("09/18/1996")
stormeventskey$dateF <- mdy(stormeventskey$BEGIN_DATE)
stormeventskey$Year <- year(stormeventskey$dateF)
stormeventskey$month <- month(stormeventskey$dateF)
stormeventskey$date <- stormeventskey$Year+stormeventskey$month/12


#filter data
stormeventskey2 <- stormeventskey %>%
  filter(Year < 2023)

#join storm events
joinedkey3 <- full_join(joinedkey2, # left table
                     stormeventskey2, # right table
                     by="date") # common identifier
#rename wind column
colnames(joinedkey3)[21] <- "Wind"
#create binary variable
joinedkey3$floodevent <- ifelse(is.na(joinedkey3$EVENT_TYPE), 0, 1)


joinedkey4 <- joinedkey3 %>%
  filter(date>1970.5) %>% 
  filter(date<=2023.5)

#make plot for wind with filtered data to exclude rows where there is no wind data (before 1983)
joinedkey5 <- joinedkey4 %>%
  filter(date>=1983)
plot(joinedkey4$Wind, # x data
     joinedkey4$sltrend, # y data
     type = "p", 
     pch = 19, # symbol shape
     ylab = "Monthly Mean Sea Level (meters)", #y axis label
     xlab = "Moonthly Average Wind Speed") #x axis label

#filter out unnecessary columns
joinedkey4$Year.x.y <- NULL  
joinedkey4$Year.y.x <- NULL
joinedkey4$Year.y.y <- NULL
joinedkey4$Year.x <- NULL
joinedkey4$Year.y <- NULL
joinedkey4$month.y <- NULL
joinedkey4$month.x <- NULL
joinedkey4$month <- NULL
joinedkey4$Month.x <- NULL
joinedkey4$Month.y <- NULL

# multiple regression

# creates a model object
mod.fullkey <- lm(sltrend ~
                 PRCP+temptrend+floodevent+Wind,
               data=joinedkey4)
summary(mod.fullkey)

#checking assumptions
res.fullkey <- rstandard(mod.fullkey)
fit.fullkey <- fitted.values(mod.fullkey)

#qq plot
qqnorm(res.fullkey, pch=19, col="grey50")
qqline(res.fullkey)

#data follows line

#shapiro-wilks test
shapiro.test(res.fullkey)

plot(fit.fullkey,res.fullkey, pch=19, col="grey50")
abline(h=0)

#distribution slightly abnormal

#log variables
joinedkey4$log.temp <- log(joinedkey4$temptrend)
joinedkey4$log.sl <- log(joinedkey4$sltrend)
joinedkey4$log.PRCP <- log(joinedkey4$PRCP)
joinedkey4$log.wind <- log(joinedkey4$Wind)

mod.fullkey2 <- lm(sltrend ~
                    temptrend+floodevent+PRCP,
                  data=joinedkey4)
summary(mod.fullkey2)

#checking assumptions
res.fullkey2 <- rstandard(mod.fullkey2)
fit.fullkey2 <- fitted.values(mod.fullkey2)

#qq plot
qqnorm(res.fullkey2, pch=19, col="grey50")
qqline(res.fullkey2)

#data follows line

plot(fit.fullkey2,res.fullkey2, pch=19, col="grey50")
abline(h=0)

reg.data2 <- data.frame(joinedkey4$PRCP,
                       joinedkey4$temptrend,
                       joinedkey4$floodevent)

# make a correlation matrix 
chart.Correlation(reg.data, histogram=TRUE, pch=19)

#no multicollinearity

# run stepwise
full.stepkey <- ols_step_forward_aic(mod.fullkey2)
# view table
full.stepkey

# check full model
full.stepkey$model

# plot AIC over time
plot(full.stepkey)


