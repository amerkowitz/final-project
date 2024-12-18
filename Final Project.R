install.packages(c("lubridate", "dplyr","ggplot2", "olsrr", "PerformanceAnalytics", "Reshape2"))
library(lubridate)
library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)
library(reshape2)
#  data set for sea levels
sealevels <- read.csv("/cloud/project/Sea Levels - Sheet1.csv")

names <- read.csv("/cloud/project/8443970_meantrend (2).csv",
                      nrow=5,
                      header=FALSE)

# sea levels date
sealevels$date <- sealevels$Year+sealevels$Month/12

sealevels2 <- sealevels %>%
  filter(date <= 2024) %>%
  filter(date > 1970)

#plot rising sea levels over time
ggplot(data = sealevels2, # data for plot
       aes(x = date, y=Monthly_MSL ) )+ # aes, x and y
  geom_line()+ # use lines to connect data points
  labs(x="date", y="Monthly Mean Sea Level (meters)") # make axis labels

#make time series data
sealevels_ts <- ts(sealevels2$Monthly_MSL, # data
              start = c(1970,1), #start year 1970, month 1
              frequency= 12) 

# decompose time series
sealevels_dec <- decompose(sealevels_ts)
# plot decomposition
plot(sealevels_dec)

#add trend to temperatures3 data
sealevels2$sltrend <- sealevels_dec$trend 
print(sealevels2$sltrend)

#precipitation data set
precip <- read.csv("3865197.csv")
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
# temperatures data set
temperatures <- read.csv("/cloud/project/Temperatures - Sheet1.csv",
              )
# temperatures date
temperature2 <- melt(temperatures, id="Year")
temperature2$month <- as.numeric(gsub("X","",temperature2$variable))
temperature2$date <- temperature2$Year+temperature2$month/12

# Convert temperature to numeric
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
       aes(x = sltrend, y=temptrend) )+ # aes, x and y
  geom_point()+ 
  labs(x="average temperature", y="Yearly Mean Sea Level") # make axis labels

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

# multiple regression
#log variables
joined3$log.sealevels <- log(joined3$avgsealevels)
joined3$log.temp <- log(joined3$avgtemp)
joined3$log.PRCP <- log (joined3$avgprecip)
joined3$log.SNOW <- log (joined3$SNOW)

joined4 <- joined3 %>%
  filter(date>1970.5) %>% 
  filter(date<=2023.5)
  
# creates a model object
mod.full <- lm(sltrend.y ~
                 PRCP+floodevent+temptrend.y,
                  data=joined4)
summary(mod.full)

#checking assumptions

res.full <- rstandard(mod.full)
fit.full <- fitted.values(mod.full)

# qq plot
qqnorm(res.full, pch=19, col="grey50")
qqline(res.full)


# shapiro-wilks test
shapiro.test(res.full)

plot(fit.full,res.full, pch=19, col="grey50")
abline(h=0)

reg.data <- data.frame(joined3$PRCP,
                       joined3$floodevent,
                       joined3$avgtemp)
                     

# make a correlation matrix 
chart.Correlation(reg.data, histogram=TRUE, pch=19)

# run stepwise
full.step <- ols_step_forward_aic(mod.full)
# view table
full.step 

# check full model
full.step$model

# plot AIC over time
plot(full.step )

