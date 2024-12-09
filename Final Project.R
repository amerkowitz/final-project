install.packages(c("lubridate", "dplyr","ggplot2"))
library(lubridate)
library(dplyr)
library(ggplot2)

#  data set for sea levels
sealevels <- read.csv("/cloud/project/Sea Levels - Sheet1.csv")

names <- read.csv("/cloud/project/8443970_meantrend (2).csv",
                      nrow=5,
                      header=FALSE)

# sea levels date
sealevels$date <- sealevels$Year+sealevels$Month/12

sealevels2 <- sealevels %>%
  filter(date <= 2023.833) %>%
  filter(date >= 1970)


plot(sealevels2$date, # x data
     sealevels2$Monthly_MSL, # y data
     type = "l", 
     pch = 19, # symbol shape
     ylab = "Sea level rise", #y axis label
     xlab = "Year") #x axis label

#precipitation data set
precip <- read.csv("3865197.csv")
# parse date and decimal date
exampleDate <- c("1970-01-01")
ymd(exampleDate)
precip$dateF <- ym(precip$DATE)
precip$year <- year(precip$dateF)
precip$month <- month(precip$dateF)
precip$date <- precip$year+precip$month/12

plot(precip$date, # x data
     precip$PRCP, # y data
     type = "p", 
     pch = 19, # symbol shape
     ylab = "Precipitation", #y axis label
     xlab = "Year") #x axis label

install.packages("reshape2")
library(reshape2)

install.packages("lubridate")

temperatures <- read.csv("/cloud/project/Temperatures - Sheet1.csv",
              )

temperature2 <- melt(temperatures, id="Year")
temperature2$month <- as.numeric(gsub("X","",temperature2$variable))

temperature2$date <- temperature2$Year+temperature2$month/12

temperature3 <- temperature2 %>%
  filter(date <= 2023.833)

plot(temperature3$date, # x data
     temperature3$value, # y data
     type = "l", 
     pch = 19, # symbol shape
     ylab = "Temperature", #y axis label
     xlab = "Year") #x axis label

joined <- full_join(sealevels2, # left table
                    temperature3, # right table
                    by="date") # common identifier
head(joined)

colnames(joined)[which(names(joined) == "value")] <- "average temperature"

joined["Year.1"] <- NULL
joined["variable"] <- NULL
joined["month"] <- NULL

plot(joined$`average temperature`, # x data
     joined$Monthly_MSL, # y data
     type = "p", 
     pch = 19, # symbol shape
     ylab = "Monthly Mean Sea Level (meters)", #y axis label
     xlab = "average temperature") #x axis label

joined2 <- full_join(joined, # left table
                    precip, # right table
                    by="date") # common identifier

plot(joined2$PRCP, # x data
     joined2$Monthly_MSL, # y data
     type = "p", 
     pch = 19, # symbol shape
     ylab = "Monthly Mean Sea Level (meters)", #y axis label
     xlab = "precipitation") #x axis label

