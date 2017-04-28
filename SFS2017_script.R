##R script for completing the analyses and figures SFS 2017

#set the working directory

setwd("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017")

#load required packages
library(plyr)
library(dplyr)


#import the data for 
##Time, temp, and light

datetime <- read.csv(file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/All_DateTime.csv", T)
temp <- read.csv(file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/temp_all1.csv", T)
light <- read.csv(file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/Light-est_full.csv", T)

#subset the selected streams
tempHV <- temp[,c(1,14)]
temp9 <- temp[,c(1,8)]
temp6 <- temp[,c(1,6)]
temp14 <- temp[,c(1,12)]

#converting the times to posix objects
datetime$Pd <- as.POSIXct(paste(datetime$Date, datetime$Time),format = "%m/%d/%y %H:%M:%S", tz = "UTC")
tempHV$Pd <- as.POSIXct(tempHV$time, format =  "%m-%d-%y %H:%M", tz = "UTC")
temp9$Pd <- as.POSIXct(paste(temp9$time), format =  "%m/%d/%y %H:%M:%S", tz = "UTC")
temp6$Pd <- as.POSIXct(paste(temp6$time), format =  "%m/%d/%y %H:%M:%S", tz = "UTC")
temp14$Pd <- as.POSIXct(paste(temp14$time), format =  "%m/%d/%y %H:%M:%S", tz = "UTC")

light$Pd <- as.POSIXct(paste(light$date, light$time),format = "%m/%d/%y %H:%M:%S", tz = "UTC")

#merging the full datetime file 
mylist <- list(tempHV, datetime)
tempHV <- do.call(rbind.fill, mylist)

mylist <- list(temp9, datetime)
temp9 <- do.call(rbind.fill, mylist)

mylist <- list(temp6, datetime)
temp6 <- do.call(rbind.fill, mylist)

mylist <- list(temp14, datetime)
temp14 <- do.call(rbind.fill, mylist)

mylist <- list(light, datetime)
light <- do.call(rbind.fill, mylist)
rm(mylist)
#make hourly means
tempHVhr_d <- data.frame(tempHV$Pd, tempHV)
names(tempHVhr_d) <- c("time", "tempHV")

temp9hr_d <- data.frame(temp9$Pd, temp9)
names(temp9hr_d) <- c("time", "temp9")

temp6hr_d <- data.frame(temp6$Pd, temp6)
names(temp6hr_d) <- c("time", "temp6")

temp14hr_d <- data.frame(temp14$Pd, temp14)
names(temp14hr_d) <- c("time", "temp14")

lighthr_d <- data.frame(light$Pd, light$light)
names(lighthr_d) <- c("time", "light.est")

#first merge all the depth data by time 

tempHV <- aggregate(tempHVhr_d["tempHV"], list(hour = cut(tempHVhr_d$time, breaks = "hour")),
mean, na.rm = T)

temp9 <- aggregate(temp9hr_d["temp9"], list(hour = cut(temp9hr_d$time, breaks = "hour")), 
mean, na.rm = T)

temp6 <- aggregate(temp6hr_d["temp6"], list(hour = cut(temp6hr_d$time, breaks = "hour")), 
mean, na.rm = T)

temp14 <- aggregate(temp14hr_d["temp14"], list(hour = cut(temp14hr_d$time, breaks = "hour")), 
mean, na.rm = T)

lighthr <- aggregate(lighthr_d["light.est"], list(hour = cut(lighthr_d$time, breaks = "hour")),
mean, na.rm= TRUE)

