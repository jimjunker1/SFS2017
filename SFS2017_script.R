##R script for completing the analyses and figures SFS 2017

#set the working directory

setwd("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017")

#load required packages
library(plyr)
library(dplyr)
library(ggplot2)

#set ggplot theme
theme_set(theme_bw(20))
#import the data for 
##Time, temp, and light

datetime <- read.csv(file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/All_DateTime.csv", T)
temp <- read.csv(file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/temp_depth_all.csv", T)
light.est <- read.csv(file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/Light-est_full.csv", T)
light <- read.csv(file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/Light_full.csv", T)

#subset the selected streams
temp7 <- temp[,c(1,2)]
tempHV <- temp[,c(1,35)]
temp9 <- temp[,c(1,18)]
temp6 <- temp[,c(1,12)]
temp14 <- temp[,c(1,29)]

#converting the times to posix objects
datetime$Pd <- as.POSIXct(paste(datetime$Date, datetime$Time),format = "%m/%d/%y %H:%M:%S", tz = "UTC")
tempHV$Pd <- as.POSIXct(tempHV$time, format =  "%d-%m-%y %H:%M", tz = "UTC")
temp9$Pd <- as.POSIXct(temp9$time, format = "%d-%m-%y %H:%M", tz = "UTC")
temp6$Pd <- as.POSIXct(temp6$time, format = "%d-%m-%y %H:%M", tz = "UTC")
temp14$Pd <- as.POSIXct(temp14$time, format = "%d-%m-%y %H:%M", tz = "UTC")
temp7$Pd <- as.POSIXct(temp7$time, format = "%d-%m-%y %H:%M", tz = "UTC")
light$Pd <- as.POSIXct(paste(light$Date, light$Time),format = "%m/%d/%y %H:%M:%S", tz = "UTC")
light.est$Pd <- as.POSIXct(paste(light.est$date, light.est$time), format = "%m/%d/%y %H:%M:%S", tz = "UTC")

#neeed to aggregate light estimates.
lighthr_d <- aggregate(light["Intensity"], list(hour = cut(light$Pd, breaks = "hour")),mean, na.rm = T)

light.esthr_d <- aggregate(light.est["light"], list(hour = cut(light.est$Pd, breaks = "hour")),mean, na.rm = T)

lighthr_d$Pd <- as.POSIXct(lighthr_d$hour, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
light.esthr_d$Pd <- as.POSIXct(light.esthr_d$hour, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

#merge all the data frames to build one.
temp_light <- merge(tempHV[,c(2:3)], temp9[,c(2:3)], by = "Pd", all = T)
temp_light <- merge(temp_light, temp7[,c(2:3)], by = "Pd", all = T)
temp_light <- merge(temp_light, temp6[,c(2:3)], by = "Pd", all = T)
temp_light <- merge(temp_light, temp14[,c(2:3)], by = "Pd", all = T)
temp_light <- merge(temp_light, lighthr_d[,c(2:3)], by = "Pd", all = T)
temp_light <- merge(temp_light, light.esthr_d[,c(2:3)], by = "Pd", all = T)

#subsetting the posix data

temp_light11.12 <- subset(temp_light, Pd >= as.POSIXct('2011-06-01 00:00:00') & Pd <= as.POSIXct('2012-09-01 00:00:00'))
summary(temp_light11.12)

head(temp_light11.12)
tail(temp_light11.12)


#plotting the data to look at the holes.
 ggplot(temp_light11.12, aes(x = Pd, y = Hver_tempC)) + geom_line() +
    geom_line(aes(x = Pd, y = st9_tempC), colour = "grey") +
    geom_line(aes(x = Pd, y = st6_tempC), colour = "light blue") +
    geom_line(aes(x = Pd, y = st14_tempC), colour = "red") +
    geom_line(aes(x = Pd, y = L_tempC), colour = "blue") +
   scale_y_continuous(limits = c(0,37)) + ylab("Temp (C)") + 
   xlab("Date") + theme(panel.grid = element_blank())

 ggplot(temp_light11.12, aes(x = Pd, y = Intensity)) + geom_line() +
    xlab("Date") + scale_y_continuous(expand = c(0,0)) + theme(panel.grid = element_blank())
 ggplot(temp_light11.12, aes(x = Pd, y = light)) +geom_line()

#Now bring in epilithon data
 
chla <- read.csv(file = "C:/Users/Jim/Documents/Projects/Iceland/Epilithon/Chla.csv", T)
epi <- read.csv(file = "C:/Users/Jim/Documents/Projects/Iceland/Epilithon/IcelandEPI.csv", T)

chla.mod = chla[which(chla$Stream == "ST6" | chla$Stream == "ST9" | chla$Stream == "ST14" | chla$Stream == "Hver"),]


Pd <- c(rep("01-07-2011", 10), rep("01-08-2011",15), rep("01-09-2011", 20), rep("01-10-2011", 20), rep("01-01-2012", 10),
        rep("01-02-2012", 20), rep("01-03-2012", 20), rep("01-04-2012", 20), rep("01-05-2012",20), rep("01-07-2012",20))

Pd = as.POSIXct(Pd, format = "%d-%m-%Y")
chla.mod = cbind(chla.mod, Pd)


#merging the full datetime file 
#mylist <- list(tempHV, datetime)
#tempHV <- do.call(rbind.fill, mylist)
#mylist <- list(temp9, datetime)
#temp9 <- do.call(rbind.fill, mylist)
#mylist <- list(temp6, datetime)
#temp6 <- do.call(rbind.fill, mylist)
#mylist <- list(temp14, datetime)
#temp14 <- do.call(rbind.fill, mylist)
#mylist <- list(light, datetime)
#light <- do.call(rbind.fill, mylist)
#mylist <- list(light.est, datetime)
#light.est <- do.call(rbind.fill, mylist)
#rm(mylist)

#make hourly means
#tempHVhr_d <- data.frame(tempHV$Pd, tempHV$Hver_tempC)
#names(tempHVhr_d) <- c("time", "tempHV")

#temp9hr_d <- data.frame(temp9$Pd, temp9$st9_tempC)
#names(temp9hr_d) <- c("time", "temp9")

#temp6hr_d <- data.frame(temp6$Pd, temp6$st6_tempC)
#names(temp6hr_d) <- c("time", "temp6")

#temp14hr_d <- data.frame(temp14$Pd, temp14$st14_tempC)
#names(temp14hr_d) <- c("time", "temp14")

#lighthr_d <- data.frame(light$Pd, light$Intensity)
#names(lighthr_d) <- c("time", "light")

#light.esthr_d <- data.frame(light.est$Pd, light.est$light)
#names(light.esthr_d) <- c("time", "light.est")




