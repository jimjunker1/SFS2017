##R script for completing the analyses and figures SFS 2017

#set the working directory

setwd("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017")

#set options

options(stringsAsFactors = F)
options(max.print = 1000000)

#load required packages
library(plyr)
library(dplyr)
library(ggplot2)
library(chron)

#set ggplot theme
source("C:/Users/Jim/Documents/Projects/General Files/General R scripts/theme_black.txt")
theme_set(theme_bw(20))
theme_set(theme_black(18))
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

#neeed to aggregate temp and light to daily
temp9_d = aggregate(temp9["st9_tempC"], list(day = cut(temp9$Pd, breaks ="day ")), mean, na.rm = T)
temp6_d = aggregate(temp6["st6_tempC"], list(day = cut(temp6$Pd, breaks = "day")), mean, na.rm = T)
temp14_d = aggregate(temp14["st14_tempC"], list(day = cut(temp14$Pd, breaks = "day")), mean, na.rm = T)
tempHV_d = aggregate(tempHV["Hver_tempC"], list(day = cut(tempHV$Pd, breaks = "day")), mean, na.rm = T)
temp7_d = aggregate(temp7["L_tempC"], list(day = cut(temp7$Pd, breaks = "day")), mean, na.rm = T)

lighthr_d <- aggregate(light["Intensity"], list(day = cut(light$Pd, breaks = "day")),mean, na.rm = T)
light.esthr_d <- aggregate(light.est["light"], list(day = cut(light.est$Pd, breaks = "day")),mean, na.rm = T)

#reassign the Pd to a POSIX object
temp9_d$Pd <- as.POSIXct(temp9_d$day, format = "%Y-%m-%d", tz = "UTC")
temp6_d$Pd <- as.POSIXct(temp6_d$day, format = "%Y-%m-%d", tz = "UTC")
temp14_d$Pd <- as.POSIXct(temp14_d$day, format = "%Y-%m-%d", tz = "UTC")
temp7_d$Pd <- as.POSIXct(temp7_d$day, format = "%Y-%m-%d", tz = "UTC")
tempHV_d$Pd <- as.POSIXct(tempHV_d$day, format = "%Y-%m-%d", tz = "UTC")

lighthr_d$Pd <- as.POSIXct(lighthr_d$day, format = "%Y-%m-%d", tz = "UTC")
light.esthr_d$Pd <- as.POSIXct(light.esthr_d$day, format = "%Y-%m-%d", tz = "UTC")

#merge all the data frames to build one.
temp_light <- merge(tempHV_d[,2:3], temp9_d[,2:3], by = "Pd", all = T)
temp_light <- merge(temp_light, temp7_d[,2:3], by = "Pd", all = T)
temp_light <- merge(temp_light, temp6_d[,2:3], by = "Pd", all = T)
temp_light <- merge(temp_light, temp14_d[,2:3], by = "Pd", all = T)
temp_light <- merge(temp_light, lighthr_d[,c(2:3)], by = "Pd", all = T)
temp_light <- merge(temp_light, light.esthr_d[,c(2:3)], by = "Pd", all = T)

#removign all the unnecessary files

rm("datetime", "light", "light.est", "light.esthr_d", "lighthr_d", "temp", "temp14", "temp14_d", "temp6", "temp6_d", "temp9", "temp9_d", "temp7", "temp7_d", "tempHV", "tempHV_d")

#subsetting the posix data

temp_light11.12 <- subset(temp_light, Pd >= as.POSIXct('2011-06-01 00:00:00') & Pd <= as.POSIXct('2012-09-01 00:00:00'))
summary(temp_light11.12)

head(temp_light11.12)
tail(temp_light11.12)


#creating a data file with just the montly temperature data
temp_light.m = aggregate(temp_light11.12[c("Hver_tempC", "st9_tempC", "st6_tempC", "st14_tempC", "L_tempC", "Intensity", "light")], list(month=cut(temp_light11.12$Pd, breaks = "month")), mean, na.rm = T)

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
 
#final plots of temp and light
theme_set(theme_bw(18))
L.plot = ggplot(temp_light11.12, aes(x = Pd)) + geom_line(aes(y = Intensity/1000)) + xlab("Date") + ylab("Intensity (10^3 lx)") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank());L.plot

T.plot = ggplot(temp_light11.12, aes(x = Pd)) + geom_line(aes(y = Hver_tempC), colour = "grey") +
    geom_line(aes(y = st9_tempC), colour = "light blue") +
   geom_line(aes(y = st6_tempC), colour = "red") +
   geom_line(aes(y = st14_tempC), colour = "blue") +
   geom_line(aes(y = L_tempC), colour = "yellow") +
  scale_y_continuous(position = "right") + scale_x_chron(labels = NULL) +
   xlab("") + ylab("Temperature (C)") + theme(plot.background = element_rect(fill = "transparent", colour = NA), panel.background = element_rect(fill = "transparent", colour = NA), 
                                              axis.ticks.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank());T.plot

#######

library(gtable)
library(grid)

g1 = ggplot_gtable(ggplot_build(L.plot))
g2 = ggplot_gtable(ggplot_build(T.plot)) 

pp = c(subset(g1$layout, name == "panel", se = t:r))
#g = gtable_add_grob(g1, list(g2$grobs[[which(g2$layout$name == "panel")]], g2$grobs[[which(g2$layout$name == "ylab-r")]]), pp$t, pp$l, pp$b, pp$l)
g = gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)

#axis tweaks
ia = which(g2$layout$name == "axis-r")
ga = g2$grobs[[ia]]
ax = ga$children[[2]]
#ax$widths = rev(ax$widths)
#ax$grobs = rev(ax$grobs)
ax$grobs[[1]]$x = ax$grobs[[1]]$x - unit(0.5, "npc") + unit(0.15, "cm")
g = gtable_add_cols(g, g2$widths[g2$layout[ia,]$l], length(g$widths)- 1)
g = gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
g = gtable_add_cols(g, width = unit(1, "cm"), length(g$widths))
#grid.draw(g)


#axis label
ib = which(g2$layout$name == "ylab-r")
gb = g2$grobs[[ib]]
bx = gb$children[[1]]
bx$width
g = gtable_add_grob(g, bx, pp$t, length(g$width) - 1, pp$b, z = Inf)
grid.draw(g)

##

g1 = ggplotGrob(L.plot)
#g1 = gtable_add_cols(g1, unit(0, "mm"))
g2 = ggplotGrob(T.plot)
g = rbind(g1, g2, size = "first")
g$widths = unit.pmax(g1$widths, g2$widths) 
g$layout[grep("guide", g$layout$name), c("t", "b")] <- c(1,nrow = 1)
grid.newpage()
grid.draw(rbind(g1, g2, size = "last"))
#what is the correlation between light and temperature
 
 cor(temp_light11.12$Intensity, temp_light11.12$Hver_tempC, use = "pairwise.complete.obs")
 cor(temp_light11.12$Intensity, temp_light11.12$st9_tempC, use = "pairwise.complete.obs")
 cor(temp_light11.12$Intensity, temp_light11.12$st6_tempC, use = "pairwise.complete.obs")
 cor(temp_light11.12$Intensity, temp_light11.12$st14_tempC, use = "pairwise.complete.obs")
 cor(temp_light11.12$Intensity, temp_light11.12$L_tempC, use = "pairwise.complete.obs")
 
#Now bring in epilithon data
 
chla <- read.csv(file = "C:/Users/Jim/Documents/Projects/Iceland/Epilithon/Chla.csv", T)
epi <- read.csv(file = "C:/Users/Jim/Documents/Projects/Iceland/Epilithon/IcelandEPI.csv", T)

chla.mod = chla[which(chla$Stream == "ST6" | chla$Stream == "ST9" | chla$Stream == "ST14" | chla$Stream == "Hver"),]
epi.mod = epi[which(epi$Stream == "ST6" | epi$Stream == "ST9" | epi$Stream == "ST14" | epi$Stream == "Hver"),]

Pd <- c(rep("01-07-2011", 10), rep("01-08-2011",15), rep("01-09-2011", 20), rep("01-10-2011", 20), rep("01-01-2012", 10),
        rep("01-02-2012", 20), rep("01-03-2012", 20), rep("01-04-2012", 20), rep("01-05-2012",20), rep("01-07-2012",20))

chla.Pd = as.POSIXct(Pd, format = "%d-%m-%Y")

chla.mod$temp_k = chla.mod$Temp+273.15
chla.mod = cbind(chla.mod, chla.Pd)

colnames(chla.mod) = c("Date", "Stream", "Temp", "chla", "Pd")

Pd = c(rep("01-10-2011", 20), rep("01-02-2012", 20), rep("01-05-2012", 20), rep("01-07-2012", 20))
epi.Pd = as.POSIXct(Pd, format = "%d-%m-%Y")

epi.mod = cbind(epi.mod, epi.Pd)

theme_set(theme_classic())
ggplot(chla.mod, aes(x = Temp, y = chla)) + geom_point(aes(colour = Stream)) +
  stat_smooth(aes(x = Temp, y = chla), method = "lm", se = F)


#Time to import Dan's data and convert it to the correct form

warming_data = read.csv(file = "C:/Users/Jim/Documents/Projects/Iceland/Bug Samples/Secondary Production/Final data files/Dan_abundance.csv", T, check.names = F)

#converting the dates to POSIX
warming_data$Pd = as.POSIXct(warming_data$DATE, format = "%d-%b-%Y", tz = "UTC")

#subsetting the dates to relevant frame

warming_sub = subset(warming_data, Pd >= as.POSIXct('2010-10-25') & Pd <= as.POSIXct('2011-10-25'))
#warming_sub = as.data.frame(warming_sub,)
#changing the stream names

st7.fix=which(warming_sub$STREAM == "7")
warming_sub[st7.fix, "STREAM"] <- "ST7"

#pare down the columns to relate to similar structure on 

warming_sub = warming_sub[,c(5,6,3,162,11,12,16:160)]

large_spp = which(warming_sub[,146] > 0) #checking what the species that is sooo huge

year = as.numeric(as.character(years(chron(dates = as.character(warming_sub$Pd)))))
month = as.numeric(months(chron(dates = as.character(warming_sub$Pd))))
day = as.numeric(days(chron(dates = as.character(warming_sub$Pd))))

JULIAN = julian(month, day, year, origin = c(month = 01, day = 01, year = 2010))

## now reads in file for secondary production of files ##

#load in the merge function ##

source("C:/Users/Jim/Documents/Projects/Iceland/Bug Samples/Secondary Production/Secondary Production R code suite/frac_merge/frac_merge_function.txt")

## Load in the full file of sample counts ##

bugs_fin <- read.table("C:/Users/Jim/Documents/Projects/Iceland/Bug Samples/Secondary Production/Final data files/SamplesFull_fin.txt", header = T, sep = "\t", quote = "", strip.white = T, check.names = F)
bugs_fin = bugs_fin[,c(1:47)]

frac_merge(bugs_fin, "hengill")

hengill_merge <- read.table(file = './hengill_merged.txt', header= T, sep = "\t", quote = "", strip.white = T, check.names = F, row.names = 1)

#str(hengill_merge)

st9_bugs = hengill_merge[which(hengill_merge$SITE == "ST9"),]
st6_bugs = hengill_merge[which(hengill_merge$SITE == "ST6"),]
st14_bugs = hengill_merge[which(hengill_merge$SITE == "ST14"),]

unique(hver_bugs$TAXON)






unique(st9_bugs$TAXON)
unique(st6_bugs$TAXON)
unique(st14_bugs$TAXON)

#########################

#maxlength = warming_sub[, (colSums(warming_sub == 0) < (dim(warming_sub)[1]))]

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




