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
library(lubridate)
library(chron)
#install.packages("nloptr")
#install.packages("car")
#library(nloptr)
#library(car)
library(reshape2)
library(tidyr)

#set ggplot theme
#source("C:/Users/Jim/Documents/Projects/General Files/General R scripts/theme_black.txt")
#theme_set(theme_bw(20))
#theme_set(theme_black(18))
theme_set(theme_classic(20))
#import the data for 
##Time, temp, and light
##load in color scheme
cbbPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#######
datetime <- read.csv(file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/All_DateTime.csv", T)
temp <- read.csv(file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/temp_depth_all.csv", T)
light.est <- read.csv(file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/Light-est_full.csv", T)
light <- read.csv(file = "C:/Users/Jim/Documents/Projects/Iceland/Temp-Disch-Light/Working Q/Do Not Touch/Light_full.csv", T)

#subset the selected streams
#temp7 <- temp[,c(1,2)]
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
#temp7$Pd <- as.POSIXct(temp7$time, format = "%d-%m-%y %H:%M", tz = "UTC")
light$Pd <- as.POSIXct(paste(light$Date, light$Time),format = "%m/%d/%y %H:%M:%S", tz = "UTC")
light.est$Pd <- as.POSIXct(paste(light.est$date, light.est$time), format = "%m/%d/%y %H:%M:%S", tz = "UTC")

#neeed to aggregate temp and light to daily
temp9_d = aggregate(temp9["st9_tempC"], list(day = cut(temp9$Pd, breaks ="day ")), mean, na.rm = T)
temp6_d = aggregate(temp6["st6_tempC"], list(day = cut(temp6$Pd, breaks = "day")), mean, na.rm = T)
temp14_d = aggregate(temp14["st14_tempC"], list(day = cut(temp14$Pd, breaks = "day")), mean, na.rm = T)
tempHV_d = aggregate(tempHV["Hver_tempC"], list(day = cut(tempHV$Pd, breaks = "day")), mean, na.rm = T)
#temp7_d = aggregate(temp7["L_tempC"], list(day = cut(temp7$Pd, breaks = "day")), mean, na.rm = T)

lighthr_d <- aggregate(light["Intensity"], list(day = cut(light$Pd, breaks = "day")),mean, na.rm = T)
light.esthr_d <- aggregate(light.est["light"], list(day = cut(light.est$Pd, breaks = "day")),mean, na.rm = T)

#reassign the Pd to a POSIX object
temp9_d$Pd <- as.POSIXct(temp9_d$day, format = "%Y-%m-%d", tz = "UTC")
temp6_d$Pd <- as.POSIXct(temp6_d$day, format = "%Y-%m-%d", tz = "UTC")
temp14_d$Pd <- as.POSIXct(temp14_d$day, format = "%Y-%m-%d", tz = "UTC")
#temp7_d$Pd <- as.POSIXct(temp7_d$day, format = "%Y-%m-%d", tz = "UTC")
tempHV_d$Pd <- as.POSIXct(tempHV_d$day, format = "%Y-%m-%d", tz = "UTC")

lighthr_d$Pd <- as.POSIXct(lighthr_d$day, format = "%Y-%m-%d", tz = "UTC")
light.esthr_d$Pd <- as.POSIXct(light.esthr_d$day, format = "%Y-%m-%d", tz = "UTC")

#merge all the data frames to build one.
temp_light <- merge(tempHV_d[,2:3], temp9_d[,2:3], by = "Pd", all = T)
#temp_light <- merge(temp_light, temp7_d[,2:3], by = "Pd", all = T)
temp_light <- merge(temp_light, temp6_d[,2:3], by = "Pd", all = T)
temp_light <- merge(temp_light, temp14_d[,2:3], by = "Pd", all = T)
temp_light <- merge(temp_light, lighthr_d[,c(2:3)], by = "Pd", all = T)
temp_light <- merge(temp_light, light.esthr_d[,c(2:3)], by = "Pd", all = T)

#removign all the unnecessary files

rm("datetime", "light", "light.est", "light.esthr_d", "lighthr_d", "temp", "temp14", "temp14_d", "temp6", "temp6_d", "temp9", "temp9_d", "tempHV", "tempHV_d")

#subsetting the posix data

temp_light11.12 <- subset(temp_light, Pd >= as.POSIXct('2011-06-01 00:00:00') & Pd <= as.POSIXct('2012-11-01 00:00:00'))
temp_light11.12$doy = yday(temp_light11.12$Pd)

summary(temp_light11.12)

head(temp_light11.12)
tail(temp_light11.12)

# import ST7 and OH2 temperatures 
st7_temp = read.csv(file = "./st7_temp.csv", T)
oh2_temp = read.csv(file = "./OH2_temp.csv", T)

st7_temp$Pd <- as.POSIXct(st7_temp$DATE, format = "%m/%d/%y", tz = "UTC")
oh2_temp$Pd <- as.POSIXct(oh2_temp$DATE, format = "%m/%d/%y", tz = "UTC")

st7_temp_d = aggregate(st7_temp["Temp"], list(month = cut(st7_temp$Pd, breaks = "month")), mean, na.rm = T)
oh2_temp_d = aggregate(oh2_temp["temp"], list(month = cut(oh2_temp$Pd, breaks = "month")), mean, na.rm = T)

warming_temp = merge(st7_temp_d, oh2_temp_d, by = c("month"), all = T)
######  
warming_temp$Pd = as.POSIXct(warming_temp$month, format = "%Y-%m-%d", tz = "UTC")
#####
temp_light10.11 <- subset(warming_temp, Pd >= as.POSIXct('2010-10-01') & Pd <= as.POSIXct('2011-11-01'))

colnames(temp_light10.11) = c("month", "st7_temp", "oh2_temp", "Pd")

st7_temp10.11 = subset(st7_temp, Pd >= as.POSIXct('2010-10-01') & Pd <= as.POSIXct('2011-11-01'))
oh2_temp10.11 = subset(oh2_temp, Pd >= as.POSIXct('2010-10-01') & Pd <= as.POSIXct('2011-11-01'))

st7_tempC = mean(st7_temp10.11[,2])
oh2_tempC = mean(oh2_temp10.11[,6])

#creating a data file with just the montly temperature data
## grab month variable fomr both 

temp_light.m = aggregate(temp_light11.12[c("Hver_tempC", "st9_tempC", "st6_tempC", "st14_tempC", "Intensity", "light")], list(month=cut(temp_light11.12$Pd, breaks = "month")), mean, na.rm = T)
temp_merge = merge(temp_light.m , temp_light10.11, by = c("month"), all = T)

temp_merge = temp_merge[,-10]
temp_merge$m = month(as.POSIXct(temp_merge$month, format = "%Y-%m-%d"))

temp_light.annual = subset(temp_light, Pd >= as.POSIXct('2011-07-01 00:00:00') & Pd <= as.POSIXct('2012-07-01 00:00:00'))
temp_light.y = apply(temp_light.annual[,2:(dim(temp_light.annual)[2])], 2, mean, na.rm = T)

saveRDS(temp_merge, file = "temp_light.m.rda")
saveRDS(temp_light.annual, file = "temp_light.annual.rda")
saveRDS(temp_light.y, file = "temp_light.y.rda")

#plotting the data to look at the holes. #####
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

##########  
#final plots of temp and light

temp_light_mod = subset(temp_light11.12, Pd >= as.POSIXct("2011-10-27") & Pd <= as.POSIXct("2012-10-26"))
temp_light_mod$doy = yday(temp_light_mod$Pd)
warming_full = merge(st7_temp, oh2_temp, by = c("Pd"), all = T)
warming_full$doy = yday(warming_full$Pd)
warming_mod = subset(warming_full, Pd >= as.POSIXct('2010-10-27') & Pd <= as.POSIXct('2011-10-26'))

temp_light11.12$my = format(as.Date(temp_light11.12$Pd), "%m")
L.plot = ggplot(temp_light_mod, aes(x = doy)) + geom_line(aes(y = Intensity/1000), size = 4) + xlab("Date") + ylab("Intensity") + 
  xlab("Date") + ylab("Light intensity") +
  theme(axis.title = element_text(size = 24), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank());L.plot

png(file = "light_plot.png", res = 300, height = 10, width = 35, unit = "in")
L.plot
dev.off()

T.plot = ggplot(temp_light_mod, aes(x = doy)) + geom_line(aes(y = Hver_tempC), colour = "#000000", size = 4) +
  geom_line(aes(y = st9_tempC), colour = "#56B4E9", size = 4) +
  geom_line(aes(y = st6_tempC), colour = "#E69F00", size = 4) +
  geom_line(aes(y = st14_tempC), colour = "#0072B2", size = 4) +
  geom_line(data = warming_mod, aes(y = Temp), colour = "#F0E442", size = 4) +
  geom_line(data = warming_mod, aes(y = temp), colour = "#009E73", size = 4) +
  scale_y_continuous() + #scale_x_chron(labels = NULL) +
  xlab("Date") + ylab("Temperature (C)") + 
  theme(axis.title = element_text(size = 24), axis.text = element_text(size = 20), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank());T.plot

png(file = "temp_plot.png", res = 300, height = 10, width = 35, unit = "in")
T.plot
dev.off()

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

#chla.mod$temp_k = chla.mod$Temp+273.15
chla.mod = cbind(chla.mod, chla.Pd)
chla.mod$my = format(as.Date(chla.Pd), "%Y-%m")
chla.mod$doy = yday(chla.mod$Pd)
colnames(chla.mod) = c("Date", "Stream", "Temp", "chla", "Pd", "my", "doy")

Pd = c(rep("01-10-2011", 20), rep("01-02-2012", 20), rep("01-05-2012", 20), rep("01-07-2012", 20))
epi.Pd = as.POSIXct(Pd, format = "%d-%m-%Y")

epi.mod = cbind(epi.mod, epi.Pd)

theme_set(theme_classic())
ggplot(chla.mod, aes(x = Temp, y = chla)) + geom_point(aes(colour = Stream)) +
  stat_smooth(aes(x = Temp, y = chla), method = "lm", se = F)

ggplot(chla.mod, aes(x = doy, y = chla, group = Stream, colour = Stream)) + geom_point(size = 8) +
  stat_smooth(method = "loess", se = F)

ggplot(epi.mod, aes( x = epi.Pd, y = AFDM, group = Stream, colour = Stream)) + geom_point(size = 8)

chla.m = ddply(chla.mod, .(Stream, my), summarize, chla.m = mean(chla, na.rm = T))
######  
#Time to import Dan's data and convert it to the correct form
library(chron)
options(stringsAsFactors = F)
warming_data = read.csv(file = "C:/Users/Jim/Documents/Projects/Iceland/Bug Samples/Secondary Production/Final data files/Dan_abundance.csv", T, check.names = F)

#changing the stream names
st7.fix=which(warming_data$STREAM == "7")
warming_data[st7.fix, "STREAM"] <- "ST7"
#converting the dates to POSIX and correct format

warming_data$DATE = as.Date(warming_data$DATE, format = "%d-%b-%Y")
warming_data$DATE = format(warming_data$DATE, "%m/%d/%y")
warming_data$Pd = as.POSIXct(warming_data$DATE, format = "%m/%d/%y", tz = "UTC")
### insert JULIAN dates ###
#subsetting the dates to relevant frame

warming_sub = subset(warming_data, Pd >= as.POSIXct('2010-10-25') & Pd <= as.POSIXct('2011-10-25'))
#pare down the columns to relate to similar structure on 
warming_sub$HABITAT = rep("COBBLE", nrow(warming_sub))

warming_sub = warming_sub[,c(5,6,3,163,12,16:160)]

names(warming_sub)
colnames(warming_sub)[1:5] = c("SITE", "SAMPLE", "DATE", "HABITAT", "TAXON") 

#warming_data = as.data.frame(unclass(warming_data))
########### now reads in file for secondary production of files ########

#load in the merge function ##

#source("C:/Users/Jim/Documents/Projects/Iceland/Bug Samples/Secondary Production/Secondary Production R code suite/frac_merge/frac_merge_function.txt")

## Load in the full file of sample counts ##

#bugs_fin <- read.table("C:/Users/Jim/Documents/Projects/Iceland/Bug Samples/Secondary Production/Final data files/SamplesFull_fin.txt", header = T, sep = "\t", quote = "", strip.white = T, check.names = F)
#bugs_fin = bugs_fin[,c(1:47)]

#frac_merge(bugs_fin, "hengill")

########  This has already been run ######

library(plyr)
#load other Hengill file
hengill_merge <- read.table(file = './hengill_merged.txt', header= T, sep = "\t", quote = "", strip.white = T, check.names = F, row.names = 1)
names(hengill_merge)
#merge warming_sub and hengill_merge

hengill_full = rbind.fill(hengill_merge, warming_sub) 
names(hengill_full)
hengill_full= hengill_full[!is.na(hengill_full$TAXON),]
hengill_full[is.na(hengill_full)] = 0 #this is the new complete file to run before everything 
names(hengill_full)
hengill_full = hengill_full[,colSums(hengill_full != 0) >0]
#names(hengill_full)

write.table(hengill_full, file = "hengill_full.txt", sep = "\t", quote = F, append = F)

#########################

## Now bring in all the annual estimates of production and temperature
#temp_light.y
#st14.prod
#st7.prod
#st6.prod
#st9.prod
#hver.prod  need to adjust the growth rates. 80 P/B not realistic

#######  run these if need to run production again #####
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/get.dates_function.txt")
source(file = "C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/hver_prod.R")
source(file = "C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/st9_prod.R")
source(file = "C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/st6_prod.R")
source(file = "C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/st7_prod.R")
source(file = "C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/st14_prod.R")
source(file = "C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/oh2_prod.R")

####Individual production rank-produciton ####

st14.spp.ann.prod = readRDS(file = "st14.spp.ann.prod.rda")
st6.spp.ann.prod = readRDS(file = "st6.spp.ann.prod.rda")
st9.spp.ann.prod = readRDS(file = "st9.spp.ann.prod.rda")
st7.spp.ann.prod = readRDS(file = "st7.spp.ann.prod.rda")
hver.spp.ann.prod = readRDS(file = "hver.spp.ann.prod.rda")
oh2.spp.ann.prod = readRDS(file = "oh2.spp.ann.prod.rda")

st14.spp.ann.bio = readRDS(file = "st14.spp.ann.bio.rda")
st6.spp.ann.bio = readRDS(file = "st6.spp.ann.bio.rda")
st9.spp.ann.bio = readRDS(file = "st9.spp.ann.bio.rda")
st7.spp.ann.bio = readRDS(file = "st7.spp.ann.bio.rda")
hver.spp.ann.bio = readRDS(file = "hver.spp.ann.bio.rda")
oh2.spp.ann.bio = readRDS(file = "oh2.spp.ann.bio.rda")

######
#Interval production and temperature
#run these if we want to re-run production#####

temp_light.m = readRDS(file = "temp_light.m.rda")
temp_light.annual = readRDS(file = "temp_light.annual.rda")
temp_light.y = readRDS(file = "temp_light.y.rda")

hver.out = readRDS(file = "hver_out.rda")
st14.out = readRDS(file = "st14_out.rda")
st9.out = readRDS(file = "st9_out.rda")
st6.out = readRDS(file = "st6_out.rda")
st7.out = readRDS(file = "st7_out.rda")
oh2.out = readRDS(file = "oh2_out.rda")

hver.int = readRDS(file = "hver.int.rda")
st9.int = readRDS(file = "st9.int.rda")
st6.int = readRDS(file = "st6.int.rda")
st7.int = readRDS(file = "st7.int.rda")
st14.int = readRDS(file = "st14.int.rda")
oh2.int = readRDS(file = "oh2.int.rda")

######

### Need to adjust interval production for interval length ###
## load in the bugs data sets ###

hver_bugs = readRDS(file = "hver_bugs.rda")
st14_bugs = readRDS(file = "st14_bugs.rda")
st9_bugs = readRDS(file = "st9_bugs.rda")
st6_bugs = readRDS(file = "st6_bugs.rda")
st7_bugs = readRDS(file = "st7_bugs.rda")
oh2_bugs = readRDS(file = "oh2_bugs.rda")

## find the time of each interval for each stream ##
#Create a vector of sampling dates (in Julian units):
#HVER
hv.t <- sort(union(hver_bugs$JULIAN, NULL))
hvt.int <- diff(hv.t)
hvt.int <- c(hvt.int, (365-sum(hvt.int)))	

hver.int = cbind(hver.int, hvt.int)
#ST14
st14.t = sort(union(st14_bugs$JULIAN, NULL))
st14t.int = diff(st14.t)
st14t.int <- c(st14t.int, (365-sum(st14t.int)))	

st14.int = cbind(st14.int, st14t.int)
#ST6
st6.t = sort(union(st6_bugs$JULIAN, NULL))
st6t.int = diff(st6.t)
st6t.int <- c(st6t.int, (365-sum(st6t.int)))	

st6.int = cbind(st6.int, st6t.int)
#ST9
st9.t = sort(union(st9_bugs$JULIAN, NULL))
st9t.int = diff(st9.t)
st9t.int <- c(st9t.int, (365-sum(st9t.int)))	

st9.int = cbind(st9.int, st9t.int)
#ST7
st7.t = sort(union(st7_bugs$JULIAN, NULL))
st7t.int = diff(st7.t)
st7t.int <- c(st7t.int, (365-sum(st7t.int)))	

st7.int = cbind(st7.int, st7t.int)
#OH2
oh2.t = sort(union(oh2_bugs$JULIAN, NULL))
oh2t.int = diff(oh2.t)
oh2t.int <- c(oh2t.int, (365-sum(oh2t.int)))	

oh2.int = cbind(oh2.int, oh2t.int)
####### standardize the production to daily ######

hver.int = mutate(hver.int, int_prod = Production/hvt.int ) 
colnames(hver.int)= c("Stream", "month", "Temperature", "Production", "t.int", "int_prod")
st14.int = mutate(st14.int, int_prod = Production/st14t.int)
colnames(st14.int)= c("Stream", "month", "Temperature", "Production", "t.int", "int_prod")
st9.int = mutate(st9.int, int_prod = Production/st9t.int)
colnames(st9.int)= c("Stream", "month", "Temperature", "Production", "t.int", "int_prod")
st6.int = mutate(st6.int, int_prod = Production/st6t.int)
colnames(st6.int)= c("Stream", "month", "Temperature", "Production", "t.int", "int_prod")
st7.int = mutate(st7.int, int_prod = Production/st7t.int)
colnames(st7.int)= c("Stream", "month", "Temperature", "Production", "t.int", "int_prod")
oh2.int = mutate(oh2.int, int_prod = Production/oh2t.int)
colnames(oh2.int)= c("Stream", "month", "Temperature", "Production", "t.int", "int_prod")


######
hver.prod = hver.out$Pboots.cob
hver.bio = hver.out$Bboots.cob

hver.ann.prod = apply(hver.prod, 1, sum, na.rm = T)
hver.ann.bio = apply(hver.bio, 1, sum, na.rm = T)

oh2.prod = oh2.out$Pboots.cob
oh2.bio = oh2.out$Bboots.cob

oh2.ann.prod = apply(oh2.prod, 1, sum, na.rm = T)
oh2.ann.bio = apply(oh2.bio, 1, sum, na.rm = T)

st14.prod = st14.out$Pboots.cob
st14.bio = st14.out$Bboots.cob

st14.ann.prod = apply(st14.prod, 1, sum, na.rm = T)
st14.ann.bio = apply(st14.bio, 1, sum, na.rm = T)

st6.prod = st6.out$Pboots.cob
st6.bio = st6.out$Bboots.cob

st6.ann.prod = apply(st6.prod, 1, sum, na.rm = T)
st6.ann.bio = apply(st6.bio, 1, sum, na.rm = T)

st7.prod = st7.out$Pboots.cob
st7.bio = st7.out$Bboots.cob

st7.ann.prod = apply(st7.prod, 1, sum, na.rm = T)
st7.ann.bio = apply(st7.bio, 1, sum, na.rm = T)

st9.prod = st9.out$Pboots.cob
st9.bio = st9.out$Bboots.cob

st9.ann.prod = apply(st9.prod, 1, sum, na.rm = T)
st9.ann.bio = apply(st9.bio, 1, sum, na.rm = T)
######

hver.ann.prod = apply(hver.prod, 1, sum, na.rm = T)
st14.ann.prod = apply(st14.prod, 1, sum, na.rm = T)
st6.ann.prod = apply(st6.prod, 1, sum, na.rm = T)
st7.ann.prod = apply(st7.prod, 1, sum, na.rm = T)
st9.ann.prod = apply(st9.prod, 1, sum, na.rm = T)
oh2.ann.prod = apply(oh2.prod, 1, sum, na.rm = T)

######
temp_light.y
production = c(mean(hver.ann.prod), mean(st9.ann.prod), mean(st6.ann.prod), mean(st14.ann.prod),mean(st7.ann.prod), mean(oh2.ann.prod))
biomass = c(mean(hver.ann.bio), mean(st9.ann.bio), mean(st6.ann.bio), mean(st14.ann.bio), mean(st7.ann.bio), mean(oh2.ann.bio))
Stream = c(names(temp_light.y[c(1:4)]), "st7", "oh2")
temps = c(temp_light.y[c(1:4)], st7_tempC, oh2_tempC)
annual.prod = data.frame(Stream, production, biomass, temps)

colnames(annual.prod) = c("Stream", "Production", "Biomass", "Temperature")

ann_plot = ggplot(annual.prod, aes(x = Temperature, y = Production)) +stat_smooth(method = "lm", se = F, colour = "black", size = 4) +
  geom_point( shape = 21, fill = "red", colour = "black", stroke = 1.2, size = 10) +
    xlab("Mean annual Temperature (C)") + ylab("Annual production [mg m-2 yr-1]") + scale_x_continuous(limits = c(0,32)) +
  theme(axis.title = element_text(size = 24), axis.text = element_text(size = 20))
ann_plot
ann.lm = lm(Production~Temperature, data = annual.prod);summary(ann.lm)

png(file = "ann_plot.png", res = 300, height = 10, width = 10, unit = "in")
ann_plot
dev.off()

ggplot(annual.prod, aes(x = Temperature, y = Biomass, label = Stream)) + geom_point() + geom_text(aes(label = Stream)) + stat_smooth(method = "lm", se = T) +
  xlab("Mean annual Temperature (C)") + ylab("Annual biomass [mg m-2]") + scale_x_continuous(limits = c(0,35))

##try to read in the species specific boots #####

prod.int = rbind(hver.int, st9.int, st6.int, st7.int, st14.int, oh2.int)
prod.int = merge(prod.int, temp_merge[,c(1:9)], by = "month")
prod.int$DATE = as.Date(prod.int$month, format = "%Y-%m-%d")
prod.int$my = format(as.Date(prod.int$DATE), "%m")

prod.int.merge = merge(prod.int, chla.m, by = c("Stream", "my"), all = T)
prod.int.merge = prod.int.merge[! is.na(prod.int.merge$month),]

prod.int$Stream = factor(prod.int$Stream, levels = c("Hver", "ST6", "ST9", "ST7", "OH2", "ST14"), labels = c("Hver", "ST6", "ST9", "ST7", "OH2", "ST14"))
int_prod = ggplot(prod.int, aes(x = Temperature, y = Production, group = Stream, colour = Stream)) + stat_smooth(method = "lm", se = F, size = 4) +
  geom_point(aes(fill = Stream), shape = 21, colour = "black", stroke = 1.2, size = 10) + 
  xlab("Mean interval temperature (C)") + ylab("Interval production [mg m-2 interval-1]") +
  scale_color_manual(values = cbbPalette) + scale_fill_manual(values = cbbPalette) +
  theme(legend.position = c(0.15, 0.8), legend.title = element_blank(), legend.text = element_text(size = 20), axis.title = element_text(size = 24), 
        axis.text = element_text(size = 20))
int_prod 

png(file = "int_prod.png", res = 300, width = 10, height = 10, units = "in") 
int_prod
dev.off()
  
lin.mod = lm(Production~Temperature * Stream, data = prod.int);summary(lin.mod)

### now plot by standardized production ###

stand_prod = ggplot(prod.int, aes(x = Temperature, y = int_prod, group= Stream, colour = Stream)) + stat_smooth(method = "lm", se = F, size = 4) +
  geom_point(aes(fill = Stream), shape = 21, colour = "black", stroke = 1.2, size = 10) +
  xlab("Mean interval temperature (C)") + ylab("Interval production [mg AFDM m-2 d-1]") +
  scale_color_manual(values = cbbPalette) + scale_fill_manual(values = cbbPalette) +
  theme(legend.position = c(0.15, 0.8), legend.title = element_blank(), legend.text = element_text(size = 20), axis.title = element_text(size = 24),
        axis.text = element_text(size = 20))
stand_prod

######
streams = c("Hver", "ST6", "ST9", "ST7", "OH2", "ST14")
temps = c(temp_light.y[1], temp_light.y[3], temp_light.y[2], st7_tempC, oh2_tempC, temp_light.y[4])
coefs = lin.mod$coefficients
int_slope = c(coefs[[2]], coefs[[2]]+coefs[[8]], coefs[[2]]+coefs[[9]], coefs[[2]]+coefs[[10]], coefs[[2]]+coefs[[11]], coefs[[2]]+coefs[[12]])
int.df = data.frame(streams, temps, int_slope)

ggplot(int.df[c(1:3,5:6),], aes(x = temps, y = int_slope)) + geom_point(size = 10, stroke = 1.2,colour = "black", shape = 21, fill = "red") + 
  geom_point(data = int.df[4,], aes(x = temps, y = int_slope), stroke = 1.2,size = 10, shape = 21, colour = "black", fill = "white") +
  geom_smooth(method = "lm", se = F, size = 2, colour = "black") + scale_y_continuous(limits = c(20,350))  +
  xlab("Annual mean Temp (C)") + ylab("Interval production slope") +
  theme(legend.text = element_text(size = 20), legend.title = element_text(size = 24))

lin.mod = lm(Production~Temperature, data = prod.int); summary(lin.mod)

ggplot(prod.int, aes(x = Intensity, y = log(Production), group = Stream, colour = Stream)) + geom_point(aes(colour = Stream), size = 8) + geom_text(aes(label = month)) +
  stat_smooth(method = "lm", se = F)
lin.mod = lm(Production~Temperature*Intensity, data = prod.int);summary(lin.mod)

ggplot(prod.int.merge, aes(x = chla.m, y = Production, group = Stream)) + geom_point()
lin.mod = lm(Production~Temperature * chla.m, data = prod.int.merge);summary(lin.mod)

Temperature = prod.int.merge$Temperature
Intensity = prod.int.merge$Intensity

prod.int.merge$Stream = factor(prod.int.merge$Stream, levels = c("Hver", "ST6", "ST9", "ST7", "OH2", "ST14"), labels = c("Hver", "ST6", "ST9", "ST7", "OH2", "ST14"))
date_prod = ggplot(prod.int.merge, aes(x = my, y = Production, group = Stream, colour = Stream)) + stat_smooth(method = "loess", se = F, size = 4) +
  geom_point(aes(fill = Stream), shape = 21, stroke = 1.2, colour = "black", size = 10) +
  xlab( "Month") + ylab("Interval produciton [mg AFDM m-2]") +
  scale_color_manual(values = cbbPalette) + scale_fill_manual(values = cbbPalette) +
  theme(legend.position = c(0.85, 0.8), axis.title = element_text( size = 24), axis.text = element_text(size = 20), legend.text = element_text(size = 20), legend.title = element_blank())
date_prod

png(file = "date_prod.png", res = 300, width = 10, height = 10, units = "in") 
date_prod
dev.off()
### standardized production to daily rates ###
prod.int.merge$Stream = factor(prod.int.merge$Stream, levels = c("Hver", "ST6", "ST9", "ST7", "OH2", "ST14"), labels = c("Hver", "ST6", "ST9", "ST7", "OH2", "ST14"))
date_prod = ggplot(prod.int.merge, aes(x = my, y = int_prod, group = Stream, colour = Stream)) + stat_smooth(method = "loess", se = F, size = 4) +
  geom_point(aes(fill = Stream), shape = 21, stroke = 1.2, colour = "black", size = 10) +
  xlab( "Month") + ylab("Interval produciton [mg AFDM m-2 d-1]") +
  scale_color_manual(values = cbbPalette) + scale_fill_manual(values = cbbPalette) +
  theme(legend.position = c(0.85, 0.8), axis.title = element_text( size = 24), axis.text = element_text(size = 20), legend.text = element_text(size = 20), legend.title = element_blank())
date_prod

#build a dataframe with all streams
st14.spp.ann.prod$Stream = rep("ST14", nrow(st14.spp.ann.prod))
st6.spp.ann.prod$Stream = rep("ST6", nrow(st6.spp.ann.prod))
st9.spp.ann.prod$Stream = rep("ST9", nrow(st9.spp.ann.prod))
st7.spp.ann.prod$Stream = rep("ST7", nrow(st7.spp.ann.prod))
hver.spp.ann.prod$Stream = rep("Hver", nrow(hver.spp.ann.prod))
oh2.spp.ann.prod$Stream = rep("OH2", nrow(oh2.spp.ann.prod))


full.spp.ann.prod = rbind(hver.spp.ann.prod, st7.spp.ann.prod, st6.spp.ann.prod, st9.spp.ann.prod, st14.spp.ann.prod, oh2.spp.ann.prod)

full.spp.ann.prod$Stream = factor(full.spp.ann.prod$Stream, levels = c("Hver", "ST6", "ST9", "ST7", "OH2", "ST14"), labels = c("Hver", "ST6", "ST9", "ST7", "OH2", "ST14"))
prod_even = ggplot(full.spp.ann.prod %>% arrange(spp.rank), aes(x = spp.rank, y = log(Mean.Annual.Prod))) + 
  geom_point(aes(group = Stream, colour = Stream), size = 10) + geom_path(aes(group = Stream, colour = Stream), size = 4) +
  ylab("Log(Annual production [mg m-2 yr-1])") + xlab("Rank") + 
  #stat_smooth(aes(group = Stream, colour = Stream),  method = "lm", se = F) +
  scale_y_continuous(limits = c(-10,10)) +
  scale_color_manual(values = cbbPalette) + scale_fill_manual(values = cbbPalette) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.title = element_text(size = 24), axis.text = element_text(size = 20), legend.position = c(0.07, 0.15),
        legend.text = element_text(size = 20), legend.title = element_blank())
prod_even
png(file = "annual_evenness.png", res = 300, height = 10, width = 10, units = "in")
prod_even
dev.off()

even.lm = lm(log(Mean.Annual.Prod)~spp.rank*Stream, data = full.spp.ann.prod);summary(even.lm)

##### build up the production~temperature relationships with light and dark ####
## Sept - mar ##
## apr - aug ##
prod.int$my = as.numeric(prod.int$my)
prod.int = data.frame(unclass(prod.int))
prod.int.dark = prod.int[which(prod.int$my >= 1 & prod.int$my <= 3 | prod.int$my >= 10 & prod.int$my <= 12), ]
prod.int.light = prod.int[which(prod.int$my >= 4 & prod.int$my <= 9),]

prod.int.light$Stream = factor(prod.int.light$Stream, levels = c("Hver", "ST6", "ST9", "ST7", "OH2", "ST14"), labels = c("Hver", "ST6", "ST9", "ST7", "OH2", "ST14"))
prod.int.dark$Stream = factor(prod.int.dark$Stream, levels = c("Hver", "ST6", "ST9", "ST7", "OH2", "ST14"), labels = c("Hver", "ST6", "ST9", "ST7", "OH2", "ST14"))

dark_prod = ggplot(prod.int.dark, aes(x = Temperature, y = int_prod)) + stat_smooth(method = "lm", se = F, size = 3, colour = "grey", alpha = 0.3) +
  geom_point(shape = 21, fill = "black", colour = "black", size = 10) +
  scale_fill_manual(values = cbbPalette) + scale_color_manual(values = cbbPalette) +
  scale_x_continuous(limits = c(0, 35)) + scale_y_continuous(limits = c(-12, 170)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 24), legend.text = element_text(size = 24), legend.title = element_blank(),
        legend.position = c(0.15, 0.8)) +
  ylab("Interval production [mg AFDM m-2 d-1]") + xlab("Temperature (C)")
dark_prod
png( file = "dark_prod.png", res = 300, height = 10, width = 10, units = "in")
dark_prod
dev.off()

light_prod = ggplot(prod.int.light, aes(x = Temperature, y = int_prod, group = Stream, colour = Stream)) + geom_point(shape = 21, fill = "yellow", stroke = 4, size = 6) +
  stat_smooth(method = "lm", se = F, size = 3) +
  scale_fill_manual(values = cbbPalette) + scale_color_manual(values = cbbPalette) +
  scale_x_continuous(limits = c(0,35)) + scale_y_continuous(limits = c(-12,170)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 24), legend.text = element_text(size = 24), legend.title = element_blank(),
        legend.position = c(0.15, 0.8)) +
  ylab("Interval production [mg AFDM m-2 d-1]") + xlab("Temperature (C)")
light_prod

png(file = "light_prod.png", res = 300, height = 10, width = 10, units = "in")
light_prod
dev.off()

drklt_prod = ggplot(prod.int.light, aes(x = Temperature, y = int_prod, group = Stream, colour = Stream)) + geom_point(data = prod.int.dark, aes( x = Temperature, y = int_prod, colour = Stream), shape = 21,  fill = "black", stroke = 3,  size = 3, alpha = 0.5) +
  geom_point(shape = 21, fill = "yellow", stroke = 4, size = 6) +
  stat_smooth(method = "lm", se = F, size = 3) +
  scale_fill_manual(values = cbbPalette) + scale_color_manual(values = cbbPalette) +
  scale_x_continuous(limits = c(0,35)) + scale_y_continuous(limits = c(-12,170)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 24), legend.text = element_text(size = 24), legend.title = element_blank(),
        legend.position = c(0.15, 0.8)) +
  ylab("Interval production [mg AFDM m-2 d-1]") + xlab("Temperature (C)")
drklt_prod

png(file = "drklt_prod.png", res = 300, height = 10, width = 10, units = "in")
drklt_prod
dev.off()

light.lin = lm(int_prod~Temperature * Stream , data = prod.int.light); summary(light.lin)
dark.lin = lm(int_prod~Temperature + Stream, data = prod.int.dark); summary(dark.lin)
##### Annual production of each stream for mean and sd ####

sd(hver.ann.prod)/mean(hver.ann.prod) # 0.18
sd(st14.ann.prod)/mean(st14.ann.prod) #1.14
sd(st6.ann.prod)/mean(st6.ann.prod)  #0.120
sd(st7.ann.prod)/mean(st7.ann.prod) #0.099
sd(st9.ann.prod)/mean(st9.ann.prod)  #0.128

###### Building dataframes to look at patterns of individual species
#######

colnames(hver.out$Pintboots.cob) = get.dates(hver_bugs, site = "Hver", habitat= "COBBLE")$DATE
colnames(st14.out$Pintboots.cob) = get.dates(st14_bugs, site = "ST14", habitat = "COBBLE")$DATE
colnames(st9.out$Pintboots.cob) = get.dates(st9_bugs, site = "ST9", habitat = "COBBLE")$DATE
colnames(st6.out$Pintboots.cob) = get.dates(st6_bugs, site = "ST6", habitat = "COBBLE")$DATE
colnames(st7.out$Pintboots.cob) = get.dates(st7_bugs, site = "ST7", habitat = "COBBLE")$DATE
colnames(oh2.out$Pintboots.cob) = get.dates(oh2_bugs, site = "OH2", habitat = "COBBLE")$DATE

colnames(hver.out$Bintboots.cob) = get.dates(hver_bugs, site = "Hver", habitat = "COBBLE")$DATE
Species = rownames(hver.out$Pintboots.cob)
hver.spp.boots = hver.out$Pintboots
hver.spp.boots = cbind(hver.spp.boots, Species)

Species = rownames(st14.out$Pintboots.cob)
st14.spp.boots = st14.out$Pintboots
st14.spp.boots = cbind(st14.spp.boots, Species)

Species = rownames(st9.out$Pintboots.cob)
st9.spp.boots = st9.out$Pintboots
st9.spp.boots = cbind(st9.spp.boots, Species)

Species = rownames(st6.out$Pintboots.cob)
st6.spp.boots = st6.out$Pintboots
st6.spp.boots = cbind(st6.spp.boots, Species)

Species = rownames(st7.out$Pintboots.cob)
st7.spp.boots = st7.out$Pintboots
st7.spp.boots = cbind(st7.spp.boots, Species)

Species = rownames(oh2.out$Pintboots.cob)
oh2.spp.boots = oh2.out$Pintboots
oh2.spp.boots = cbind(oh2.spp.boots, Species)

##### lengthen out the Pintboots.cob  ######

hv_prod_long = melt(hver.spp.boots, id.var = "Species")
st14_prod_long = melt(st14.spp.boots, id.var = "Species")
st9_prod_long = melt(st9.spp.boots, id.var = "Species")
st6_prod_long = melt(st6.spp.boots, id.var = "Species")
st7_prod_long = melt(st7.spp.boots, id.var = "Species")
oh2_prod_long = melt(oh2.spp.boots, id.var = "Species")

#######

colnames(hv_prod_long) = c("Species", "DATE", "Production")
colnames(st14_prod_long) = c("Species", "DATE", "Production")
colnames(st9_prod_long) = c("Species", "DATE", "Production")
colnames(st6_prod_long) = c("Species", "DATE", "Production")
colnames(st7_prod_long) = c("Species", "DATE", "Production")
colnames(oh2_prod_long) = c("Species", "DATE", "Production")

######

hv_prod_long = hv_prod_long %>% group_by(DATE) %>% mutate(spp.rank = dense_rank(-Production))
hv_prod_long = hv_prod_long[ which(hv_prod_long$Production > 0),]

st14_prod_long = st14_prod_long %>% group_by(DATE) %>% mutate(spp.rank = dense_rank(-Production))
st14_prod_long = st14_prod_long[which(st14_prod_long$Production >0),]

st9_prod_long = st9_prod_long %>% group_by(DATE) %>% mutate(spp.rank = dense_rank(-Production))
st9_prod_long = st9_prod_long[which(st9_prod_long$Production >0),]

st6_prod_long = st6_prod_long %>% group_by(DATE) %>% mutate(spp.rank = dense_rank(-Production))
st6_prod_long = st6_prod_long[which(st6_prod_long$Production >0),]

st7_prod_long = st7_prod_long %>% group_by(DATE) %>% mutate(spp.rank = dense_rank(-Production))
st7_prod_long = st7_prod_long[which(st7_prod_long$Production >0),]

oh2_prod_long = oh2_prod_long %>% group_by(DATE) %>% mutate(spp.rank = dense_rank(-Production))
oh2_prod_long = oh2_prod_long[which(oh2_prod_long$Production >0),]
### merge with mean monthly temperature and separate out by species
##### Temp file is temp_merge #####
Stream = c(rep("Hver", nrow(hv_prod_long)), rep("ST14", nrow(st14_prod_long)), rep("ST9", nrow(st9_prod_long)),
           rep("ST6", nrow(st6_prod_long)), rep("ST7", nrow(st7_prod_long)), rep("OH2", nrow(oh2_prod_long)))

full_prod_long = rbind(hv_prod_long, st9_prod_long, st6_prod_long, st7_prod_long, oh2_prod_long, st14_prod_long)

full_prod_long$Stream = Stream

month = as.numeric(months(chron(dates = as.character(full_prod_long$DATE))))
year = as.numeric(years(chron(dates = as.character(full_prod_long$DATE))))
full_prod_long$my = paste(month,"-",year)

temp_mod = temp_merge
colnames(temp_mod) = c("month", "Hver", "ST9", "ST6", "ST14", "Intensity", "light", "ST7", "OH2", "m")

temp_mod_long = melt(temp_mod[,c(1:5,8:9)], id.var = c("month"))
temp_mod_long = temp_mod_long[! is.na(temp_mod_long$value),]
colnames(temp_mod_long) = c("month", "Stream", "Temperature")
month = as.numeric(months(chron(dates = as.character(temp_mod_long$month))))
year = as.numeric(as.character(years(chron(dates = as.character(temp_mod_long$month)))))

temp_mod_long$my = paste(month,"-",year)

full_prod_long = merge(full_prod_long, temp_mod_long, by = c("my"), all = T)

#year = as.numeric(as.character(years(chron(dates = as.character(full_prod_radix$DATE))))) 
#month = as.numeric(months(chron(dates = as.character(full_prod_radix$DATE))))
#full_prod_radix$my = as.character(paste(month,"-",year))
##### need to deal with this identify these patterns #####


full_prod_radix = full_prod_long[which(full_prod_long$Species == "Radix balthica"),]

##### Isolate the dominant species in Hver #####

hv_prod_long$m = as.numeric(months(chron(dates = as.character(hv_prod_long$DATE))))
hv_spp_int = merge(hv_prod_long, temp_merge[3:14,c(2,10)], by = c("m"), all = T)
hv_spp_int = hv_spp_int[! is.na(hv_spp_int$Species) & ! is.na(hv_spp_int$Hver_tempC),]

hv_spp_dom = hv_spp_int[which(hv_spp_int$Species == "Radix balthica" | hv_spp_int$Species == "Midge 1" | hv_spp_int$Species == "Nais spp."),]
hv_spp_plot= ggplot(hv_spp_dom, aes(x = m, y = Production, group = Species, colour = Species)) + geom_point(size = 6) +
 geom_path(size = 3) + scale_x_continuous(limits = c(1,12), breaks = rep(1:12))  + 
  theme(legend.position = "none", axis.text = element_text(size = 20), axis.title = element_text(size = 24)) +
 scale_colour_manual(values = cbbPalette) + xlab("Date") + ylab("Interval production [mg AFDM m-2]")
 hv_spp_plot 
 
 png(file = "hv_spp_int.png", res = 300, height = 10, width = 10, units = "in")
 hv_spp_plot
 dev.off()
#####
cbbPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(hv_prod_long %>% arrange(spp.rank), aes(x = spp.rank, y = log(Production))) +
  geom_point(aes(group = DATE), size = 10) + geom_path(aes(group = DATE), size = 4) +
  ylab("Log(Production [mg m-2])") + xlab("Rank") + ggtitle("Hver") +
  scale_color_manual(values = "#000000") + 
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 24), plot.title=element_text(hjust = 0.5))

ggplot(st14_prod_long %>% arrange(spp.rank), aes(x = spp.rank, y = log(Production))) +
  geom_point(aes(group = DATE), colour = "#0072B2",size = 10) + geom_path(aes(group = DATE), size = 4, colour = "#0072B2") +
  ylab("Log(Production [mg m-2])") + xlab("Rank") + ggtitle("ST14") +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 24), plot.title=element_text(hjust = 0.5))

ggplot(st9_prod_long %>% arrange(spp.rank), aes(x = spp.rank, y = log(Production))) +
  geom_point(aes(group = DATE), size = 10, colour = "#56B4E9") + geom_path(aes(group = DATE), size = 4, colour = "#56B4E9") +
  ylab("Log(Production [mg m-2])") + xlab("Rank") + ggtitle("ST9") +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 24), plot.title=element_text(hjust = 0.5))

ggplot(st6_prod_long %>% arrange(spp.rank), aes(x = spp.rank, y = log(Production))) +
  geom_point(aes(group = DATE), size = 10, colour = "#E69F00") + geom_path(aes(group = DATE), colour = "#E69F00", size = 4) +
  ylab("Log(Production [mg m-2])") + xlab("Rank") + ggtitle("ST6") +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 24), plot.title=element_text(hjust = 0.5))

ggplot(st7_prod_long %>% arrange(spp.rank), aes(x = spp.rank, y = log(Production))) +
  geom_point(aes(group = DATE), size = 10, colour = "#009E73") + geom_path(aes(group = DATE), size = 4, colour = "#009E73") +
  ylab("Log(Production [mg m-2])") + xlab("Rank") + ggtitle("ST7") +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 24), plot.title=element_text(hjust = 0.5))

ggplot(oh2_prod_long %>% arrange(spp.rank), aes(x = spp.rank, y = log(Production))) +
  geom_point(aes(group = DATE), size = 10, colour = "#F0E442") + geom_path(aes(group = DATE), size = 4, colour = "#F0E442") +
  ylab("Log(Production [mg m-2])") + xlab("Rank") + ggtitle("OH2") +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 24), plot.title=element_text(hjust = 0.5))

################################################################################
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
