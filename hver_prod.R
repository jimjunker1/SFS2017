##  r script for running Hver production ##

#load in 
library(chron)

## Define fuctions stored in separate scripts

source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/get.dates_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/hvigr.prod_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/sf.prod_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/pb.prod_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/wrapper.site.yr_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/hab.weight_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/sampinfo.site.yr_function.txt")

#import raw data

hengill_merge <- read.table(file = './hengill_merged.txt', header= T, sep = "\t", quote = "", strip.white = T, check.names = F, row.names = 1)

#str(hengill_merge)

hver_bugs = hengill_merge[which(hengill_merge$SITE == "Hver"),]

#rename some of the species
hver.fix = which(hver_bugs$TAXON == "Snail")
hver_bugs[hver.fix, "TAXON"] = "Radix balthica" 

hver.fix = which(hver_bugs$TAXON == "Brn Mite")
hver_bugs[hver.fix, "TAXON"] = "Oribatid 1"

hver.fix = which(hver_bugs$TAXON == "Limnophora")
hver_bugs[hver.fix, "TAXON"] = "Limnophora riparia"

hver.fix = which(hver_bugs$TAXON == "Lumb 1")
hver_bugs[hver.fix, "TAXON"] = "Nais spp."

hver.fix = which(hver_bugs$TAXON == "Simulium spp.")
hver_bugs[hver.fix, "TAXON"] = "S. vittatum"

hver.fix = which(hver_bugs$TAXON == "S. vitattum")
hver_bugs[hver.fix, "TAXON"] = "S. vittatum"

hver.fix = which(hver_bugs$TAXON == "Lumb 2")
hver_bugs[hver.fix, "TAXON"] = "Tub. 1"

###
#Extract years, months, and days, and re-code them as numeric variables:
year <- as.numeric(as.character(years(chron(dates = as.character(hver_bugs$DATE)))))
month <- as.numeric(months(chron(dates = as.character(hver_bugs$DATE))))
day <- as.numeric(days(chron(dates = as.character(hver_bugs$DATE))))

#Combine year, month, and day into a single julian date variable starting with January 1, 2006 (i.e.- Jan 1, 2006 = day1):
JULIAN <- julian(month, day, year, origin=c(month = 12, day = 31, year = 2005))

#Insert the julian date variable into the dataframe:
hver_bugs <- data.frame(hver_bugs[,1:2], JULIAN, hver_bugs[,3:(dim(hver_bugs)[2])])

#Import temperature data:
#Average temperatures (degrees C) for each of the 4 sampling intervals for RM30, year2.5
#DON'T HAVE REAL TEMP DATA FOR RM30, SO I MADE IT UP BY SUBSETTING RM0 DATA:
hv_temp <- temp_light.m[,1:2]

##Import taxa info:
 #L-M parameter, method of production, growth parameters, cpi's, p/b's, etc. for each taxon:
  tax = read.table(file = "./SFS code/hver_taxa_info_LISA.txt", header = T, sep = "\t", quote = "", strip.white = T)
  
  
colnames(tax) = c("TAXON", 	"METHOD", 	"LM.a", "LM.b",	"LM.p.ash",	"g.a",	"g.b",	"g.c",	"g.d",	"min.cpi",	"max.cpi",	"num.size.classes",	"p.b",	"Growth.equation", 	"min.growth",	"notes")

hver.out = wrapper.site.yr(DATA = hver_bugs, site = "Hver", habitat = "ALL", TEMP.COB = 
                           TAXA = tax, boot.num = 50)
names(hver.out)
