##  r script for running Hver production ##

#load in 
library(chron)
options(stringsAsFactors = F)
library(ggplot2)
theme_set(theme_classic())

#import raw data

hengill_full <- read.table(file = './hengill_full.txt', header= T, sep = "\t", quote = "", strip.white = T, check.names = F, row.names = 1)

#subset to just one site
hver_bugs = hengill_full[which(hengill_full$SITE == "Hver"),]

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
rm(hver.fix)

taxon = as.factor(hver_bugs$TAXON)
unique(levels(taxon))

###
#Extract years, months, and days, and re-code them as numeric variables:
year <- as.numeric(as.character(years(chron(dates = as.character(hver_bugs$DATE)))))
month <- as.numeric(months(chron(dates = as.character(hver_bugs$DATE))))
day <- as.numeric(days(chron(dates = as.character(hver_bugs$DATE))))

#Combine year, month, and day into a single julian date variable starting with January 1, 2006 (i.e.- Jan 1, 2006 = day1):
JULIAN <- julian(month, day, year, origin=c(month = 12, day = 31, year = 2005))

#Insert the julian date variable into the dataframe:

hver_bugs <- data.frame(hver_bugs[,1:2], JULIAN, hver_bugs[,3:(dim(hver_bugs)[2])], check.names = F)
hver_bugs = as.data.frame(unclass(hver_bugs))

#Import temperature data:

hv_temp <- temp_light.m[,1:2]
#find number of dates
get.dates(hver_bugs, site = "Hver", habitat = "COBBLE")


hv_temp1 = round(hv_temp[c(3:7,9:10,12:14),2],2)

## Define fuctions stored in separate scripts

source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/get.dates_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/hvigr.prod_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/sf.prod_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/pb.prod_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/wrapper.site.yr_function.R")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/hab.weight_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/sampinfo.site.yr_function.txt")


##Import taxa info:
 #L-M parameter, method of production, growth parameters, cpi's, p/b's, etc. for each taxon:
  #tax = read.table(file = "./SFS code/hver_taxa_info_LISA.txt", header = T, sep = "\t", quote = "", strip.white = T)
  tax = read.table(file = "./SFS code/hver_taxa_info_LISA_mod.txt", header = T, sep = "\t", quote = "", strip.white = T)
colnames(tax) = c("TAXON", 	"METHOD", 	"LM.a", "LM.b",	"LM.p.ash",	"g.a",	"g.b",	"g.c",	"g.d",	"min.cpi",	"max.cpi",	"num.size.classes",	"p.b",	"Growth.equation", 	"min.growth",	"notes")
#source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/wrapper.site.yr_function.txt")
set.seed(123)
hver.out = wrapper.site.yr(DATA = hver_bugs, site = "Hver", habitat = "COBBLE", TEMP.COB = hv_temp1, TEMP.DEP = hv_temp1, TEMP.TAL = hv_temp1, first.date = "08/02/11", last.date = "07/26/12",
                           TAXA = tax, temp.corr.igr.cob = c(1,1,1,1,1,1,1,1,1,1), temp.corr.igr.dep = c(1,1,1,1,1,1,1,1,1,1), temp.corr.igr.tal = c(1,1,1,1,1,1,1,1,1,1), wrap = T, boot.num = 50)
names(hver.out)
hver.out$Pintboots.cob
colSums(hver.out$Pintboots.cob, na.rm = T)

hv_temp1 = round(hv_temp[c(3:7,9:10,12:14),2],2)
hv_tempint = hv_temp[c(3:7,9:10,12:14),]

Stream = rep("Hver", length(hv_temp1))
hver.int = data.frame(Stream, hv_tempint, colSums(hver.out$Pintboots.cob, na.rm = T))
colnames(hver.int) = c("Stream", "Month", "Temperature", "Production")
sum(colSums(hver.out$Pintboots.cob, na.rm = T))
## This finally fucking worked!! woohoo. 
# Now work with hver.out$Pboots.cob to get the mean summed community production annually
# then compare with annual mean temperature 

hver.prod = hver.out$Pboots.cob
hver.bio = hver.out$Bboots.cob

hver.ann.prod = apply(hver.prod, 1, sum, na.rm = T)
hver.ann.bio = apply(hver.bio, 1, sum, na.rm = T)

hver.spp.ann.prod2 = data.frame(apply(hver.out$Pintboots.cob, 1, sum, na.rm = T))
hver.spp.ann.prod = data.frame(apply(hver.prod, 2, mean, na.rm = T))

df = data.frame(hver.spp.ann.prod[,1],hver.spp.ann.prod2[,1]) 
df[is.na(df)]<- 0

plot(log(df[,1]), log(df[,2]))
abline(a = 0, b = 1)

colnames(hver.spp.ann.prod) = "Mean.Annual.Prod"
hver.spp.ann.prod$Species = rownames(hver.spp.ann.prod)
hver.spp.ann.prod$Species.rank = reorder(hver.spp.ann.prod$Species, -hver.spp.ann.prod$Mean.Annual.Prod)

ggplot(hver.spp.ann.prod, aes(x = Species.rank, y = log(Mean.Annual.Prod))) + geom_point(size = 2) +
  ylab("ln(Annual production [mg m-2 yr-2])") +  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.title.x = element_blank())

mean(hver.ann.prod)  #21627
mean(hver.ann.bio)   #270.7499
mean(hver.ann.prod)/mean(hver.ann.bio)   #80 need to fix this. way too high

##This pulls out biomass and abundance data for each taxa at each interval
sampinfo.site.yr(DATA = hver_bugs, site = "Hver", first.date = "08/02/11", last.date = "07/26/12", habitat = "COBBLE", TAXA = tax)

##run on an individual taxon to see how it works

hver.radix = igr.prod(DATA = hver_bugs, site = "Hver", habitat = "COBBLE", TEMP = hv_temp1, first.date = "08/02/11", last.date = "07/26/12",
                           taxon = "Radix balthica",LM.a = 0.0284, LM.b = 2.613, LM.p.ash = 18.85, g.a = -6.27541, g.b = 0.1421, g.c = -0.77972, g.d = NA, min.growth = 0.01, 
                      wrap = T, temp.corr = c(1,1,1,1,1,1,1,1,1,1), boot.num = 50)

names(hver.radix)
str(hver.radix$Pintboots)
hver.radix.mean = colMeans(data.frame(hver.radix$Pintboots))
hver.summ = apply(hver.radix$Pintboots, 2, mean, na.rm = T)
str(hver.radix$Pintboots)


