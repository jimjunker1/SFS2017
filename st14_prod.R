##  r script for running Hver production ##

#load in 
library(chron)
options(stringsAsFactors = F)
library(ggplot2)
theme_set(theme_classic())


#import raw data

hengill_merge <- read.table(file = './hengill_merged.txt', header= T, sep = "\t", quote = "", strip.white = T, check.names = F, row.names = 1)

#subset to just one site
st14_bugs = hengill_merge[which(hengill_merge$SITE == "ST14"),]

taxon = as.factor(st14_bugs$TAXON)
unique(levels(taxon))


#rename some of the species
st14.fix = which(st14_bugs$TAXON == "Snail")
st14_bugs[st14.fix, "TAXON"] = "Radix balthica" 

st14.fix = which(st14_bugs$TAXON == "Radix peregra")
st14_bugs[st14.fix, "TAXON"] = "Radix balthica" 

st14.fix = which(st14_bugs$TAXON == "Snail 2")
st14_bugs[st14.fix, "TAXON"] = "Galba trunculata" 

st14.fix = which(st14_bugs$TAXON == "Mite")
st14_bugs[st14.fix, "TAXON"] = "Sperchon sp."

st14.fix = which(st14_bugs$TAXON == "Brn Mite")
st14_bugs[st14.fix, "TAXON"] = "Oribatid 1"

st14.fix = which(st14_bugs$TAXON == "Wht Mite")
st14_bugs[st14.fix, "TAXON"] = "Oribatid 1"

st14.fix = which(st14_bugs$TAXON == "Brn Mite 1")
st14_bugs[st14.fix, "TAXON"] = "Oribatid 1"

st14.fix = which(st14_bugs$TAXON == "Brn Mite 2")
st14_bugs[st14.fix, "TAXON"] = "Oribatid 2"

st14.fix = which(st14_bugs$TAXON == "Brn Mite 3")
st14_bugs[st14.fix, "TAXON"] = "Oribatid 3"

st14.fix = which(st14_bugs$TAXON == "Brn mite 3")
st14_bugs[st14.fix, "TAXON"] = "Oribatid 3"

st14.fix = which(st14_bugs$TAXON == "Limnophora")
st14_bugs[st14.fix, "TAXON"] = "Limnophora riparia"

st14.fix = which(st14_bugs$TAXON == "Clinocera sp.")
st14_bugs[st14.fix, "TAXON"] = "Clinocera"

st14.fix = which(st14_bugs$TAXON == "Naididae")
st14_bugs[st14.fix, "TAXON"] = "Nais spp."

st14.fix = which(st14_bugs$TAXON == "Simulium spp.")
st14_bugs[st14.fix, "TAXON"] = "S. vittatum"

st14.fix = which(st14_bugs$TAXON == "S. vitattum")
st14_bugs[st14.fix, "TAXON"] = "S. vittatum"

st14.fix = which(st14_bugs$TAXON == "Simulium spp.")
st14_bugs[st14.fix, "TAXON"] = "S. indet"

st14.fix = which(st14_bugs$TAXON == "S. vitttatum")
st14_bugs[st14.fix, "TAXON"] = "S. vittatum"

st14.fix = which(st14_bugs$TAXON == "Tubificid")
st14_bugs[st14.fix, "TAXON"] = "Tub. 1"

st14.fix = which(st14_bugs$TAXON == "Tubificid 1")
st14_bugs[st14.fix, "TAXON"] = "Tub. 1"

st14.fix = which(st14_bugs$TAXON == "Tubificid 2")
st14_bugs[st14.fix, "TAXON"] = "Tub. 2"

st14.fix = which(st14_bugs$TAXON == "Lumb")
st14_bugs[st14.fix, "TAXON"] = "Oligochaeta indet."

st14.fix = which(st14_bugs$TAXON == "Lumb 1")
st14_bugs[st14.fix, "TAXON"] = "Oligochaeta indet."

st14.fix = which(st14_bugs$TAXON == "Lumb 2")
st14_bugs[st14.fix, "TAXON"] = "Oligochaeta indet."

st14.fix = which(st14_bugs$TAXON == "Lumbricidae")
st14_bugs[st14.fix, "TAXON"] = "Oligochaeta indet."

st14.fix = which(st14_bugs$TAXON == "Oligo")
st14_bugs[st14.fix, "TAXON"] = "Oligochaeta indet."

st14.fix = which(st14_bugs$TAXON == "oligo")
st14_bugs[st14.fix, "TAXON"] = "Oligochaeta indet."

st14.fix = which(st14_bugs$TAXON == "Oligo indet")
st14_bugs[st14.fix, "TAXON"] = "Oligochaeta indet."

st14.fix = which(st14_bugs$TAXON == "Midge 3/2")
st14_bugs[st14.fix, "TAXON"] = "Midge 3-2"

st14.fix = which(st14_bugs$TAXON == "Eukifierella sp.")
st14_bugs[st14.fix, "TAXON"] = "Midge 2"

st14.fix = which(st14_bugs$TAXON == "Midge")
st14_bugs[st14.fix, "TAXON"] = "Midge indet."

st14.fix = which(st14_bugs$TAXON == "Tanytarsus")
st14_bugs[st14.fix, "TAXON"] = "Midge 1"

st14.fix = which(st14_bugs$TAXON == "P. cingulatus")
st14_bugs[st14.fix, "TAXON"] = "Potamaphylax"

st14.fix = which(st14_bugs$TAXON == "Midge B")
st14_bugs[st14.fix, "TAXON"] = "Midge 4"

st14.fix = which(st14_bugs$TAXON == "Midge 4/7")
st14_bugs[st14.fix, "TAXON"] = "Midge 4"

st14.fix = which(st14_bugs$TAXON == "Midge C")
st14_bugs[st14.fix, "TAXON"] = "Midge 3"

st14.fix = which(st14_bugs$TAXON == "Midge D")
st14_bugs[st14.fix, "TAXON"] = "Midge 5"

st14.fix = which(st14_bugs$TAXON == "Midge E")
st14_bugs[st14.fix, "TAXON"] = "Midge 6"

rm(st14.fix)

taxon = as.factor(st14_bugs$TAXON)
unique(levels(taxon))
###
#Extract years, months, and days, and re-code them as numeric variables:
year <- as.numeric(as.character(years(chron(dates = as.character(st14_bugs$DATE)))))
month <- as.numeric(months(chron(dates = as.character(st14_bugs$DATE))))
day <- as.numeric(days(chron(dates = as.character(st14_bugs$DATE))))

#Combine year, month, and day into a single julian date variable starting with January 1, 2006 (i.e.- Jan 1, 2006 = day1):
JULIAN <- julian(month, day, year, origin=c(month = 12, day = 31, year = 2005))

#Insert the julian date variable into the dataframe:

st14_bugs <- data.frame(st14_bugs[,1:2], JULIAN, st14_bugs[,3:(dim(st14_bugs)[2])], check.names = F)
st14_bugs = as.data.frame(unclass(st14_bugs))


## Define fuctions stored in separate scripts

source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/get.dates_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/st14igr.prod_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/sf.prod_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/pb.prod_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/wrapper.site.yr_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/hab.weight_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/sampinfo.site.yr_function.txt")

#Import temperature data:

st14_temp <- temp_light.m[,c(1,5)]
#find the dates
get.dates(st14_bugs, site = "ST14", habitat = "COBBLE")

#set the date file
st14_temp1 = round(st14_temp[c(3:5,7,10:14),2],2)

##Import taxa info:
 #L-M parameter, method of production, growth parameters, cpi's, p/b's, etc. for each taxon:
#tax = read.table(file = "./SFS code/st14_taxa_info_LISA_mod.txt", header = T, sep = "\t", quote = "", strip.white = T)
tax = read.table(file = "./SFS code/st14_taxa_info_LISA.txt", header = T, sep = "\t", quote = "", strip.white = T) #this is an updated file for l. riparia growth rates. 
colnames(tax) = c("TAXON", 	"METHOD", 	"LM.a", "LM.b",	"LM.p.ash",	"g.a",	"g.b",	"g.c",	"g.d",	"min.cpi",	"max.cpi",	"num.size.classes",	"p.b",	"Growth.equation", 	"min.growth",	"notes")
#source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/wrapper.site.yr_function.txt")
set.seed(123)
st14.out = wrapper.site.yr(DATA = st14_bugs, site = "ST14", habitat = "COBBLE", TEMP.COB = st14_temp1, TEMP.DEP = st14_temp1, TEMP.TAL = st14_temp1, first.date = "07/01/11", last.date = "07/26/12",
                           TAXA = tax, temp.corr.igr.cob = c(1,1,1,1,1,1,1,1,1), temp.corr.igr.dep = c(1,1,1,1,1,1,1,1,1), temp.corr.igr.tal = c(1,1,1,1,1,1,1,1,1), wrap = F, boot.num = 50)

names(st14.out)

## This finally fucking worked!! woohoo. 
# Now work with hver.out$Pboots.cob to get the mean summed community production annually
# then compare with annual mean temperature 

st14.prod = st14.out$Pboots.cob
st14.bio = st14.out$Bboots.cob

st14.ann.prod = apply(st14.prod, 1, sum, na.rm = T)
st14.ann.bio = apply(st14.bio, 1, sum, na.rm = T)

st14.spp.ann.prod = data.frame(apply(st14.prod, 2, mean, na.rm = T))
colnames(st14.spp.ann.prod) = "Mean.Annual.Prod"
st14.spp.ann.prod$Species = rownames(st14.spp.ann.prod)
st14.spp.ann.prod$Species.rank = reorder(st14.spp.ann.prod$Species, -st14.spp.ann.prod$Mean.Annual.Prod)

ggplot(st14.spp.ann.prod, aes(x = Species.rank, y = log(Mean.Annual.Prod))) + geom_point(size = 2) +
  ylab("ln(Annual production [mg m-2 yr-2])") +  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.title.x = element_blank())

mean(st14.ann.prod)  #pulls the mean community production estimate
mean(st14.ann.bio)   #pulls mean community biomass estimate
mean(st14.ann.prod)/mean(st14.ann.bio)  #annual p/b
##This pulls out biomass and abundance data for each taxa at each interval
sampinfo.site.yr(DATA = hver_bugs, site = "Hver", first.date = "08/02/11", last.date = "07/26/12", habitat = "COBBLE", TAXA = tax)

