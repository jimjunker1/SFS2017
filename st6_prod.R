##  r script for running Hver production ##

#load in 
library(chron)
options(stringsAsFactors = F)
library(ggplot2)
theme_set(theme_classic())


#import raw data

hengill_full <- read.table(file = './hengill_full.txt', header= T, sep = "\t", quote = "", strip.white = T, check.names = F, row.names = 1)

#subset to just one site
st6_bugs = hengill_full[which(hengill_full$SITE == "ST6"),]

taxon = as.factor(st6_bugs$TAXON)
unique(levels(taxon))


#rename some of the species
st6.fix = which(st6_bugs$TAXON == "Snail")
st6_bugs[st6.fix, "TAXON"] = "Radix balthica" 

st6.fix = which(st6_bugs$TAXON == "Radix peregra")
st6_bugs[st6.fix, "TAXON"] = "Radix balthica" 

st6.fix = which(st6_bugs$TAXON == "Snail 2")
st6_bugs[st6.fix, "TAXON"] = "Galba trunculata" 

st6.fix = which(st6_bugs$TAXON == "Brn Mite")
st6_bugs[st6.fix, "TAXON"] = "Oribatid 1"

st6.fix = which(st6_bugs$TAXON == "Brn Mite 2")
st6_bugs[st6.fix, "TAXON"] = "Oribatid 2"

st6.fix = which(st6_bugs$TAXON == "Limnophora")
st6_bugs[st6.fix, "TAXON"] = "Limnophora riparia"

st6.fix = which(st6_bugs$TAXON == "Naididae")
st6_bugs[st6.fix, "TAXON"] = "Nais spp."

st6.fix = which(st6_bugs$TAXON == "Simulium spp.")
st6_bugs[st6.fix, "TAXON"] = "S. vittatum"

st6.fix = which(st6_bugs$TAXON == "S. vitattum")
st6_bugs[st6.fix, "TAXON"] = "S. vittatum"

st6.fix = which(st6_bugs$TAXON == "S. vitttatum")
st6_bugs[st6.fix, "TAXON"] = "S. vittatum"

st6.fix = which(st6_bugs$TAXON == "Tubificid")
st6_bugs[st6.fix, "TAXON"] = "Tub. 1"

st6.fix = which(st6_bugs$TAXON == "Lumbricidae")
st6_bugs[st6.fix, "TAXON"] = "Oligochaeta indet."

st6.fix = which(st6_bugs$TAXON == "Midge 3/2")
st6_bugs[st6.fix, "TAXON"] = "Midge 3-2"
rm(st6.fix)
###
#Extract years, months, and days, and re-code them as numeric variables:
year <- as.numeric(as.character(years(chron(dates = as.character(st6_bugs$DATE)))))
month <- as.numeric(months(chron(dates = as.character(st6_bugs$DATE))))
day <- as.numeric(days(chron(dates = as.character(st6_bugs$DATE))))

#Combine year, month, and day into a single julian date variable starting with January 1, 2006 (i.e.- Jan 1, 2006 = day1):
JULIAN <- julian(month, day, year, origin=c(month = 12, day = 31, year = 2005))

#Insert the julian date variable into the dataframe:

st6_bugs <- data.frame(st6_bugs[,1:2], JULIAN, st6_bugs[,3:(dim(st6_bugs)[2])], check.names = F)
st6_bugs = as.data.frame(unclass(st6_bugs))


## Define fuctions stored in separate scripts

source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/get.dates_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/st6igr.prod_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/sf.prod_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/pb.prod_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/wrapper.site.yr_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/hab.weight_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/sampinfo.site.yr_function.txt")

#Import temperature data:

st6_temp <- temp_light.m[,c(1,4)]
#find the dates
get.dates(st6_bugs, site = "ST6", habitat = "COBBLE")

#set the date file
st6_temp1 = round(st6_temp[c(4:6,8:11,13:14),2],2)

##Import taxa info:
 #L-M parameter, method of production, growth parameters, cpi's, p/b's, etc. for each taxon:
#tax = read.table(file = "./SFS code/st6_taxa_info_LISA.txt", header = T, sep = "\t", quote = "", strip.white = T)
tax = read.table(file = "./SFS code/st6_taxa_info_LISA_mod.txt", header = T, sep = "\t", quote = "", strip.white = T) #this is an updated file for l. riparia growth rates. 

colnames(tax) = c("TAXON", 	"METHOD", 	"LM.a", "LM.b",	"LM.p.ash",	"g.a",	"g.b",	"g.c",	"g.d",	"min.cpi",	"max.cpi",	"num.size.classes",	"p.b",	"Growth.equation", 	"min.growth",	"notes")
#source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/wrapper.site.yr_function.txt")

st6.out = wrapper.site.yr(DATA = st6_bugs, site = "ST6", habitat = "COBBLE", TEMP.COB = st6_temp1, TEMP.DEP = st6_temp1, TEMP.TAL = st6_temp1, first.date = "08/02/11", last.date = "06/19/12",
                           TAXA = tax, temp.corr.igr.cob = c(1,1,1,1,1,1,1,1,1), temp.corr.igr.dep = c(1,1,1,1,1,1,1,1,1), temp.corr.igr.tal = c(1,1,1,1,1,1,1,1,1), wrap = T, boot.num = 50)

names(st6.out)
st6.out$Pintboots.cob
colSums(st6.out$Pintboots.cob, na.rm = T)

st6.int = data.frame(st6_temp1, colSums(st6.out$Pintboots.cob, na.rm = T))
colnames(st6.int) = c("Temperature", "Production")
sum(colSums(st6.out$Pintboots.cob, na.rm = T))
## This finally fucking worked!! woohoo. 
# Now work with hver.out$Pboots.cob to get the mean summed community production annually
# then compare with annual mean temperature 

st6.prod = st6.out$Pboots.cob
st6.bio = st6.out$Bboots.cob

st6.ann.prod = apply(st6.prod, 1, sum, na.rm = T)
st6.ann.bio = apply(st6.bio, 1, sum, na.rm = T)

st6.spp.ann.prod2 = data.frame(apply(st6.out$Pintboots.cob, 1, sum, na.rm = T))
st6.spp.ann.prod = data.frame(apply(st6.prod, 2, mean, na.rm = T))

df = data.frame(st6.spp.ann.prod[,1],st6.spp.ann.prod2[,1]) 
df[is.na(df)]<- 0

plot(log(df[,1]), log(df[,2]))
abline(a = 0, b = 1)

st6.spp.ann.prod = data.frame(apply(st6.prod, 2, mean, na.rm = T))
colnames(st6.spp.ann.prod) = "Mean.Annual.Prod"
st6.spp.ann.prod$Species = rownames(st6.spp.ann.prod)
st6.spp.ann.prod$Species.rank = reorder(st6.spp.ann.prod$Species, -st6.spp.ann.prod$Mean.Annual.Prod)

ggplot(st6.spp.ann.prod, aes(x = Species.rank, y = log(Mean.Annual.Prod))) + geom_point(size = 2) +
  ylab("ln(Annual production [mg m-2 yr-2])") +  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.title.x = element_blank())

mean(st6.ann.prod)  #4886  #pulls the mean community production estimate
mean(st6.ann.bio)   # 446  pulls mean community biomass estimate
mean(st6.ann.prod)/mean(st6.ann.bio)  #10.95  #annual p/b
##This pulls out biomass and abundance data for each taxa at each interval
sampinfo.site.yr(DATA = hver_bugs, site = "Hver", first.date = "08/02/11", last.date = "07/26/12", habitat = "COBBLE", TAXA = tax)

