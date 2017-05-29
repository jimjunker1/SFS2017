##  r script for running Hver production ##

#load in 
library(chron)
options(stringsAsFactors = F)
library(ggplot2)
theme_set(theme_classic())

#import raw data

hengill_full <- read.table(file = './hengill_full.txt', header= T, sep = "\t", quote = "", strip.white = T, check.names = F, row.names = 1)

#subset to just one site
st7_bugs = hengill_full[which(hengill_full$SITE == "ST7"),]


#rename some of the species 
st7.fix = which(st7_bugs$TAXON == "Radix peregra")
st7_bugs[st7.fix, "TAXON"] = "Radix balthica"

st7.fix = which(st7_bugs$TAXON == "Simulium vittatum")
st7_bugs[st7.fix, "TAXON"] = "S. vittatum"

st7.fix = which(st7_bugs$TAXON == "Simulium vernum")
st7_bugs[st7.fix, "TAXON"] = "S. vernum"

st7.fix = which(st7_bugs$TAXON == "Simulium aureum")
st7_bugs[st7.fix, "TAXON"] = "S. aureum"

st7.fix = which(st7_bugs$TAXON == "Prosimulium ursinum")
st7_bugs[st7.fix, "TAXON"] = "Prosimulium"

st7.fix = which(st7_bugs$TAXON == "Clinocera stagnalis")
st7_bugs[st7.fix, "TAXON"] = "Clinocera"

st7.fix = which(st7_bugs$TAXON == "Sperchon glandulosus")
st7_bugs[st7.fix, "TAXON"] = "Sperchon sp."

st7.fix = which(st7_bugs$TAXON == "Nematoda")
st7_bugs[st7.fix, "TAXON"] = "Nematode"

st7.fix = which(st7_bugs$TAXON == "Potamophylax cingulatus")
st7_bugs[st7.fix, "TAXON"] = "Potamophylax"

st7.fix = which(st7_bugs$TAXON == "Diamesa bohemani/zernyi")
st7_bugs[st7.fix, "TAXON"] = "Diamesa bohemani-zernyi"

st7.fix = which(st7_bugs$TAXON == "Copepoda")
st7_bugs[st7.fix, "TAXON"] = "Copepod"

rm(st7.fix)
taxon = as.factor(st7_bugs$TAXON)
unique(levels(taxon))


###
#Extract years, months, and days, and re-code them as numeric variables:
year <- as.numeric(as.character(years(chron(dates = as.character(st7_bugs$DATE)))))
month <- as.numeric(months(chron(dates = as.character(st7_bugs$DATE))))
day <- as.numeric(days(chron(dates = as.character(st7_bugs$DATE))))

#Combine year, month, and day into a single julian date variable starting with January 1, 2006 (i.e.- Jan 1, 2006 = day1):
JULIAN <- julian(month, day, year, origin=c(month = 12, day = 31, year = 2005))

#Insert the julian date variable into the dataframe:

st7_bugs <- data.frame(st7_bugs[,1:2], JULIAN, st7_bugs[,3:(dim(st7_bugs)[2])], check.names = F)
st7_bugs = as.data.frame(unclass(st7_bugs))

saveRDS(st7_bugs, file = "st7_bugs.rda")

## Define fuctions stored in separate scripts

source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/get.dates_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/st7igr.prod_function.R")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/sf.prod_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/pb.prod_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/wrapper.site.yr_function.R")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/hab.weight_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/sampinfo.site.yr_function.txt")

#Import temperature data:

st7_temp <- temp_light10.11[,c(1,2)]
#find the dates
get.dates(st7_bugs, site = "ST7", habitat = "COBBLE")

#set the date file
st7_temp1 = round(st7_temp[c(1,3:13),2],2)
st7_tempint = st7_temp[c(1,3:13),]

##Import taxa info:
 #L-M parameter, method of production, growth parameters, cpi's, p/b's, etc. for each taxon:
tax = read.table(file = "./SFS code/st7_taxa_info_LISA_mod.txt", header = T, sep = "\t", quote = "", strip.white = T)
#tax = read.table(file = "./SFS code/st7_taxa_info_LISA.txt", header = T, sep = "\t", quote = "", strip.white = T) #this is an updated file for l. riparia growth rates. 
colnames(tax) = c("TAXON", 	"METHOD", 	"LM.a", "LM.b",	"LM.p.ash",	"g.a",	"g.b",	"g.c",	"g.d",	"min.cpi",	"max.cpi",	"num.size.classes",	"p.b",	"Growth.equation", 	"min.growth",	"notes")
#source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/wrapper.site.yr_function.txt")
set.seed(123)
st7.out = wrapper.site.yr(DATA = st7_bugs, site = "ST7", habitat = "COBBLE", TEMP.COB = st7_temp1, TEMP.DEP = st7_temp1, TEMP.TAL = st7_temp1, first.date = "10/26/10", last.date = "10/17/11",
                           TAXA = tax, temp.corr.igr.cob = c(1,1,1,1,1,1,1,1,1,1,1,1), temp.corr.igr.dep = c(1,1,1,1,1,1,1,1,1,1,1,1), temp.corr.igr.tal = c(1,1,1,1,1,1,1,1,1,1,1,1), wrap = T, boot.num = 50)

saveRDS(st7.out, file = "st7_out.rda")
names(st7.out)
st7.out$Pintboots.cob
colSums(st7.out$Pintboots.cob, na.rm = T)

Stream = rep("ST7", length(st7_temp1))
st7.int = data.frame(Stream, st7_tempint, colSums(st7.out$Pintboots.cob, na.rm = T))
colnames(st7.int) = c("Stream", "month", "Temperature", "Production")
saveRDS(st7.int, file = "st7.int.rda")

sum(colSums(st7.out$Pintboots.cob, na.rm = T))
## This finally fucking worked!! woohoo. 
# Now work with hver.out$Pboots.cob to get the mean summed community production annually
# then compare with annual mean temperature 

st7.prod = st7.out$Pboots.cob
st7.bio = st7.out$Bboots.cob

st7.ann.prod = apply(st7.prod, 1, sum, na.rm = T)
st7.ann.bio = apply(st7.bio, 1, sum, na.rm = T)

st7.spp.ann.prod2 = data.frame(apply(st7.out$Pintboots.cob, 1, sum, na.rm = T))
st7.spp.ann.prod = data.frame(apply(st7.prod, 2, mean, na.rm = T))

df = data.frame(st7.spp.ann.prod[,1],st7.spp.ann.prod2[,1]) 
df[is.na(df)]<- 0

plot(log(df[,1]), log(df[,2]))
abline(a = 0, b = 1)

#st7.spp.ann.prod = data.frame(apply(st7.prod, 2, mean, na.rm = T))
colnames(st7.spp.ann.prod) = "Mean.Annual.Prod"
st7.spp.ann.prod$Species = rownames(st7.spp.ann.prod)
st7.spp.ann.prod$Species.rank = reorder(st7.spp.ann.prod$Species, -st7.spp.ann.prod$Mean.Annual.Prod)
st7.spp.ann.prod$spp.rank = seq(1, length(st7.spp.ann.prod$Species.rank), length = length(st7.spp.ann.prod$Species.rank))
st7.spp.ann.prod$spp.rank = reorder(st7.spp.ann.prod$spp.rank, - st7.spp.ann.prod$Mean.Annual.Prod)
st7.spp.ann.prod[which(st7.spp.ann.prod$Mean.Annual.Prod == 0),] <- NA
st7.spp.ann.prod = st7.spp.ann.prod[! is.na(st7.spp.ann.prod$spp.rank),]
st7.spp.ann.prod = st7.spp.ann.prod %>% mutate(spp.rank = dense_rank(-Mean.Annual.Prod))
  
saveRDS(st7.spp.ann.prod, file = "st7.spp.ann.prod.rda")

#no species to remove
####
ggplot(st7.spp.ann.prod %>% arrange(spp.rank), aes(x = spp.rank, y = log(Mean.Annual.Prod))) + geom_point(size = 2) + geom_path(aes(group = 1), size = 1.5) +
  ylab("ln(Annual production [mg m-2 yr-1])") +  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.title.x = element_blank())

ggplot(st7.spp.ann.prod, aes(x = Species.rank, y = log(Mean.Annual.Prod))) + geom_point(size = 2) +
  ylab("ln(Annual production [mg m-2 yr-1])") +  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.title.x = element_blank())
###

st7.spp.ann.bio = data.frame(apply(st7.bio, 2, mean, na.rm = T))
colnames(st7.spp.ann.bio) = "Mean.Annual.Bio"
st7.spp.ann.bio$Species = rownames(st7.spp.ann.bio)
st7.spp.ann.bio$Species.rank = reorder(st7.spp.ann.bio$Species, -st7.spp.ann.bio$Mean.Annual.Bio)

ggplot(st7.spp.ann.bio, aes(x = Species.rank, y = Mean.Annual.Bio)) + geom_point(size = 2) +
  ylab("ln(Annual Biomass[mg m-2])") +  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.title.x = element_blank())


mean(st7.ann.prod)  #92148  #pulls the mean community production estimate
mean(st7.ann.bio)  #25226    #pulls mean community biomass estimate
mean(st7.ann.prod)/mean(st7.ann.bio) # 3.65278  #annual p/b
##This pulls out biomass and abundance data for each taxa at each interval
sampinfo.site.yr(DATA = hver_bugs, site = "Hver", first.date = "08/02/11", last.date = "07/26/12", habitat = "COBBLE", TAXA = tax)

#pull out the species 

#prosimulium = st7_bugs[which(st7_bugs$TAXON == "Prosimulium"),]

#for (i in c(2,3,7:dim(prosimulium)[2])){
#  prosimulium[,i] <- as.numeric(as.character(prosimulium[,i]))	#Must force the appropriate columns to be numeric
#}

#Create a dataframe of sizes:
#mm <- as.numeric(substr(names(prosimulium)[7:(dim(prosimulium)[2])], 2, 6))
#AFDM <- (1-(7.9/100))*LM.a*(mm^LM.b)
#sizes <- data.frame(mm, AFDM)