##  r script for running Hver production ##

#load in 
library(chron)
options(stringsAsFactors = F)
library(ggplot2)
theme_set(theme_classic())

#import raw data

hengill_full <- read.table(file = './hengill_full.txt', header= T, sep = "\t", quote = "", strip.white = T, check.names = F, row.names = 1)

#subset to just one site
oh2_bugs = hengill_full[which(hengill_full$SITE == "OH2"),]


#rename some of the species 
oh2.fix = which(oh2_bugs$TAXON == "Radix peregra")
oh2_bugs[oh2.fix, "TAXON"] = "Radix balthica"

oh2.fix = which(oh2_bugs$TAXON == "Simulium vittatum")
oh2_bugs[oh2.fix, "TAXON"] = "S. vittatum"

oh2.fix = which(oh2_bugs$TAXON == "Simulium vernum")
oh2_bugs[oh2.fix, "TAXON"] = "S. vernum"

oh2.fix = which(oh2_bugs$TAXON == "Simulium aureum")
oh2_bugs[oh2.fix, "TAXON"] = "S. aureum"

oh2.fix = which(oh2_bugs$TAXON == "Prosimulium ursinum")
oh2_bugs[oh2.fix, "TAXON"] = "Prosimulium"

oh2.fix = which(oh2_bugs$TAXON == "Clinocera stagnalis")
oh2_bugs[oh2.fix, "TAXON"] = "Clinocera"

oh2.fix = which(oh2_bugs$TAXON == "Sperchon glandulosus")
oh2_bugs[oh2.fix, "TAXON"] = "Sperchon sp."

oh2.fix = which(oh2_bugs$TAXON == "Nematoda")
oh2_bugs[oh2.fix, "TAXON"] = "Nematode"

oh2.fix = which(oh2_bugs$TAXON == "Potamophylax cingulatus")
oh2_bugs[oh2.fix, "TAXON"] = "Potamophylax"

oh2.fix = which(oh2_bugs$TAXON == "Diamesa bohemani/zernyi")
oh2_bugs[oh2.fix, "TAXON"] = "Diamesa bohemani-zernyi"

oh2.fix = which(oh2_bugs$TAXON == "Copepoda")
oh2_bugs[oh2.fix, "TAXON"] = "Copepod"

rm(oh2.fix)
taxon = as.factor(oh2_bugs$TAXON)
unique(levels(taxon))


###
#Extract years, months, and days, and re-code them as numeric variables:
year <- as.numeric(as.character(years(chron(dates = as.character(oh2_bugs$DATE)))))
month <- as.numeric(months(chron(dates = as.character(oh2_bugs$DATE))))
day <- as.numeric(days(chron(dates = as.character(oh2_bugs$DATE))))

#Combine year, month, and day into a single julian date variable starting with January 1, 2006 (i.e.- Jan 1, 2006 = day1):
JULIAN <- julian(month, day, year, origin=c(month = 12, day = 31, year = 2005))

#Insert the julian date variable into the dataframe:

oh2_bugs <- data.frame(oh2_bugs[,1:2], JULIAN, oh2_bugs[,3:(dim(oh2_bugs)[2])], check.names = F)
oh2_bugs = as.data.frame(unclass(oh2_bugs))

saveRDS(oh2_bugs, file = "oh2_bugs.rda")

## Define fuctions stored in separate scripts

source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/get.dates_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/st7igr.prod_function.R")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/sf.prod_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/pb.prod_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/wrapper.site.yr_function.R")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/hab.weight_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/sampinfo.site.yr_function.txt")

#Import temperature data:

oh2_temp <- temp_light10.11[,c(1,3)]
#find the dates
get.dates(oh2_bugs, site = "OH2", habitat = "COBBLE")

#set the date file
oh2_temp1 = round(oh2_temp[c(1,3:13),2],2)
oh2_tempint = oh2_temp[c(1,3:13),]

##Import taxa info:
 #L-M parameter, method of production, growth parameters, cpi's, p/b's, etc. for each taxon:
tax = read.table(file = "./SFS code/oh2_taxa_info_LISA_mod.txt", header = T, sep = "\t", quote = "", strip.white = T)
#tax = read.table(file = "./SFS code/st7_taxa_info_LISA.txt", header = T, sep = "\t", quote = "", strip.white = T) #this is an updated file for l. riparia growth rates. 
colnames(tax) = c("TAXON", 	"METHOD", 	"LM.a", "LM.b",	"LM.p.ash",	"g.a",	"g.b",	"g.c",	"g.d",	"min.cpi",	"max.cpi",	"num.size.classes",	"p.b",	"Growth.equation", 	"min.growth",	"notes")
#source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/wrapper.site.yr_function.txt")
set.seed(123)
oh2.out = wrapper.site.yr(DATA = oh2_bugs, site = "OH2", habitat = "COBBLE", TEMP.COB = oh2_temp1, TEMP.DEP = oh2_temp1, TEMP.TAL = oh2_temp1, first.date = "10/26/10", last.date = "10/25/11",
                           TAXA = tax, temp.corr.igr.cob = c(1,1,1,1,1,1,1,1,1,1,1,1), temp.corr.igr.dep = c(1,1,1,1,1,1,1,1,1,1,1,1), temp.corr.igr.tal = c(1,1,1,1,1,1,1,1,1,1,1,1), wrap = T, boot.num = 50)

saveRDS(oh2.out, file = "oh2_out.rda")
names(oh2.out)
oh2.out$Pintboots.cob
colSums(oh2.out$Pintboots.cob, na.rm = T)

Stream = rep("OH2", length(oh2_temp1))
oh2.int = data.frame(Stream, oh2_tempint, colSums(oh2.out$Pintboots.cob, na.rm = T))
colnames(oh2.int) = c("Stream", "month", "Temperature", "Production")
saveRDS(oh2.int, file = "oh2.int.rda")

sum(colSums(oh2.out$Pintboots.cob, na.rm = T))
## This finally fucking worked!! woohoo. 
# Now work with hver.out$Pboots.cob to get the mean summed community production annually
# then compare with annual mean temperature 

oh2.prod = oh2.out$Pboots.cob
oh2.bio = oh2.out$Bboots.cob

oh2.ann.prod = apply(oh2.prod, 1, sum, na.rm = T)
oh2.ann.bio = apply(oh2.bio, 1, sum, na.rm = T)

oh2.spp.ann.prod2 = data.frame(apply(oh2.out$Pintboots.cob, 1, sum, na.rm = T))
oh2.spp.ann.prod = data.frame(apply(oh2.prod, 2, mean, na.rm = T))

df = data.frame(oh2.spp.ann.prod[,1],oh2.spp.ann.prod2[,1]) 
df[is.na(df)]<- 0

plot(log(df[,1]), log(df[,2]))
abline(a = 0, b = 1)

#oh2.spp.ann.prod = data.frame(apply(oh2.prod, 2, mean, na.rm = T))
colnames(oh2.spp.ann.prod) = "Mean.Annual.Prod"
oh2.spp.ann.prod$Species = rownames(oh2.spp.ann.prod)
oh2.spp.ann.prod$Species.rank = reorder(oh2.spp.ann.prod$Species, -oh2.spp.ann.prod$Mean.Annual.Prod)
oh2.spp.ann.prod$spp.rank = seq(1, length(oh2.spp.ann.prod$Species.rank), length = length(oh2.spp.ann.prod$Species.rank))
oh2.spp.ann.prod$spp.rank = reorder(oh2.spp.ann.prod$spp.rank, - oh2.spp.ann.prod$Mean.Annual.Prod)
oh2.spp.ann.prod[which(oh2.spp.ann.prod$Mean.Annual.Prod == 0),] <- NA
oh2.spp.ann.prod = oh2.spp.ann.prod[! is.na(oh2.spp.ann.prod$spp.rank),]
oh2.spp.ann.prod = oh2.spp.ann.prod %>% mutate(spp.rank = dense_rank(-Mean.Annual.Prod))
  
saveRDS(oh2.spp.ann.prod, file = "oh2.spp.ann.prod.rda")

#no species to remove
####
ggplot(oh2.spp.ann.prod %>% arrange(spp.rank), aes(x = spp.rank, y = log(Mean.Annual.Prod))) + geom_point(size = 2) + geom_path(aes(group = 1), size = 1.5) +
  ylab("ln(Annual production [mg m-2 yr-1])") +  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.title.x = element_blank())

ggplot(oh2.spp.ann.prod, aes(x = Species.rank, y = log(Mean.Annual.Prod))) + geom_point(size = 2) +
  ylab("ln(Annual production [mg m-2 yr-1])") +  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.title.x = element_blank())
###

oh2.spp.ann.bio = data.frame(apply(oh2.bio, 2, mean, na.rm = T))
colnames(oh2.spp.ann.bio) = "Mean.Annual.Bio"
oh2.spp.ann.bio$Species = rownames(oh2.spp.ann.bio)
oh2.spp.ann.bio$Species.rank = reorder(oh2.spp.ann.bio$Species, -oh2.spp.ann.bio$Mean.Annual.Bio)

ggplot(oh2.spp.ann.bio, aes(x = Species.rank, y = Mean.Annual.Bio)) + geom_point(size = 2) +
  ylab("ln(Annual Biomass[mg m-2])") +  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.title.x = element_blank())


mean(oh2.ann.prod)  #92148  #pulls the mean community production estimate
mean(oh2.ann.bio)  #25226    #pulls mean community biomass estimate
mean(oh2.ann.prod)/mean(oh2.ann.bio) # 3.65278  #annual p/b
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