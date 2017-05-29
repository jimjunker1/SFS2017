##  r script for running Hver production ##

#load in 
library(chron)
options(stringsAsFactors = F)
library(ggplot2)
theme_set(theme_classic())


#import raw data

hengill_full <- read.table(file = './hengill_full.txt', header= T, sep = "\t", quote = "", strip.white = T, check.names = F, row.names = 1)

#subset to just one site
st9_bugs = hengill_full[which(hengill_full$SITE == "ST9"),]

taxon = as.factor(st9_bugs$TAXON)
unique(levels(taxon))


#rename some of the species
st9.fix = which(st9_bugs$TAXON == "Snail")
st9_bugs[st9.fix, "TAXON"] = "Radix balthica" 

st9.fix = which(st9_bugs$TAXON == "Radix peregra")
st9_bugs[st9.fix, "TAXON"] = "Radix balthica" 

st9.fix = which(st9_bugs$TAXON == "Snail 2")
st9_bugs[st9.fix, "TAXON"] = "Galba trunculata" 

st9.fix = which(st9_bugs$TAXON == "Mite")
st9_bugs[st9.fix, "TAXON"] = "Sperchon sp."

st9.fix = which(st9_bugs$TAXON == "Brn Mite")
st9_bugs[st9.fix, "TAXON"] = "Oribatid 1"

st9.fix = which(st9_bugs$TAXON == "Wht Mite")
st9_bugs[st9.fix, "TAXON"] = "Oribatid 1"

st9.fix = which(st9_bugs$TAXON == "Brn Mite 1")
st9_bugs[st9.fix, "TAXON"] = "Oribatid 1"

st9.fix = which(st9_bugs$TAXON == "Brn Mite 2")
st9_bugs[st9.fix, "TAXON"] = "Oribatid 2"

st9.fix = which(st9_bugs$TAXON == "Brn Mite 3")
st9_bugs[st9.fix, "TAXON"] = "Oribatid 3"

st9.fix = which(st9_bugs$TAXON == "Brn mite 3")
st9_bugs[st9.fix, "TAXON"] = "Oribatid 3"

st9.fix = which(st9_bugs$TAXON == "Limnophora")
st9_bugs[st9.fix, "TAXON"] = "Limnophora riparia"

st9.fix = which(st9_bugs$TAXON == "Clinocera sp.")
st9_bugs[st9.fix, "TAXON"] = "Clinocera"

st9.fix = which(st9_bugs$TAXON == "Naididae")
st9_bugs[st9.fix, "TAXON"] = "Nais spp."

st9.fix = which(st9_bugs$TAXON == "Simulium spp.")
st9_bugs[st9.fix, "TAXON"] = "S. vittatum"

st9.fix = which(st9_bugs$TAXON == "S. vittattum")
st9_bugs[st9.fix, "TAXON"] = "S. vittatum"

st9.fix = which(st9_bugs$TAXON == "Simulium spp.")
st9_bugs[st9.fix, "TAXON"] = "S. indet"

st9.fix = which(st9_bugs$TAXON == "S. vitttatum")
st9_bugs[st9.fix, "TAXON"] = "S. vittatum"

st9.fix = which(st9_bugs$TAXON == "Tubificid")
st9_bugs[st9.fix, "TAXON"] = "Tub. 1"

st9.fix = which(st9_bugs$TAXON == "Tubificid 1")
st9_bugs[st9.fix, "TAXON"] = "Tub. 1"

st9.fix = which(st9_bugs$TAXON == "Tubificid 2")
st9_bugs[st9.fix, "TAXON"] = "Tub. 2"

st9.fix = which(st9_bugs$TAXON == "Lumb")
st9_bugs[st9.fix, "TAXON"] = "Oligochaeta indet."

st9.fix = which(st9_bugs$TAXON == "Lumbricidae")
st9_bugs[st9.fix, "TAXON"] = "Oligochaeta indet."

st9.fix = which(st9_bugs$TAXON == "Oligo")
st9_bugs[st9.fix, "TAXON"] = "Oligochaeta indet."

st9.fix = which(st9_bugs$TAXON == "oligo")
st9_bugs[st9.fix, "TAXON"] = "Oligochaeta indet."

st9.fix = which(st9_bugs$TAXON == "Oligo indet")
st9_bugs[st9.fix, "TAXON"] = "Oligochaeta indet."

st9.fix = which(st9_bugs$TAXON == "Midge 3/2")
st9_bugs[st9.fix, "TAXON"] = "Midge 3-2"

st9.fix = which(st9_bugs$TAXON == "Eukifierella sp.")
st9_bugs[st9.fix, "TAXON"] = "Midge 2"

st9.fix = which(st9_bugs$TAXON == "Midge")
st9_bugs[st9.fix, "TAXON"] = "Midge indet."

st9.fix = which(st9_bugs$TAXON == "Tanytarsus")
st9_bugs[st9.fix, "TAXON"] = "Midge 1"

st9.fix = which(st9_bugs$TAXON == "P. cingulatus")
st9_bugs[st9.fix, "TAXON"] = "Potamaphylax"

st9.fix = which(st9_bugs$TAXON == "Midge B")
st9_bugs[st9.fix, "TAXON"] = "Midge 4"

st9.fix = which(st9_bugs$TAXON == "Midge 4/7")
st9_bugs[st9.fix, "TAXON"] = "Midge 4"

st9.fix = which(st9_bugs$TAXON == "Midge C")
st9_bugs[st9.fix, "TAXON"] = "Midge 3"

st9.fix = which(st9_bugs$TAXON == "Midge D")
st9_bugs[st9.fix, "TAXON"] = "Midge 5"

st9.fix = which(st9_bugs$TAXON == "Midge E")
st9_bugs[st9.fix, "TAXON"] = "Midge 6"

rm(st9.fix)

taxon = as.factor(st9_bugs$TAXON)
unique(levels(taxon))
###
#Extract years, months, and days, and re-code them as numeric variables:
year <- as.numeric(as.character(years(chron(dates = as.character(st9_bugs$DATE)))))
month <- as.numeric(months(chron(dates = as.character(st9_bugs$DATE))))
day <- as.numeric(days(chron(dates = as.character(st9_bugs$DATE))))

#Combine year, month, and day into a single julian date variable starting with January 1, 2006 (i.e.- Jan 1, 2006 = day1):
JULIAN <- julian(month, day, year, origin=c(month = 12, day = 31, year = 2005))

#Insert the julian date variable into the dataframe:

st9_bugs <- data.frame(st9_bugs[,1:2], JULIAN, st9_bugs[,3:(dim(st9_bugs)[2])], check.names = F)
st9_bugs = as.data.frame(unclass(st9_bugs))

saveRDS(st9_bugs, file = "st9_bugs.rda")

## Define fuctions stored in separate scripts

source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/get.dates_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/st9igr.prod_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/sf.prod_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/pb.prod_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/wrapper.site.yr_function.R")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/hab.weight_function.txt")
source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/sampinfo.site.yr_function.txt")

#Import temperature data:

st9_temp <- temp_light.m[,c(1,3)]
#find the dates
get.dates(st9_bugs, site = "ST9", habitat = "COBBLE")

#set the date file
st9_temp1 = round(st9_temp[c(3:7,10:11,13:14),2],2)
st9_tempint = st9_temp[c(3:7,10:11,13:14),]

##Import taxa info:
 #L-M parameter, method of production, growth parameters, cpi's, p/b's, etc. for each taxon:
#tax = read.table(file = "./SFS code/st9_taxa_info_LISA.txt", header = T, sep = "\t", quote = "", strip.white = T)
#tax = read.table(file = "./SFS code/st9_taxa_info_LISA_mod.txt", header = T, sep = "\t", quote = "", strip.white = T) #this is an updated file for l. riparia growth rates. 
tax = read.table(file = "./SFS code/st9_taxa_info_LISA_mod.txt", header = T, sep = "\t", quote = "", strip.white = T) #this is an updated file for l. riparia growth rates. 
colnames(tax) = c("TAXON", 	"METHOD", 	"LM.a", "LM.b",	"LM.p.ash",	"g.a",	"g.b",	"g.c",	"g.d",	"min.cpi",	"max.cpi",	"num.size.classes",	"p.b",	"Growth.equation", 	"min.growth",	"notes")
#source("C:/Users/Jim/Documents/Projects/Talk/SFS 2017/SFS2017/R Secondary Production-GC Example/Scripts/wrapper.site.yr_function.txt")
set.seed(123)
st9.out = wrapper.site.yr(DATA = st9_bugs, site = "ST9", habitat = "COBBLE", TEMP.COB = st9_temp1, TEMP.DEP = st9_temp1, TEMP.TAL = st9_temp1, first.date = "07/01/11", last.date = "06/19/12",
                           TAXA = tax, temp.corr.igr.cob = c(1,1,1,1,1,1,1,1,1), temp.corr.igr.dep = c(1,1,1,1,1,1,1,1,1), temp.corr.igr.tal = c(1,1,1,1,1,1,1,1,1), wrap = T, boot.num = 50)

saveRDS(st9.out, file = "st9_out.rda")
names(st9.out)
st9.out$Pintboots.cob
colSums(st9.out$Pintboots.cob, na.rm = T)

Stream = rep("ST9", length(st9_temp1))
st9.int = data.frame(Stream, st9_tempint, colSums(st9.out$Pintboots.cob, na.rm = T))
colnames(st9.int) = c("Stream", "month", "Temperature", "Production")
saveRDS(st9.int, file = "st9.int.rda")

sum(colSums(st9.out$Pintboots.cob, na.rm = T))
## This finally fucking worked!! woohoo. 
# Now work with hver.out$Pboots.cob to get the mean summed community production annually
# then compare with annual mean temperature 

st9.prod = st9.out$Pboots.cob
st9.bio = st9.out$Bboots.cob

st9.ann.prod = apply(st9.prod, 1, sum, na.rm = T)
st9.ann.bio = apply(st9.bio, 1, sum, na.rm = T)

st9.spp.ann.prod2 = data.frame(apply(st9.out$Pintboots.cob, 1, sum, na.rm = T))
st9.spp.ann.prod = data.frame(apply(st9.prod, 2, mean, na.rm = T))

df = data.frame(st9.spp.ann.prod[,1],st9.spp.ann.prod2[,1]) 
df[is.na(df)]<- 0

plot(log(df[,1]), log(df[,2]))
abline(a = 0, b = 1)

#st9.spp.ann.prod = data.frame(apply(st9.prod, 2, mean, na.rm = T))
colnames(st9.spp.ann.prod) = "Mean.Annual.Prod"
st9.spp.ann.prod$Species = rownames(st9.spp.ann.prod)
st9.spp.ann.prod$Species.rank = reorder(st9.spp.ann.prod$Species,  -st9.spp.ann.prod$Mean.Annual.Prod)
st9.spp.ann.prod$spp.rank = seq(1,length(st9.spp.ann.prod$Species.rank), length = length(st9.spp.ann.prod$Species.rank))
st9.spp.ann.prod$spp.rank = reorder(st9.spp.ann.prod$spp.rank,  -st9.spp.ann.prod$Mean.Annual.Prod)
#st9.spp.ann.prod$Species.rank = reorder(st9.spp.ann.prod$Species, -st9.spp.ann.prod$Mean.Annual.Prod)
st9.spp.ann.prod[which(st9.spp.ann.prod$Mean.Annual.Prod == 0),] <- NA
st9.spp.ann.prod = st9.spp.ann.prod[! is.na(st9.spp.ann.prod$spp.rank),]
st9.spp.ann.prod = st9.spp.ann.prod %>% mutate(spp.rank = dense_rank(-Mean.Annual.Prod))

saveRDS(st9.spp.ann.prod, file = "st9.spp.ann.prod.rda")

ggplot(st9.spp.ann.prod %>% arrange(spp.rank), aes(x = spp.rank, y = log(Mean.Annual.Prod))) + geom_point(size = 2) + geom_path(aes(group = 1), size = 1.5) +
  ylab("ln(Annual production [mg m-2 yr-1])") +  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.title.x = element_blank())



#remove species from list
spp.rem = c("Homoptera", "Clinocera pupa", "Limnophora pupa", "Midge pupa", "slug", "Snail egg", "Thrips", "Limnophora egg", "Limnophora pupa", "S. vittatum pupa" )
st9.spp.ann.prod = st9.spp.ann.prod[! st9.spp.ann.prod$Species.rank %in% spp.rem,]

ggplot(st9.spp.ann.prod %>% arrange(spp.rank), aes(x = Species.rank, y = log(Mean.Annual.Prod))) + geom_point(size = 2) + geom_path(aes(group = 1), size = 2) +
  ylab("Annual production [mg m-2 yr-2]") +  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.title.x = element_blank())
# This works awesomely!! git'r done for others.


ggplot(st9.spp.ann.prod, aes(x = spp.rank, y = log(Mean.Annual.Prod), group = 1)) + geom_point(size = 2) + geom_path() +
  ylab("Annual production [mg m-2 yr-2]") +  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), axis.title.x = element_blank())

mean(st9.ann.prod)  #2393    #pulls the mean community production estimate
mean(st9.ann.bio)   #447    #pulls mean community biomass estimate
mean(st9.ann.prod)/mean(st9.ann.bio)  #5.35   #annual p/b

