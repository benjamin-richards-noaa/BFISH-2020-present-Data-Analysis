###############################################################################################################
## BFISH data QA/QC ####

#Research Fihsing Data ###

#created by Benjamin L. Richards (benjaminrichards@noaa.gov) 

###############################################################################################################

library(readxl)
library(plyr)
library(rgdal)
library(ggplot2)

#read in data tables
fishing.sample.input <- read_excel("~/Bottomfish/Bottomfish project/BFISH/BFISH Data/BFISH Data.xlsx", sheet = "RF_SAMPLE")
fishing.drift.input <- read_excel("~/Bottomfish/Bottomfish project/BFISH/BFISH Data/BFISH Data.xlsx", sheet = "DRIFT")
fishing.catch.input <- read_excel("~/Bottomfish/Bottomfish project/BFISH/BFISH Data/BFISH Data.xlsx", sheet = "CATCH")

#read in BFISH domain information
#import domain information
fgdb <- "G:/Documents/FBSAD/Bottomfish/Bottomfish project/BFISH/GIS/BFISH.gdb"
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
domain <- as.data.frame(readOGR(dsn = fgdb,layer = "BFISH_PSU_allocation"))

#merge drift and domain tables
m1 <- merge(fishing.drift.input, domain, by.x = "Target_Grid_ID", by.y = "Cell_ID")

#flag drifts where depths and more than 10 metere shallower that shallowest part of grid or deeper than deepest part of grid (with 10m buffer)
m1$f1 <- ifelse(m1$Start_Depth_m < ((m1$MAX*-1)-10), 
                       1,
                       0)

m1$f2 <- ifelse(m1$Start_Depth_m > ((m1$MIN*-1)+10), 
                                      1,
                                      0)

m1$f3 <- ifelse(m1$End_Depth_m < ((m1$MAX*-1)-10), 
                              1,
                              0)

m1$f4 <- ifelse(m1$End_Depth_m > ((m1$MIN*-1)+10), 
                                      1,
                                      0)

m2 <- ddply(m1, .(sampleID), summarize, Start_Depth_toShallow = sum(f1))
m3 <- ddply(m1, .(sampleID), summarize, Start_Depth_toDeep = sum(f2))
m4 <- ddply(m1, .(sampleID), summarize, End_Depth_toShallow = sum(f3))
m5 <- ddply(m1, .(sampleID), summarize, End_Depth_toDeep = sum(f4))

m6 <- join_all(list(m2,m3,m4,m5), by = 'sampleID', type = 'full')

m7 <- subset(m6, Start_Depth_toShallow > 0 | Start_Depth_toDeep > 0 | End_Depth_toShallow > 0 | End_Depth_toDeep > 0 )

m8 <- merge(m7, fishing.sample.input, by.x = "sampleID", by.y = "SAMPLE_ID")

m9 <- merge (m8, domain, by = "PSU")

m9$MINdepth <- m9$MAX * -1
m9$MAXdepth <- m9$MIN * -1

drift.depth.problems <- subset(m9, select = c("sampleID", 
                                              "Start_Depth_toShallow", 
                                              "Start_Depth_toDeep", 
                                              "End_Depth_toShallow", 
                                              "End_Depth_toDeep", 
                                              "MINdepth", 
                                              "MAXdepth"))


write.csv(drift.depth.problems, file = "drift depth problems.csv", row.names = F)

#################################################################################################

#compute sample depth strata
n1 <- merge(fishing.sample.input, domain, by ="PSU")

n1$SAMPLE_DEPTH_CAT <- ifelse(n1$Sample_Mean_Depth_m >= 75 & n1$Sample_Mean_Depth_m < 200,
                              "S",
                              ifelse(n1$Sample_Mean_Depth_m >= 200 & n1$Sample_Mean_Depth_m < 300,
                                     "M",
                                     ifelse(n1$Sample_Mean_Depth_m >= 300 & n1$Sample_Mean_Depth_m <= 400,
                                            "D",
                                            "Z")))

summary(as.factor(n1$SAMPLE_DEPTH_CAT))

n1$MEDIAN <- n1$MEDIAN * -1

#compute map depth strata
n1$MAP_DEPTH_CAT <- ifelse(n1$MEDIAN >= 75 & n1$MEDIAN < 200,
                              "S",
                              ifelse(n1$MEDIAN >= 200 & n1$MEDIAN < 300,
                                     "M",
                                     ifelse(n1$MEDIAN >= 300 & n1$MEDIAN <= 400,
                                            "D",
                                            "Z")))

summary(as.factor(n1$MAP_DEPTH_CAT))

#test for differneces between sample and map depth strat
n1$depth_cat_flag <- ifelse (n1$SAMPLE_DEPTH_CAT == n1$MAP_DEPTH_CAT, 0, 1)
n1$diff <- abs(n1$Sample_Mean_Depth_m - n1$MEDIAN)

depth.strata.differences <- subset(n1, depth_cat_flag == 1, select = c("SAMPLE_ID",
                                                                     "PSU",
                                                                     "SAMPLE_DEPTH_CAT",
                                                                     "MAP_DEPTH_CAT",
                                                                     "Sample_Mean_Depth_m",
                                                                     "MEDIAN",
                                                                     "diff"))

write.csv(depth.strata.differences, file = "depth strata differences.csv", row.names = F)

#################################################################################################
#check catch lengths

l1 <- subset(fishing.catch.input, LENGTH_CM > 0 &
               (SPECIES_CD == "APRU" | 
                  SPECIES_CD == "ETCA" |
                  SPECIES_CD == "ETCO" |
                  SPECIES_CD == "HYQU" |
                  SPECIES_CD == "PRFI" |
                  SPECIES_CD == "PRSI" |
                  SPECIES_CD == "PRZO"))

ggplot(l1, aes(x=LENGTH_CM)) + geom_histogram(binwidth=.5, colour="black", fill="white") + 
  facet_grid(SPECIES_CD ~ .)