###############################################################################################################
## BFISH data QA/QC ####

#MOUSS Data ###

#created by Benjamin L. Richards (benjaminrichards@noaa.gov) 

###############################################################################################################

library(readxl)
library(plyr)
library(rgdal)
library(ggplot2)

#read in data tables
#import MOUSS sample sheet
cam.sample.input <- read_excel("~/Bottomfish/Bottomfish project/BFISH/BFISH Data/BFISH Data.xlsx", sheet = "CAM_SAMPLE")
maxn.input <- read_excel("~/Bottomfish/Bottomfish project/BFISH/BFISH Data/BFISH Data.xlsx", sheet = "MAXN")

#read in BFISH domain information
fgdb <- "G:/Documents/FBSAD/Bottomfish/Bottomfish project/BFISH/GIS/BFISH.gdb"
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
domain <- as.data.frame(readOGR(dsn = fgdb,layer = "BFISH_PSU_allocation"))

#select drops too dark to annotate
y1 <- subset(maxn.input, SPECIES_CD == "DARK", select = c("DROP_CD","SPECIES_CD"))

y2 <- merge(cam.sample.input, y1, by = "DROP_CD", all.x = T)

y3 <- subset(y2, !(SPECIES_CD %in% "DARK"))

#check TDR depth ranges
summary(y3$OFFICIAL_DEPTH_M)

#merge in domain information
y4 <- merge(y3, domain, by = "PSU", all.x = T)

#flag drifts where depths and more than 10 metere shallower that shallowest part of grid or deeper than deepest part of grid (with 10m buffer)
y4$f1 <- ifelse(y4$TDR_DEPTH_MEAN_M < ((y4$MAX*-1)-10), 
                1,
                0)

y4$f2 <- ifelse(y4$TDR_DEPTH_MEAN_M > ((y4$MIN*-1)+10), 
                1,
                0)


y5 <- ddply(y4, .(DROP_CD), summarize, Depth_toShallow = sum(f1))
y6 <- ddply(y4, .(DROP_CD), summarize, Depth_toDeep = sum(f2))


y7 <- join_all(list(y5, y6), by = 'DROP_CD', type = 'full')

y8 <- subset(y7, Depth_toShallow > 0 | Depth_toDeep > 0)

y9 <- merge(y8, cam.sample.input, by = "DROP_CD")

y10 <- merge (y9, domain, by = "PSU")

y10$MINdepth <- y10$MAX * -1
y10$MAXdepth <- y10$MIN * -1

drop.depth.problems <- subset(y10, select = c("DROP_CD", 
                                              "Depth_toShallow", 
                                              "Depth_toDeep", 
                                              "MINdepth", 
                                              "MAXdepth"))

write.csv(drop.depth.problems, file = "camera drop depth problems.csv", row.names = F)
