###############################################################################################################
#### BFISH data processing ####
## merging of Research Fishing and Camera-based AR data sets ##

## This script is for processing BFISH data from years 2020-present ##


#created by Benjamin L. Richards (benjaminrichards@noaa.gov) 

###############################################################################################################

###################
##### settings ####
###################

#set bfish_id for muli-gear missions
bfish_id <- "BFISH_2020_F"      #"BFISH_2016_F" "BFISH_2017_F" "BFISH_2018_F" "BFISH_2019_F" "BFISH_2020_F"

########################################################

cam <- read.csv(paste(bfish_id,"_AR data Camera.csv", sep =""))
fish <- read.csv(paste(bfish_id,"_AR data Fishing.csv", sep =""))

a1 <- rbind(cam,fish)

#count number of gears per PSU
gear.count <- ddply(a1, .(STRATA, PSU), summarize, gear_count = length(unique((GEAR))))

multigearPSU <- count(gear.count, "gear_count")

#add gear count info to a1
a2 <- merge(a1, gear.count, by = c("STRATA","PSU"))

#subset for PSU with only 1 gear
a3 <- subset(a2, gear_count == 1)

#subset for PSU with 2 gears
a4 <- subset(a2, gear_count == 2)

if (nrow(a4) == 0) {
  
  # subset for PSU with only 1 gear
  ar.data.all <- a3
  
  ar.data.all <- subset(ar.data.all, select = c("YEAR", "BFISH", "Island", "STRATA", "SAMPLE_DEPTH_M", "PSU", "SPECIES_CD","LENGTH_CM", "CPUE"))
  #assign(paste("ar.data.all", mission, sep = "."),ar.data.all)
  
  write.csv(ar.data.all, file = paste(bfish_id, "AR data Both Gears Combined.csv", sep ="_"), row.names = F)
  
} else {

  # sum CPUE both gear types within a PSU for each species
  a5 <- ddply(a4, .(YEAR, BFISH, Island, STRATA, SAMPLE_DEPTH_M, PSU, SPECIES_CD, GEAR), summarize, CPUE_gear = sum(CPUE))
  
  # compute mean CPUE per species per PSU
  a6 <- ddply(a5, .(YEAR, BFISH, Island, STRATA, SAMPLE_DEPTH_M, PSU, SPECIES_CD), summarize, CPUE_tot = mean(CPUE_gear))
  
  # remove records with CPUE of 0 or -9
  a7 <- subset(a4, LENGTH_CM > 0)
  
  # sum CPUE by species by PSU
  len.1 <- ddply(a7, .(PSU, SPECIES_CD), summarize, CPUE_spp = sum(CPUE))
  
  len.2 <- merge(a7, len.1, by = c("PSU", "SPECIES_CD"))
  
  # compute relative frequency for each species per PSU
  len.2$rfreq <- len.2$CPUE / len.2$CPUE_spp
  
  a8 <- merge(a6, len.2, by = c("YEAR", "BFISH", "Island", "STRATA", "SAMPLE_DEPTH_M", "PSU", "SPECIES_CD"), all.x = T)
  a8 <- subset(a8, select = c("YEAR", "BFISH", "Island", "STRATA", "SAMPLE_DEPTH_M","PSU","SPECIES_CD","LENGTH_CM","GEAR","gear_count","rfreq","CPUE_tot"))
  
  # if CPUE_tot = 0 and length is missing, then length = 0
  # if CPUE_tot > 0 and length is missing, then length = -9
  a8$LENGTH_CM <- ifelse(a8$CPUE_tot == 0,
                         0,
                         ifelse(a8$CPUE_tot > 0 & is.na(a8$LENGTH_CM),
                                -9,
                                a8$LENGTH_CM))
  
  # recompute CPUE
  a8$CPUE <- ifelse(a8$LENGTH_CM == -9,
                    a8$CPUE_tot,
                    (a8$CPUE_tot * a8$rfreq)
  )
  
  # fill in missing CPUE values
  a8$CPUE <- ifelse(is.na(a8$CPUE), 0, a8$CPUE)
  
  # drop unneeded columns before mergeing
  a9 <- subset(a8, select = c("YEAR", "BFISH", "Island", "STRATA", "SAMPLE_DEPTH_M", "PSU", "SPECIES_CD","LENGTH_CM", "CPUE","GEAR","gear_count"))
  
  # rbind gear count 1 and gear count 2 data
  ar.data.all <- rbind(a3, a9)
  
  # remove extra collumns
  ar.data.all <- subset(ar.data.all, select = c("YEAR", "BFISH", "Island", "STRATA", "SAMPLE_DEPTH_M", "PSU", "SPECIES_CD","LENGTH_CM", "CPUE"))
  #assign(paste("ar.data.all", bfish_id, sep = "."),ar.data.all)
  
  write.csv(ar.data.all, file = paste(bfish_id, "AR data Both Gears Combined.csv", sep ="_"), row.names = F)
}
