###############################################################################################################
#### BFISH data processing ####
## analysis ready data processing script for MOUSS camera data ##

## This script is for processing BFISH data from years 2020-present ##


#created by Benjamin L. Richards (benjaminrichards@noaa.gov) 

###############################################################################################################

#######################
##### dependencies ####
#######################
if(!exists("CAM_SAMPLE")){source("G:/Documents/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/GitHub/BFISH/1_BFISH Set General Conditions Init.r")}

###################
##### settings ####
###################

#set BFISH_ID
bfish_id <- "BFISH_2020_F"     #"BFISH_2016_F" "BFISH_2017_F" "BFISH_2018_F" "BFISH_2019_F" "BFISH_2020_F"

#######################
##### Data import #####
#######################

#import MOUSS sample sheet
cam.sample.input <- subset(CAM_SAMPLE, BFISH==bfish_id, select = c(BFISH, DROP_CD,PSU, OFFICIAL_DEPTH_M))

#import MOUSS MaxN sheet
maxn.input <- subset(CAM_MAXN, BFISH==bfish_id, select = c(DROP_CD, SPECIES_CD, MAXN))

#import MOUSS Lengths sheet
lengths.input <- subset(CAM_LENGTHS, BFISH==bfish_id, select = c(DROP_CD, SPECIES_CD, MEAN_MM))


#merge in domain information
cam.sample <- merge(cam.sample.input, subset(domain.input, select = c("PSU","Island","lat_deg","lon_deg","HARD_CAT", "SLOPE_CAT")), by = "PSU")

#compute sample depth as mean of OFFICIAL_DEPTH_M for each drop and merge back in to sample table
cam.sample.depth <- ddply(cam.sample, .(PSU), summarize, SAMPLE_DEPTH_M = mean(OFFICIAL_DEPTH_M))

cam.sample <- merge(cam.sample, cam.sample.depth, by = "PSU")

#recompute DEPTH_CAT based on MOUSS TDR Dapths
cam.sample$DEPTH_CAT <- ifelse(cam.sample$SAMPLE_DEPTH_M >= 70 & cam.sample$SAMPLE_DEPTH_M < 200,
                                   "S",
                                   ifelse(cam.sample$SAMPLE_DEPTH_M >= 200 & cam.sample$SAMPLE_DEPTH_M < 300,
                                          "M",
                                          ifelse(cam.sample$SAMPLE_DEPTH_M >= 300 & cam.sample$SAMPLE_DEPTH_M <= 400,
                                                 "D",
                                                 "Z")))

#build STRATA catagory for cam samples
cam.sample$STRATA <- paste(cam.sample$HARD_CAT, cam.sample$SLOPE_CAT, cam.sample$DEPTH_CAT, sep = "_")

#remove any records with depth Z
cam.sample.1 <- subset(cam.sample, !grepl("Z", cam.sample$STRATA), select = c(BFISH, PSU, DROP_CD, STRATA, SAMPLE_DEPTH_M))

#merge sample and lengths data
c1 <- merge(cam.sample.1, maxn.input, by = "DROP_CD")

#remove dark drops
c2 <- subset(c1, SPECIES_CD !="DARK")

#add dummy variable "1" for MAXN for non D7
c2$MAXN[is.na(c2$MAXN)] <- 1


####################

#merge in GCF information
c2.2 <- merge(c2, d7.lh.gcf, by = "SPECIES_CD", all.x = T, all.y = T)

#fill in NA data for any D7 spp not seen with first value that is not NA
NonNAindex <- which(!is.na(c2.2$DROP_CD))
firstNonNA <- min(NonNAindex)

c2.2$BFISH[is.na(c2.2$BFISH)] <- bfish_id
c2.2$STRATA[is.na(c2.2$STRATA)] <- c2.2[firstNonNA, "STRATA"]
c2.2$SAMPLE_DEPTH_M[is.na(c2.2$SAMPLE_DEPTH_M)] <- c2.2[firstNonNA, "SAMPLE_DEPTH_M"]
c2.2$PSU[is.na(c2.2$PSU)] <- c2.2[firstNonNA, "PSU"]
c2.2$DROP_CD[is.na(c2.2$DROP_CD)] <- c2.2[firstNonNA, "DROP_CD"]
c2.2$MAXN[is.na(c2.2$MAXN)] <- 0

#######################

#transpose the data and fill in 0 for all possible species that were not caught
c3.wide <- dcast(c2.2, BFISH + PSU + DROP_CD + STRATA + SAMPLE_DEPTH_M ~ SPECIES_CD, fun.aggregate = sum, value.var="MAXN", fill=0)

#transpose the wide table back to long form
c4.long <- melt(c3.wide, id.vars = c("BFISH", "PSU", "DROP_CD", "STRATA", "SAMPLE_DEPTH_M"), variable.name = "SPECIES_CD", value.name="MAXN")

#subset for Deep 7
c5 <- subset(c4.long, subset = SPECIES_CD %in% d7)

#remove all SPECIES_CD with no observations
c5[] <- lapply(c5, function(x) if(is.factor(x)) factor(x) else x)

#compute mean CPUE per PSU
c6 <- ddply(c5, .(BFISH, PSU, STRATA, SAMPLE_DEPTH_M, SPECIES_CD), summarize, MAXN_mean = mean(MAXN))

# compute number of fish measured per PSU
lengths <- merge(lengths.input, subset(cam.sample.input, select = c("DROP_CD","PSU")), by = "DROP_CD")
c7 <- ddply(lengths, .(PSU, SPECIES_CD), summarize, len_count = length(MEAN_MM))

#merge count back into c6
c8 <- merge(lengths, c7, by = c("PSU", "SPECIES_CD"))

#compute relative frequncy for each length
c8$rfreq <- 1 / c8$len_count

#merge c8 with c6
c9 <- merge(c6, c8, by = c("PSU", "SPECIES_CD"), all.x = T)

#convert legnths to cm
c9$LENGTH_CM <- c9$MEAN_MM / 10

#compute CPUE
c9$CPUE <- c9$MAXN_mean * c9$rfreq

############################################

#if MaxN = 0 and length is missing, then length = 0
#if MaxN > 0 and length is missing, then length = -9
c9$LENGTH_CM <- ifelse(c9$MAXN_mean == 0,
                                0,
                                ifelse(c9$MAXN_mean > 0 & is.na(c9$LENGTH_CM),
                                       -9,
                                       c9$LENGTH_CM
                                ))

# if LENGTH_CM is 0 or -9, set CPUE to MAXN_mean
c9$CPUE <- ifelse(c9$LENGTH_CM == 0 | c9$LENGTH_CM == -9,
                       c9$MAXN_mean,
                             (c9$MAXN_mean * c9$rfreq)
                       )

#create analysis ready dataset
ar.data.camera <- subset(c9, select = c(BFISH, STRATA, SAMPLE_DEPTH_M, PSU, SPECIES_CD, LENGTH_CM, CPUE))
ar.data.camera$GEAR <- "Camera"
ar.data.camera$YEAR <- substr(ar.data.camera$BFISH, 7,10)
ar.data.camera <- merge((subset(domain.input, select = c("Island", "PSU"))), ar.data.camera, by = "PSU")

write.csv(ar.data.camera, file = paste(bfish_id, "AR data Camera.csv", sep ="_"), row.names = F)
