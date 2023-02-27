## BFISH data processing ####
## This script is for processing BFISH data from years 2020-present ##
## this scripts creates yealry gear specific and merged analysis ready (AR) data sets and biomass calculations ##

#created by Benjamin L. Richards (benjaminrichards@noaa.gov) 

# Settings ----------------------------------------------------------------

#install packages
library(plyr)
library(reshape2)
library(rgdal)
library(ggplot2)
library(gdata)
library(readxl)
library(DBI)
library(odbc)
library(rstudioapi)

setwd("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/WORKING")

#set BFISH_ID
bfish_id <- "BFISH_2022_F"   #"BFISH_2016_F" "BFISH_2017_F" "BFISH_2018_F" "BFISH_2019_F" "BFISH_2020_F" "BFISH_2021_F"

# Step 1. Run set General Conditions Script ------------------------------

# Step 2. Create AR Data for Research Fishing -----------------------------

#subsample import data
fishing.sample.input <- subset(CRF_SAMPLE, BFISH==bfish_id, select = c(BFISH, SAMPLE_ID, PSU, SAMPLE_MEAN_DEPTH_M))
fishing.catch.input <- subset(CRF_CATCH, BFISH==bfish_id, select = c(SAMPLE_ID, SPECIES_CD, LENGTH_CM))

#convert all numbers to numeric (NAs introduced by coercion ok)
fishing.catch.input$LENGTH_CM <- as.numeric(fishing.catch.input$LENGTH_CM)

#subset species list for Deep7 unique record
d7.lh.gcf <- subset(spp_list.input, subset = SPECIES_CD %in% d7)
d7.lh.gcf <- d7.lh.gcf[!duplicated(d7.lh.gcf$SPECIES_CD),]

#merge in domain information
fishing.sample <- merge(fishing.sample.input, subset(domain.input, select = c("PSU","Island","lat_deg","lon_deg","acrstrat", "hbstrat")), by = "PSU")

#recompute 2020 depthstrat based on SAMPLE_MEAN_DEPTH (mean of mean drift depths for each sample)
fishing.sample$depthstrat <- ifelse(fishing.sample$SAMPLE_MEAN_DEPTH_M < 70,
                                   "DZ",
                                   ifelse(fishing.sample$SAMPLE_MEAN_DEPTH_M >= 70 & fishing.sample$SAMPLE_MEAN_DEPTH_M < 110,
                                   "D1",
                                   ifelse(fishing.sample$SAMPLE_MEAN_DEPTH_M >= 110 & fishing.sample$SAMPLE_MEAN_DEPTH_M < 170,
                                   "D2",
                                   ifelse(fishing.sample$SAMPLE_MEAN_DEPTH_M >= 170 & fishing.sample$SAMPLE_MEAN_DEPTH_M < 220,
                                   "D3",
                                   ifelse(fishing.sample$SAMPLE_MEAN_DEPTH_M >= 220 & fishing.sample$SAMPLE_MEAN_DEPTH_M < 330,
                                   "D4",
                                   ifelse(fishing.sample$SAMPLE_MEAN_DEPTH_M >= 330 & fishing.sample$SAMPLE_MEAN_DEPTH_M <= 400,
                                   "D5",
                                   ifelse(fishing.sample$SAMPLE_MEAN_DEPTH_M > 400,
                                   "DZ",
                                   "DZ")))))))


      # build STRATA_2020 -------------------------------------------------------------
fishing.sample$STRATA_2020 <- ifelse(fishing.sample$depthstrat == "D1" &
                                       (fishing.sample$acrstrat == "MA1"| fishing.sample$acrstrat == "MA2") &
                                       (fishing.sample$hbstrat == "HB1" | fishing.sample$hbstrat == "HB2" | fishing.sample$hbstrat == "HB3"), 
                                "S01",
                              ifelse(fishing.sample$depthstrat == "D1"
                                     & fishing.sample$acrstrat == "MA3" &
                                       fishing.sample$hbstrat == "HB1",
                                "S02",
                              ifelse(fishing.sample$depthstrat == "D1" &
                                       fishing.sample$acrstrat == "MA3" &
                                       fishing.sample$hbstrat == "HB2",
                                "S03",
                              ifelse(fishing.sample$depthstrat == "D1" &
                                       fishing.sample$acrstrat == "MA3" &
                                       fishing.sample$hbstrat == "HB3",
                                "S04",
                              ifelse(fishing.sample$depthstrat == "D2" &
                                       fishing.sample$acrstrat == "MA1" &
                                       (fishing.sample$hbstrat == "HB1"|fishing.sample$hbstrat == "HB2"),
                                "S05",
                              ifelse(fishing.sample$depthstrat == "D2" &
                                       fishing.sample$acrstrat == "MA1" &
                                       fishing.sample$hbstrat == "HB3",
                                "S06",
                              ifelse(fishing.sample$depthstrat == "D2" &
                                       fishing.sample$acrstrat == "MA2" &
                                       fishing.sample$hbstrat == "HB1",
                                "S07",
                              ifelse(fishing.sample$depthstrat == "D2" &
                                       fishing.sample$acrstrat == "MA2" &
                                       fishing.sample$hbstrat == "HB2",
                                "S08",
                              ifelse(fishing.sample$depthstrat == "D2" &
                                       fishing.sample$acrstrat == "MA2" &
                                       fishing.sample$hbstrat == "HB3",
                                "S09",
                              ifelse(fishing.sample$depthstrat == "D2" &
                                       fishing.sample$acrstrat == "MA3" &
                                       fishing.sample$hbstrat == "HB1",
                                "S10",
                              ifelse(fishing.sample$depthstrat == "D2" &
                                       fishing.sample$acrstrat == "MA3" &
                                       fishing.sample$hbstrat == "HB2",
                                "S11",
                              ifelse(fishing.sample$depthstrat == "D2" &
                                       fishing.sample$acrstrat == "MA3" &
                                       fishing.sample$hbstrat == "HB3",
                              "S12",
                              ifelse(fishing.sample$depthstrat == "D3" &
                                       (fishing.sample$acrstrat == "MA1"| fishing.sample$acrstrat == "MA2") &
                                       (fishing.sample$hbstrat == "HB1" | fishing.sample$hbstrat == "HB2" | fishing.sample$hbstrat == "HB3"), 
                               "S13",
                              ifelse(fishing.sample$depthstrat == "D3" &
                                       fishing.sample$acrstrat == "MA3" &
                                       fishing.sample$hbstrat == "HB1",
                               "S14",
                              ifelse(fishing.sample$depthstrat == "D3" &
                                       fishing.sample$acrstrat == "MA3" &
                                       fishing.sample$hbstrat == "HB2",
                               "S15",
                              ifelse(fishing.sample$depthstrat == "D3" &
                                       fishing.sample$acrstrat == "MA3" &
                                       fishing.sample$hbstrat == "HB3",
                               "S16",
                              ifelse(fishing.sample$depthstrat == "D4" &
                                        (fishing.sample$acrstrat == "MA1"| fishing.sample$acrstrat == "MA2") &
                                        (fishing.sample$hbstrat == "HB1" | fishing.sample$hbstrat == "HB2"),
                                "S17",
                               ifelse(fishing.sample$depthstrat == "D4" &
                                        fishing.sample$acrstrat == "MA1" &
                                        fishing.sample$hbstrat == "HB3",
                                 "S18",
                               ifelse(fishing.sample$depthstrat == "D4" &
                                        fishing.sample$acrstrat == "MA2" &
                                        fishing.sample$hbstrat == "HB3",
                                 "S19",
                               ifelse(fishing.sample$depthstrat == "D4" &
                                        fishing.sample$acrstrat == "MA3" &
                                        fishing.sample$hbstrat == "HB1",
                                 "S20",
                               ifelse(fishing.sample$depthstrat == "D4" &
                                        fishing.sample$acrstrat == "MA3" &
                                        fishing.sample$hbstrat == "HB2",
                                 "S21",
                               ifelse(fishing.sample$depthstrat == "D4" &
                                        fishing.sample$acrstrat == "MA3" &
                                        fishing.sample$hbstrat == "HB3",
                                 "S22",
                               ifelse(fishing.sample$depthstrat == "D5" &
                                         (fishing.sample$acrstrat == "MA1"| fishing.sample$acrstrat == "MA2") &
                                         (fishing.sample$hbstrat == "HB1" | fishing.sample$hbstrat == "HB2" | fishing.sample$hbstrat == "HB3"),
                                  "S23",
                               ifelse(fishing.sample$depthstrat == "D5" &
                                          fishing.sample$acrstrat == "MA3"&
                                          (fishing.sample$hbstrat == "HB1" | fishing.sample$hbstrat == "HB2" | fishing.sample$hbstrat == "HB3"),
                                   "S24",
                                   "SZZ"))))))))))))))))))))))))



# Step2 cont'd ------------------------------------------------------------

#remove any rows with depth strata Z
fishing.sample <- subset(fishing.sample,!grepl("SZZ", fishing.sample$STRATA_2020))

#remove any rows with NA values
fishing.sample.nona <- na.omit(fishing.sample)

#merge fishing.sample table and catch table
cp.1 <- merge(fishing.sample.nona, fishing.catch.input, by = "SAMPLE_ID")

#count number of spp by Sample_ID
count.1 <- count(cp.1, c("BFISH","STRATA_2020","SAMPLE_MEAN_DEPTH_M", "PSU","SPECIES_CD","SAMPLE_ID"))

#merge in GCF information
count.2 <- merge(count.1, d7.lh.gcf, by = "SPECIES_CD", all.x = T, all.y = T)

#fill in NA data for any D7 spp not seen with first value that is not NA
NonNAindex <- which(!is.na(count.2$SAMPLE_ID))
firstNonNA <- min(NonNAindex)

count.2$BFISH[is.na(count.2$BFISH)] <- bfish_id
count.2$STRATA_2020[is.na(count.2$STRATA_2020)] <- count.2[firstNonNA, "STRATA_2020"]
count.2$SAMPLE_MEAN_DEPTH_M[is.na(count.2$SAMPLE_MEAN_DEPTH_M)] <- count.2[firstNonNA, "SAMPLE_MEAN_DEPTH_M"]
count.2$PSU[is.na(count.2$PSU)] <- count.2[firstNonNA, "PSU"]
count.2$SAMPLE_ID[is.na(count.2$SAMPLE_ID)] <- count.2[firstNonNA, "SAMPLE_ID"]
count.2$freq[is.na(count.2$freq)] <- 0

#apply gcf = 1 for all spp that do not have established gcf
count.2$GCF[is.na(count.2$GCF)] <- 1

#apply gcf to n to compute standard count
count.2$STD_COUNT <- count.2$freq/count.2$GCF

#transpose the catch data to wide table showing av.count per SPECIES_CD by Survey_ID and PSU and fill in 0 for all possible species that were not caught
catch.wide <- dcast(count.2, BFISH + PSU + STRATA_2020 + SAMPLE_MEAN_DEPTH_M + SAMPLE_ID ~ SPECIES_CD, value.var="STD_COUNT", fill=0)

#transpose the wide table back to long form
catch.long <- melt(catch.wide, id.vars = c("BFISH", "PSU", "STRATA_2020", "SAMPLE_MEAN_DEPTH_M", "SAMPLE_ID"), variable.name = "SPECIES_CD", value.name="STD_COUNT")

#aggregate to mean std-count by spp and PSU
count.3 <- ddply(catch.long, .(BFISH, PSU, STRATA_2020, SAMPLE_MEAN_DEPTH_M, SPECIES_CD), summarize, av.count = mean(STD_COUNT))

#subset for Deep 7
count.4 <- subset(count.3, subset = SPECIES_CD %in% d7)

#remove all SPECIES_CD with no observations
count.4[] <- lapply(count.4, function(x) if(is.factor(x)) factor(x) else x)

#compute length frequency
#subset for deep 7
lf.1 <- merge(d7.lh.gcf,fishing.catch.input, all.x=T)
lf.2 <- na.omit(lf.1)
lf.3 <- subset(lf.2, lf.2[,"LENGTH_CM"] >=0)

#merge in fishing.sample for PSU
lf.4 <- merge(lf.3, fishing.sample.nona, by = "SAMPLE_ID")

#count number of observations by species by length
lf.5 <- ddply(lf.4, .(PSU,SPECIES_CD,LENGTH_CM), summarize, len_sum = length(SAMPLE_ID))

#count number of observations by species by PSU
lf.6 <- ddply(lf.5, .(PSU,SPECIES_CD), summarize, tot_sum = sum(len_sum))

lf.7 <- merge(lf.5, lf.6, by = c("PSU", "SPECIES_CD"), all.x = T)

#compute relative frequency
lf.7$rfrq <- lf.7[,"len_sum"]/lf.7[,"tot_sum"]

#merge count and length data
cpue.len.rf <- merge(count.4,lf.7, by = c("PSU", "SPECIES_CD"), all.x = T)

#if av.count = 0 and length is missing, then length = 0
#if av.count > 0 and length is missing, then length = -9
cpue.len.rf$LENGTH_CM <- ifelse(cpue.len.rf$av.count == 0 & is.na(cpue.len.rf$LENGTH_CM),
								0,
								ifelse(cpue.len.rf$av.count > 0 & is.na(cpue.len.rf$LENGTH_CM),
								-9,
								cpue.len.rf$LENGTH_CM
								))

#if rfreq is missing, the refreq = 0
cpue.len.rf$rfrq[is.na(cpue.len.rf$rfrq)] <- 0

#if length = -9, CPUE = av.count, else CPUE = av.count*rfrq
cpue.len.rf$CPUE <- ifelse(cpue.len.rf$LENGTH_CM == -9,
							cpue.len.rf$av.count,
							cpue.len.rf$av.count * cpue.len.rf$rfrq
							)

#create Analysis Ready CPUE at length data set: rename R object and drop unnecessary variables and records with Depth CAT Z
ar.data.fishing <- subset(cpue.len.rf, select = c(BFISH, STRATA_2020, SAMPLE_MEAN_DEPTH_M, PSU, SPECIES_CD, LENGTH_CM, CPUE))
names(ar.data.fishing)[names(ar.data.fishing) == "SAMPLE_MEAN_DEPTH_M"] <- "SAMPLE_DEPTH_M"
ar.data.fishing$GEAR <- "Fishing"
ar.data.fishing$YEAR <- substr(ar.data.fishing$BFISH, 7,10)
ar.data.fishing <- merge((subset(domain.input, select = c("Island", "PSU"))), ar.data.fishing, by = "PSU")

write.csv(ar.data.fishing, file = paste(bfish_id, "AR data Fishing.csv", sep ="_"), row.names = F)



# Step 3. Create AR Data for Camera ---------------------------------------

#import MOUSS sample sheet
cam.sample.input <- subset(CAM_SAMPLE, BFISH==bfish_id, select = c(BFISH, DROP_CD,PSU, OFFICIAL_DEPTH_M))

#import MOUSS MaxN sheet
maxn.input <- subset(CAM_MAXN, BFISH==bfish_id, select = c(DROP_CD, SPECIES_CD, MAXN))

#import MOUSS Lengths sheet
lengths.input <- subset(CAM_LENGTHS, BFISH==bfish_id, select = c(DROP_CD, SPECIES_CD, MEAN_MM))


#merge in domain information
cam.sample <- merge(cam.sample.input, subset(domain.input, select = c("PSU","Island","lat_deg","lon_deg","acrstrat", "hbstrat")), by = "PSU")

#compute sample depth as mean of OFFICIAL_DEPTH_M for each drop and merge back in to sample table
cam.sample.depth <- ddply(cam.sample, .(PSU), summarize, SAMPLE_DEPTH_M = mean(OFFICIAL_DEPTH_M))

cam.sample <- merge(cam.sample, cam.sample.depth, by = "PSU")

#recompute depthstrat based on SAMPLE_DEPTH_M (mean of OFFICIAL_DEPTH_M for each sample)
cam.sample$depthstrat <- ifelse(cam.sample$SAMPLE_DEPTH_M < 70,
                                    "DZ",
                                    ifelse(cam.sample$SAMPLE_DEPTH_M >= 70 & cam.sample$SAMPLE_DEPTH_M < 110,
                                           "D1",
                                           ifelse(cam.sample$SAMPLE_DEPTH_M >= 110 & cam.sample$SAMPLE_DEPTH_M < 170,
                                                  "D2",
                                                  ifelse(cam.sample$SAMPLE_DEPTH_M >= 170 & cam.sample$SAMPLE_DEPTH_M < 220,
                                                         "D3",
                                                         ifelse(cam.sample$SAMPLE_DEPTH_M >= 220 & cam.sample$SAMPLE_DEPTH_M < 330,
                                                                "D4",
                                                                ifelse(cam.sample$SAMPLE_DEPTH_M >= 330 & cam.sample$SAMPLE_DEPTH_M <= 400,
                                                                       "D5",
                                                                       ifelse(cam.sample$SAMPLE_DEPTH_M > 400,
                                                                              "DZ",
                                                                              "DZ")))))))

#build STRATA_2020 catagory for each sample
cam.sample$STRATA_2020 <- ifelse(cam.sample$depthstrat == "D1" &
                                       (cam.sample$acrstrat == "MA1"| cam.sample$acrstrat == "MA2") &
                                       (cam.sample$hbstrat == "HB1" | cam.sample$hbstrat == "HB2" | cam.sample$hbstrat == "HB3"), 
                                     "S01",
                                     ifelse(cam.sample$depthstrat == "D1"
                                            & cam.sample$acrstrat == "MA3" &
                                              cam.sample$hbstrat == "HB1",
                                            "S02",
                                            ifelse(cam.sample$depthstrat == "D1" &
                                                     cam.sample$acrstrat == "MA3" &
                                                     cam.sample$hbstrat == "HB2",
                                                   "S03",
                                                   ifelse(cam.sample$depthstrat == "D1" &
                                                            cam.sample$acrstrat == "MA3" &
                                                            cam.sample$hbstrat == "HB3",
                                                          "S04",
                                                          ifelse(cam.sample$depthstrat == "D2" &
                                                                   cam.sample$acrstrat == "MA1" &
                                                                   (cam.sample$hbstrat == "HB1"|cam.sample$hbstrat == "HB2"),
                                                                 "S05",
                                                                 ifelse(cam.sample$depthstrat == "D2" &
                                                                          cam.sample$acrstrat == "MA1" &
                                                                          cam.sample$hbstrat == "HB3",
                                                                        "S06",
                                                                        ifelse(cam.sample$depthstrat == "D2" &
                                                                                 cam.sample$acrstrat == "MA2" &
                                                                                 cam.sample$hbstrat == "HB1",
                                                                               "S07",
                                                                               ifelse(cam.sample$depthstrat == "D2" &
                                                                                        cam.sample$acrstrat == "MA2" &
                                                                                        cam.sample$hbstrat == "HB2",
                                                                                      "S08",
                                                                                      ifelse(cam.sample$depthstrat == "D2" &
                                                                                               cam.sample$acrstrat == "MA2" &
                                                                                               cam.sample$hbstrat == "HB3",
                                                                                             "S09",
                                                                                             ifelse(cam.sample$depthstrat == "D2" &
                                                                                                      cam.sample$acrstrat == "MA3" &
                                                                                                      cam.sample$hbstrat == "HB1",
                                                                                                    "S10",
                                                                                                    ifelse(cam.sample$depthstrat == "D2" &
                                                                                                             cam.sample$acrstrat == "MA3" &
                                                                                                             cam.sample$hbstrat == "HB2",
                                                                                                           "S11",
                                                                                                           ifelse(cam.sample$depthstrat == "D2" &
                                                                                                                    cam.sample$acrstrat == "MA3" &
                                                                                                                    cam.sample$hbstrat == "HB3",
                                                                                                                  "S12",
                                                                                                                  ifelse(cam.sample$depthstrat == "D3" &
                                                                                                                           (cam.sample$acrstrat == "MA1"| cam.sample$acrstrat == "MA2") &
                                                                                                                           (cam.sample$hbstrat == "HB1" | cam.sample$hbstrat == "HB2" | cam.sample$hbstrat == "HB3"), 
                                                                                                                         "S13",
                                                                                                                         ifelse(cam.sample$depthstrat == "D3" &
                                                                                                                                  cam.sample$acrstrat == "MA3" &
                                                                                                                                  cam.sample$hbstrat == "HB1",
                                                                                                                                "S14",
                                                                                                                                ifelse(cam.sample$depthstrat == "D3" &
                                                                                                                                         cam.sample$acrstrat == "MA3" &
                                                                                                                                         cam.sample$hbstrat == "HB2",
                                                                                                                                       "S15",
                                                                                                                                       ifelse(cam.sample$depthstrat == "D3" &
                                                                                                                                                cam.sample$acrstrat == "MA3" &
                                                                                                                                                cam.sample$hbstrat == "HB3",
                                                                                                                                              "S16",
                                                                                                                                              ifelse(cam.sample$depthstrat == "D4" &
                                                                                                                                                       (cam.sample$acrstrat == "MA1"| cam.sample$acrstrat == "MA2") &
                                                                                                                                                       (cam.sample$hbstrat == "HB1" | cam.sample$hbstrat == "HB2"),
                                                                                                                                                     "S17",
                                                                                                                                                     ifelse(cam.sample$depthstrat == "D4" &
                                                                                                                                                              cam.sample$acrstrat == "MA1" &
                                                                                                                                                              cam.sample$hbstrat == "HB3",
                                                                                                                                                            "S18",
                                                                                                                                                            ifelse(cam.sample$depthstrat == "D4" &
                                                                                                                                                                     cam.sample$acrstrat == "MA2" &
                                                                                                                                                                     cam.sample$hbstrat == "HB3",
                                                                                                                                                                   "S19",
                                                                                                                                                                   ifelse(cam.sample$depthstrat == "D4" &
                                                                                                                                                                            cam.sample$acrstrat == "MA3" &
                                                                                                                                                                            cam.sample$hbstrat == "HB1",
                                                                                                                                                                          "S20",
                                                                                                                                                                          ifelse(cam.sample$depthstrat == "D4" &
                                                                                                                                                                                   cam.sample$acrstrat == "MA3" &
                                                                                                                                                                                   cam.sample$hbstrat == "HB2",
                                                                                                                                                                                 "S21",
                                                                                                                                                                                 ifelse(cam.sample$depthstrat == "D4" &
                                                                                                                                                                                          cam.sample$acrstrat == "MA3" &
                                                                                                                                                                                          cam.sample$hbstrat == "HB3",
                                                                                                                                                                                        "S22",
                                                                                                                                                                                        ifelse(cam.sample$depthstrat == "D5" &
                                                                                                                                                                                                 (cam.sample$acrstrat == "MA1"| cam.sample$acrstrat == "MA2") &
                                                                                                                                                                                                 (cam.sample$hbstrat == "HB1" | cam.sample$hbstrat == "HB2" | cam.sample$hbstrat == "HB3"),
                                                                                                                                                                                               "S23",
                                                                                                                                                                                               ifelse(cam.sample$depthstrat == "D5" &
                                                                                                                                                                                                        cam.sample$acrstrat == "MA3"&
                                                                                                                                                                                                        (cam.sample$hbstrat == "HB1" | cam.sample$hbstrat == "HB2" | cam.sample$hbstrat == "HB3"),
                                                                                                                                                                                                      "S24",
                                                                                                                                                                                                      "SZZ"))))))))))))))))))))))))


#remove any rows with depth strata Z
cam.sample <- subset(cam.sample,!grepl("SZZ", cam.sample$STRATA_2020))

#remove any records with depth Z
cam.sample.1 <- subset(cam.sample, !grepl("Z", cam.sample$STRATA_2020), select = c(BFISH, PSU, DROP_CD, STRATA_2020, SAMPLE_DEPTH_M))

#merge sample and lengths data
c1 <- merge(cam.sample.1, maxn.input, by = "DROP_CD")

#remove dark drops
c2 <- subset(c1, SPECIES_CD !="DARK")

#add dummy variable "1" for MAXN for non D7
c2$MAXN[is.na(c2$MAXN)] <- 1

#merge in GCF information
c2.2 <- merge(c2, d7.lh.gcf, by = "SPECIES_CD", all.x = T, all.y = T)

#fill in NA data for any D7 spp not seen with first value that is not NA
NonNAindex <- which(!is.na(c2.2$DROP_CD))
firstNonNA <- min(NonNAindex)

c2.2$BFISH[is.na(c2.2$BFISH)] <- bfish_id
c2.2$STRATA_2020[is.na(c2.2$STRATA_2020)] <- c2.2[firstNonNA, "STRATA_2020"]
c2.2$SAMPLE_DEPTH_M[is.na(c2.2$SAMPLE_DEPTH_M)] <- c2.2[firstNonNA, "SAMPLE_DEPTH_M"]
c2.2$PSU[is.na(c2.2$PSU)] <- c2.2[firstNonNA, "PSU"]
c2.2$DROP_CD[is.na(c2.2$DROP_CD)] <- c2.2[firstNonNA, "DROP_CD"]
c2.2$MAXN[is.na(c2.2$MAXN)] <- 0

#transpose the data and fill in 0 for all possible species that were not caught
c3.wide <- dcast(c2.2, BFISH + PSU + DROP_CD + STRATA_2020 + SAMPLE_DEPTH_M ~ SPECIES_CD, fun.aggregate = sum, value.var="MAXN", fill=0)

#transpose the wide table back to long form
c4.long <- melt(c3.wide, id.vars = c("BFISH", "PSU", "DROP_CD", "STRATA_2020", "SAMPLE_DEPTH_M"), variable.name = "SPECIES_CD", value.name="MAXN")

#subset for Deep 7
c5 <- subset(c4.long, subset = SPECIES_CD %in% d7)

#remove all SPECIES_CD with no observations
c5[] <- lapply(c5, function(x) if(is.factor(x)) factor(x) else x)

#compute mean CPUE per PSU
c6 <- ddply(c5, .(BFISH, PSU, STRATA_2020, SAMPLE_DEPTH_M, SPECIES_CD), summarize, MAXN_mean = mean(MAXN))

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
ar.data.camera <- subset(c9, select = c(BFISH, STRATA_2020, SAMPLE_DEPTH_M, PSU, SPECIES_CD, LENGTH_CM, CPUE))
ar.data.camera$GEAR <- "Camera"
ar.data.camera$YEAR <- substr(ar.data.camera$BFISH, 7,10)
ar.data.camera <- merge((subset(domain.input, select = c("Island", "PSU"))), ar.data.camera, by = "PSU")

write.csv(ar.data.camera, file = paste(bfish_id, "AR data Camera.csv", sep ="_"), row.names = F)


# Step 4. AR Data Merge ---------------------------------------------------

cam <- read.csv(paste(bfish_id,"_AR data Camera.csv", sep =""))
fish <- read.csv(paste(bfish_id,"_AR data Fishing.csv", sep =""))

a1 <- rbind(cam,fish)

#count number of gears per PSU by STRATA_2020
gear.count <- ddply(a1, .(STRATA_2020, PSU),
                    summarize,
                    gear_count = length(unique((GEAR))))

multigearPSU <- count(gear.count, "gear_count")

#add gear count info to a1
a2 <- merge(a1, gear.count, by = c("STRATA_2020","PSU"))

#subset for PSU with only 1 gear
a3 <- subset(a2, gear_count == 1)

#subset for PSU with 2 gears
a4 <- subset(a2, gear_count == 2)

if (nrow(a4) == 0) {
  
  # if no PSU have 2 gears, write AR data file using a3
  ar.data.all <- a3
  ar.data.all <- subset(ar.data.all, select = c("YEAR", "BFISH", "Island", "STRATA_2020", "SAMPLE_DEPTH_M", "PSU", "SPECIES_CD","LENGTH_CM", "CPUE"))
  
  write.csv(ar.data.all, file = paste(bfish_id, "AR data Both Gears Combined.csv", sep ="_"), row.names = F)
  
  #if there are PSU with 2 gears, do the following
} else {
  # sum CPUE both gear types within a PSU for each species
  a5 <- ddply(a4, .(YEAR, BFISH, Island, STRATA_2020, SAMPLE_DEPTH_M, PSU, SPECIES_CD, GEAR),
              summarize,
              CPUE_gear = sum(CPUE))
  
  # compute mean CPUE per species per PSU
  a6 <- ddply(a5, .(YEAR, BFISH, Island, STRATA_2020, PSU, SPECIES_CD),
              summarize,
              CPUE_tot = mean(CPUE_gear),
              SAMPLE_DEPTH_M = mean(SAMPLE_DEPTH_M))
  
  # remove records without length data
  a7 <- subset(a4, LENGTH_CM > 0)
  
  # sum CPUE by species by PSU
  len.1 <- ddply(a7, .(PSU, SPECIES_CD),
                 summarize,
                 CPUE_spp = sum(CPUE))
  
  # create merged length composition
  len.2 <- merge(a7, len.1, by = c("PSU", "SPECIES_CD"))
  
  # compute relative frequency by length for each species per PSU
  len.2$rfreq <- len.2$CPUE / len.2$CPUE_spp
  
  #merge CPUE data with relative frequncy by length data
  a8 <- merge(a6, len.2[, c("BFISH","PSU","SPECIES_CD","LENGTH_CM","rfreq")], by = c("BFISH","PSU","SPECIES_CD"), all.x = T)
  
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
  a3 <- subset(a3, select = c("YEAR", "BFISH", "Island", "STRATA_2020", "SAMPLE_DEPTH_M", "PSU", "SPECIES_CD","LENGTH_CM", "CPUE"))
  a8 <- subset(a8, select = c("YEAR", "BFISH", "Island", "STRATA_2020", "SAMPLE_DEPTH_M", "PSU", "SPECIES_CD","LENGTH_CM", "CPUE"))
  
  # rbind gear count 1 and gear count 2 data
  ar.data.all <- rbind(a3, a8)

  write.csv(ar.data.all, file = paste(bfish_id, "AR data Both Gears Combined.csv", sep ="_"), row.names = F)
}

# Step 5. Biomass Calculations --------------------------------------------


#if a given year has only a single season survey
ar.data.all <- read.csv(paste(bfish_id,"_AR data Both Gears Combined.csv",sep =""))

#save spp specifc length-weight parameters, removing duplcate spp codes
ab <- subset(spp_list.input, subset = SPECIES_CD %in% d7)
ab <- ab[!duplicated(ab$SPECIES_CD),]

#merge a&b paramters with BFISH specific AR dataset
b.1 <- merge(ar.data.all, ab, by = "SPECIES_CD", all.x = T, all.y = T)

#create abundance at length for exploited length fish (enum)
#if length = 0 or length >= lc, then enum = CPUE, else enum = 0
b.1$enum <- ifelse(b.1$LENGTH_CM == 0 | b.1$LENGTH_CM >= lc,
                   b.1$CPUE,
                   0)

#abundance at length for pre-exploited length fish (pnum)
b.1$pnum <- ifelse(b.1$LENGTH_CM >= 0 & b.1$LENGTH_CM < lc,
                   b.1$CPUE,
                   0)

#create abundance at length for unknown length fish (unum)
b.1$unum <- ifelse(b.1$LENGTH_CM == -9,
                   b.1$CPUE,
                   0)

#create exploited biomass in kilogramS
b.1$ebiom_kg <-ifelse(b.1$LENGTH_CM >= 0,
                      b.1$enum * (b.1$A * (b.1$LENGTH_CM^b.1$B)),
                      0)

#sum enum, unum, and ebiom_kg by spp by YEAR, BFISH, and PSU
b.2 <- ddply(b.1, .(YEAR, BFISH, PSU, STRATA_2020, SPECIES_CD), summarize,
             biom = sum(ebiom_kg),
             abund = sum(enum),
             uabund = sum(unum))

#remove records with unknown life stage (LENGTH_CM)
b.3 <- subset(b.2, uabund == 0)

#write out file with relative abundance/biomass data by species by PSU
b.3.wide <- dcast(b.3, YEAR + BFISH + PSU + STRATA_2020 ~ SPECIES_CD, fun.aggregate = sum, value.var="biom", fill=0)
write.csv(b.3.wide, file = paste(bfish_id, "Deep7 Biomass by PSU & Species.csv", sep =" "), row.names = F)

#create stratum mean and variance for relative abundance and biomass by YEAR, by STRATA_2020, by Species
d7.biomass.spp.strata <- ddply(b.3, .(YEAR, BFISH, SPECIES_CD, STRATA_2020), summarize,
                               n = length(PSU),
                               avCPUE = mean(abund),
                               avBiom = mean(biom),
                               varCPUE = var(abund),
                               varBiom = var(biom))

#assign year to data object
assign(paste(bfish_id, "d7.biomass.spp.strata", sep = "."),d7.biomass.spp.strata)

#write out file with relative abundance/biomass data by species by STRATA_2020
d7.biomass.spp.strata.wide <- dcast(d7.biomass.spp.strata, YEAR + BFISH + STRATA_2020 ~ SPECIES_CD, fun.aggregate = sum, value.var="avBiom", fill=0)
write.csv(d7.biomass.spp.strata.wide, file = paste(bfish_id, "Deep7 Relative Biomass by Strata & Species.csv", sep =" "), row.names = F)

#bring in STRATA_2020 weighting factors
b.5 <- merge(d7.biomass.spp.strata, psu.count.2020, by = "STRATA_2020", all.x = T)

#compute total number of SSU per stratum
b.5$nmtot <- scaler * b.5$PSU_COUNT

#compute sampling fraction
b.5$f <- b.5$n / b.5$PSU_COUNT

#compute stratum variance of mean CPUE & biomass
b.5$vbar_cpue <- ((1 - b.5$f) * b.5$varCPUE/b.5$n)
b.5$vbar_biom <- ((1 - b.5$f) * b.5$varBiom/b.5$n)

#compute weighted stratum mean CPUE & biomass
b.5$wcpue <- b.5$STRATUM_WEIGHT * b.5$avCPUE
b.5$wBiom <- b.5$STRATUM_WEIGHT * b.5$avBiom

#compute weighted stratum variance of mean CPUE & biomass
b.5$wvbar_cpue <- b.5$STRATUM_WEIGHT^2 * b.5$vbar_cpue
b.5$wvbar_biom <- b.5$STRATUM_WEIGHT^2 * b.5$vbar_biom

#compute absolute stratum abundance & biomass
b.5$str_abund <- b.5$nmtot * b.5$avCPUE
b.5$str_biom <- b.5$nmtot * b.5$avBiom

#compute variance of absolute stratum abundance & biomass
b.5$vbar_str_abund <- b.5$nmtot^2 * b.5$vbar_cpue
b.5$vbar_str_biom <- b.5$nmtot^2 * b.5$vbar_biom

#sum across species for domain computations
d7.biomass.spp <- ddply(b.5, .(YEAR, SPECIES_CD), summarize,
                        n = sum(n),
                        Biomass_kg = sum(str_biom),
                        vbar_Biomass_kg = sum(vbar_str_biom),
                        CPUE = sum(wcpue),
                        vbar_CPUE = sum(wvbar_cpue),
                        abund = sum(str_abund),
                        vbar_abund = sum(vbar_str_abund),
                        avBiomass_kg = sum(wBiom),
                        vbar_avBiomass_kg = sum(wvbar_biom))

#compute standard errors and CV
d7.biomass.spp$SE_CPUE <- sqrt(d7.biomass.spp$vbar_CPUE)
d7.biomass.spp$SE_abund <- sqrt(d7.biomass.spp$vbar_abund)
d7.biomass.spp$SE_avBiomass_kg <- sqrt(d7.biomass.spp$vbar_avBiomass_kg)
d7.biomass.spp$SE_Biomass_kg <- sqrt(d7.biomass.spp$vbar_Biomass_kg)

d7.biomass.spp$CV_CPUE <- (d7.biomass.spp$SE_CPUE / d7.biomass.spp$CPUE) * 100

#bring in COMMON_NAME, rearrange column order, & sort
d7.biomass.spp <- merge(d7.biomass.spp, unique(SPECIES_TABLE[, c("SPECIES_CD", "COMMON_NAME")]), by = "SPECIES_CD", all.x = T, all.y = F)
d7.biomass.spp <- d7.biomass.spp[c("YEAR",
                                         "SPECIES_CD",
                                         "n",
                                         "COMMON_NAME",
                                         "CPUE", "SE_CPUE",
                                         "abund", "SE_abund",
                                         "Biomass_kg",
                                         "SE_Biomass_kg",
                                         "CV_CPUE",
                                         "avBiomass_kg",
                                         "vbar_CPUE",
                                         "vbar_abund",
                                         "vbar_Biomass_kg",
                                         "SE_abund",
                                         "SE_avBiomass_kg",
                                         "SE_Biomass_kg")
                                 ][order(d7.biomass.spp$COMMON_NAME),]

#assign year to data object
assign(paste(bfish_id, "d7.biomass.spp", sep = "."),d7.biomass.spp)

#write out file with abundance/biomass data by species
write.csv(d7.biomass.spp, file = paste(bfish_id, "Deep7 Biomass by Species.csv", sep =" "), row.names = F)
View(d7.biomass.spp)

#compute overall D7 biomass
d7.biomass.total <- ddply(d7.biomass.spp, .(YEAR), summarize,
                          Biomass_kg = sum(Biomass_kg),
                          var_Biomass_kg = sum(vbar_Biomass_kg))


#compute standard error
d7.biomass.total$SE_Biomass_kg <- sqrt(d7.biomass.total$var_Biomass_kg)

#compute CV of cpue by spp
d7.biomass.total$CV_Biomass_kg <- (d7.biomass.total$SE_Biomass / d7.biomass.total$Biomass_kg) * 100

#add in TOTAL for SPECIES_CD to assist with later graphing
d7.biomass.total$SPECIES_CD <- "TOTAL"

#assign year to data object
assign(paste(bfish_id, "d7.biomass.total", sep = "."),d7.biomass.total)

#write out file with Deep 7 abundance/biomass data
write.csv(d7.biomass.total, file = paste(bfish_id, "Deep7 Biomass Total.csv", sep =" "), row.names = F)

save.image("workspace file.RData")

