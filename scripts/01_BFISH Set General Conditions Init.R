###############################################################################################################
## BFISH data processing ####
## initialization of general conditions ##

## This script is for processing BFISH data from years 2020-present
#created by Benjamin L. Richards (benjamin.richards@noaa.gov) 

###############################################################################################################

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
library(sf)
library(tidyverse)

# General Settings --------------------------------------------------------

setwd("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/WORKING")

# Import any local data (not in Oracle) -----------------------------------
# CRF_SAMPLE <- read_excel("//pickingfish/users/Benjamin.Richards/Documents/Cruises/2021/BFISH_2021_F/Data/CRF/BFISH_2021_F Research Fishing data (corrected).xlsx", sheet = "CRF_SAMPLE")
# CRF_DRIFT <- read_excel("//pickingfish/users/Benjamin.Richards/Documents/Cruises/2021/BFISH_2021_F/Data/CRF/BFISH_2021_F Research Fishing data (corrected).xlsx", sheet = "CRF_DRIFT")
# CRF_CATCH <- read_excel("//pickingfish/users/Benjamin.Richards/Documents/Cruises/2021/BFISH_2021_F/Data/CRF/BFISH_2021_F Research Fishing data (corrected).xlsx", sheet = "CRF_CATCH")

# CAM_SAMPLE <- read_excel("//pickingfish/users/Benjamin.Richards/Documents/Cruises/2021/BFISH_2021_F/Data/MOUSS/BFISH_2021_F_Camera_data_corrected.xlsx", sheet = "CAM_SAMPLE")
# CAM_MAXN <- read_excel("//pickingfish/users/Benjamin.Richards/Documents/Cruises/2021/BFISH_2021_F/Data/MOUSS/BFISH_2021_F_Camera_data_corrected.xlsx", sheet = "CAM_MAXN")
# CAM_LENGTHS <- read_excel("//pickingfish/users/Benjamin.Richards/Documents/Cruises/2021/BFISH_2021_F/Data/MOUSS/BFISH_2021_F_Camera_data_corrected.xlsx", sheet = "CAM_LENGTHS")

dir.create(file.path("inputs"), recursive = TRUE)

#set Deep7 species
d7 <- c("APRU", "ETCA", "ETCO", "HYQU", "PRFI", "PRSI", "PRZO")


# Set sample area & LC parameters ----------------------------------------------------------

#set MOUSS scaler
#m.tot: number of SSU per PSU based on camera radius of 20.2 (n = #PSU, m = #SSU)
#scaler.old <- 194.889
#radius prior to data revisions - 20.2
#radius changed to 18.71548283 or 40.81898499 based on Model 1 and Model 2 Ault/Smith revisions (5/30/18)
#radius changed to 27.6 based on Ault et al. 2018 (8/22/2018)
cam.radius <- 27.60333457
psu.area <- 500 * 500
ssu.area <- pi * (cam.radius^2)
scaler <- psu.area / ssu.area

#set exploited stage specific processing (only fish > 29 cm, length corresponds to 1lb opakapaka)
#lc <- 37.0  (used in Ault et al. 2018)
lc <- 29.0


# Import domain information -----------------------------------------------
domain.init <- sf::st_read("D:/Documents/Bottomfish/Bottomfish Project/BFISH/GIS/BFISH.gdb", layer = "BFISH_PSU")
domain.input <- subset(domain.init, inDomain == "Y", select = c("inDomain","Allocation","Island","PSU","lat_deg","lon_deg","Depth_MEDIAN_m","pctHB", "pctHS","acrstrat","hbstrat","STRATA","STRATA_2020"))
domain.input <- domain.input %>% st_drop_geometry()

# Import Oracle -------------------------------------------------------------
con <- dbConnect(odbc(),
                 Driver = "Oracle in instantclient_21_7",
                 DBQ = "PIC.NMFS.LOCAL",
                 SVC = "BFISH",
                 UID = "BFISH"  #rstudioapi::askForPassword("Database user")
                 ,
                 PWD = "Nasoj!dreibmaj0" #rstudioapi::askForPassword("Database password")
                 )

dbListTables(con,schema = "BFISH")

SPECIES_TABLE <- dbGetQuery(con,"select * from BFISH.SPECIES_LOOKUP")
spp_list.input <- subset(SPECIES_TABLE, select = c(SPECIES_CD, SCIENTIFIC_NAME, COMMON_NAME, SPECIES_TYPE_CD, A, B, GCF))

CRF_SAMPLE <- dbGetQuery(con,"select * from BFISH.CRF_SAMPLE")
CRF_DRIFT <- dbGetQuery(con,"select * from BFISH.CRF_DRIFT")
CRF_CATCH <- dbGetQuery(con,"select * from BFISH.CRF_CATCH")

CAM_SAMPLE <- dbGetQuery(con,"select * from BFISH.CAM_SAMPLE")
CAM_MAXN <- dbGetQuery(con,"select * from BFISH.CAM_MAXN")
CAM_LENGTHS <- dbGetQuery(con,"select * from BFISH.CAM_LENGTHS")


#create ntot or psu.count file based on STRATA
psu.count <- ddply(domain.input, .(STRATA), summarize, PSU_COUNT = length(PSU))
psu.count$STRATUM_WEIGHT <- psu.count$PSU_COUNT/(sum(psu.count$PSU_COUNT))

#create ntot or psu.count file based on STRATA_2020
psu.count.2020 <- ddply(domain.input, .(STRATA_2020), summarize, PSU_COUNT = length(PSU))
psu.count.2020$STRATUM_WEIGHT <- psu.count.2020$PSU_COUNT/(sum(psu.count.2020$PSU_COUNT))

#create ntot or psu.count files by island
psu.count.STRATA.island <- ddply(domain.input, .(Island, STRATA), summarize, PSU_COUNT = length(PSU))
psu.count.STRATA_2020.island <- ddply(domain.input, .(Island, STRATA_2020), summarize, PSU_COUNT = length(PSU))

#check that all years are represented
unique(CRF_SAMPLE$BFISH)
unique(CRF_DRIFT$BFISH)
unique(CRF_CATCH$BFISH)

unique(CAM_SAMPLE$BFISH)
unique(CAM_MAXN$BFISH)
unique(CAM_LENGTHS$BFISH)

# Write out input data tables ---------------------------------------------
write.csv(spp_list.input, file = "./inputs/SPECIES_TABLE.csv", row.names = F)


write.csv(CRF_SAMPLE, file = "./inputs/CRF_SAMPLE.csv", row.names = F)
write.csv(CRF_DRIFT, file = "./inputs/CRF_DRIFT.csv", row.names = F)
write.csv(CRF_CATCH, file = "./inputs/CRF_CATCH.csv", row.names = F)

write.csv(CAM_SAMPLE, file = "./inputs/CAM_SAMPLE.csv", row.names = F)
write.csv(CAM_MAXN, file = "./inputs/CAM_MAXN.csv", row.names = F)
write.csv(CAM_LENGTHS, file = "./inputs/CAM_LENGTHS.csv", row.names = F)

write.csv(domain.init, file = "./inputs/domain_init.csv", row.names = F)
write.csv(domain.input, file = "./inputs/domain_input.csv", row.names = F)

write.csv(psu.count, file = "./inputs/psu_count.csv", row.names = F)
write.csv(psu.count.2020, file = "./inputs/psu_count_STRATA_2020 .csv", row.names = F)

write.csv(psu.count.STRATA.island, file = "./inputs/PSU count by STRATA by Island.csv", row.names = F)
write.csv(psu.count.STRATA_2020.island, file = "./inputs/PSU count by STRAT_2020 by Island.csv", row.names = F)