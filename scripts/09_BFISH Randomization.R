###############################################################################################################
## BFISH data processing ####
## BFISH Survey Design Analysis ##

## This script is for processing BFISH data from years 2020-present ##


#this script creates a randomized selection of PSU based on MS-Excel derived allocation

#created by Benjamin L. Richards (benjamin.richards@noaa.gov) 
###############################################################################################################

#set wd to appropriate Sampling Design directory
setwd("~/Cruises/2023/BFISH_2023_F/Sampling Design")

#set BFISH ID
bfish_id.alloc <- "BFISH_2023_F"

grids.psu <- read_excel("BFISH_2023_F allocation table.xlsx", 
                        sheet = "nPSU")
View(grids.psu)

#subset domain to include only PSU suitable for allocation
domain.allocation <- subset(domain.input, Allocation == "Y")


#create strata by island variable
domain.allocation$STRATA_2020byISLAND <- paste(domain.allocation$STRATA_2020, domain.allocation$Island, sep = "")

#create sample allocation data frame
sample.allocation <- data.frame()

#loop through the available Strata within domain randomly selecting "nh" rows from <domain> based on "nh" in <allocation>  
for(i in 1:length(grids.psu$STRATAbyISLAND)) {
  temp <- subset(domain.allocation, domain.allocation$STRATA_2020byISLAND == as.character(grids.psu$STRATAbyISLAND)[i])
  sample <- temp[sample(nrow(temp), grids.psu$nh2[i], replace = FALSE),]
  sample.allocation <- rbind(sample.allocation,sample)
}

grids <- subset(sample.allocation, select = "PSU")
colnames(grids)[1] = paste0(bfish_id.alloc,"_PSU")
grids$Gear <- NA
grids$rand <- runif(nrow(grids))

View(grids)

#write a .csv file with the PSU Grid_IDs for joining in ArcGIS
write.csv(grids, file = paste0(bfish_id.alloc,"_grids_for_Arc.csv"), row.names = F)
