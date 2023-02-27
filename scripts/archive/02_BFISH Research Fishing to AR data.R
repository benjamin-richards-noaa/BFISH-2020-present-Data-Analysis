###############################################################################################################
## BFISH data processing ####
## analysis ready data processing script for research fishing data ##

## This script is for processing BFISH data from years 2020-present ##

#created by Benjamin L. Richards (benjaminrichards@noaa.gov) 

###############################################################################################################

###################
##### settings ####
###################

#set BFISH_ID
bfish_id <- "BFISH_2020_F"   #"BFISH_2016_F" "BFISH_2017_F" "BFISH_2018_F" "BFISH_2019_F" "BFISH_2020_F"


#######################
##### Data import #####
#######################

#subsample import data
fishing.sample.input <- subset(CRF_SAMPLE, BFISH==bfish_id, select = c(BFISH, SAMPLE_ID, PSU, SAMPLE_MEAN_DEPTH_M))
fishing.catch.input <- subset(CRF_CATCH, BFISH==bfish_id, select = c(SAMPLE_ID, SPECIES_CD, LENGTH_CM))

#convert all numbers to numeric
fishing.catch.input$LENGTH_CM <- as.numeric(fishing.catch.input$LENGTH_CM)

#subset for Deep7 unique record
d7.lh.gcf <- subset(spp_list.input, subset = SPECIES_CD %in% d7)
d7.lh.gcf <- d7.lh.gcf[!duplicated(d7.lh.gcf$SPECIES_CD),]

#merge in domain information
fishing.sample <- merge(fishing.sample.input, subset(domain.input, select = c("PSU","Island","lat_deg","lon_deg","HARD_CAT", "SLOPE_CAT")), by = "PSU")

#recompute DEPTH_CAT based on SAMPLE_MEAN_DEPTH (mean of mean drift depths for each sample)
fishing.sample$DEPTH_CAT <- ifelse(fishing.sample$SAMPLE_MEAN_DEPTH_M >= 70 & fishing.sample$SAMPLE_MEAN_DEPTH_M < 200,
                                   "S",
                                   ifelse(fishing.sample$SAMPLE_MEAN_DEPTH_M >= 200 & fishing.sample$SAMPLE_MEAN_DEPTH_M < 300,
                                          "M",
                                          ifelse(fishing.sample$SAMPLE_MEAN_DEPTH_M >= 300 & fishing.sample$SAMPLE_MEAN_DEPTH_M <= 400,
                                                 "D",
                                                 "Z")))
#build STRATA catagory for each sample
fishing.sample$STRATA <- paste(fishing.sample$HARD_CAT, fishing.sample$SLOPE_CAT, fishing.sample$DEPTH_CAT, sep = "_")

#remove any rows with depth strata Z
fishing.sample <- subset(fishing.sample,!grepl("Z", fishing.sample$STRATA))

#remove any rows with NA values
fishing.sample.nona <- na.omit(fishing.sample)

#merge fishing.sample table and catch table
cp.1 <- merge(fishing.sample.nona, fishing.catch.input, by = "SAMPLE_ID")

#count number of spp by Sample_ID
count.1 <- count(cp.1, c("BFISH","STRATA","SAMPLE_MEAN_DEPTH_M", "PSU","SAMPLE_ID","SPECIES_CD"))

#merge in GCF information
count.2 <- merge(count.1, d7.lh.gcf, by = "SPECIES_CD", all.x = T, all.y = T)

#fill in NA data for any D7 spp not seen with first value that is not NA
NonNAindex <- which(!is.na(count.2$SAMPLE_ID))
firstNonNA <- min(NonNAindex)

count.2$BFISH[is.na(count.2$BFISH)] <- bfish_id
count.2$STRATA[is.na(count.2$STRATA)] <- count.2[firstNonNA, "STRATA"]
count.2$SAMPLE_MEAN_DEPTH_M[is.na(count.2$SAMPLE_MEAN_DEPTH_M)] <- count.2[firstNonNA, "SAMPLE_MEAN_DEPTH_M"]
count.2$PSU[is.na(count.2$PSU)] <- count.2[firstNonNA, "PSU"]
count.2$SAMPLE_ID[is.na(count.2$SAMPLE_ID)] <- count.2[firstNonNA, "SAMPLE_ID"]
count.2$freq[is.na(count.2$freq)] <- 0

#apply gcf = 1 for all spp that do not have established gcf
count.2$GCF[is.na(count.2$GCF)] <- 1

#apply gcf to freq to compute standard count
count.2$STD_COUNT <- count.2$freq/count.2$GCF

#transpose the catch data to wide table showing av.count per SPECIES_CD by Survey_ID and PSU and fill in 0 for all possible species that were not caught
catch.wide <- dcast(count.2, BFISH + PSU + STRATA + SAMPLE_MEAN_DEPTH_M + SAMPLE_ID ~ SPECIES_CD, value.var="STD_COUNT", fill=0)

#transpose the wide table back to long form
catch.long <- melt(catch.wide, id.vars = c("BFISH", "PSU", "STRATA", "SAMPLE_MEAN_DEPTH_M", "SAMPLE_ID"), variable.name = "SPECIES_CD", value.name="STD_COUNT")

#aggregate to mean std-count by spp and PSU
count.3 <- ddply(catch.long, .(BFISH, PSU, STRATA, SAMPLE_MEAN_DEPTH_M, SPECIES_CD), summarize, av.count = mean(STD_COUNT))

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
ar.data.fishing <- subset(cpue.len.rf, select = c(BFISH, STRATA, SAMPLE_MEAN_DEPTH_M, PSU, SPECIES_CD, LENGTH_CM, CPUE))
names(ar.data.fishing)[names(ar.data.fishing) == "SAMPLE_MEAN_DEPTH_M"] <- "SAMPLE_DEPTH_M"
ar.data.fishing$GEAR <- "Fishing"
ar.data.fishing$YEAR <- substr(ar.data.fishing$BFISH, 7,10)
ar.data.fishing <- merge((subset(domain.input, select = c("Island", "PSU"))), ar.data.fishing, by = "PSU")

write.csv(ar.data.fishing, file = "./inputs/",paste(bfish_id, "AR data Fishing.csv", sep ="_"), row.names = F)
