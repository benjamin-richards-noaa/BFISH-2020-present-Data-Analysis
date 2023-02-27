###############################################################################################################
## BFISH data processing ####
## biomass calculation and survey performance script ##

## This script is for processing BFISH data from years 2020-present ##


#created by Benjamin L. Richards (benjaminrichards@noaa.gov) 


###############################################################################################################


###################
##### settings ####
###################

#set yearly BFISH_ID
bfish_id = "BFISH_2020_F" #"BFISH_2016_F" "BFISH_2017_F" "BFISH_2018_F" "BFISH_2019_F" "BFISH_2020_F"


if(!exists("scaler")){source("G:/Documents/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/GitHub/BFISH/1_BFISH Set General Conditions Init.r")}
if(!exists("lc")){source("G:/Documents/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/GitHub/BFISH/1_BFISH Set General Conditions Init.r")}


##########################################################################
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
b.1$pnum <- ifelse(b.1$LENGTH_C >= 0 | b.1$LENGTH_C < lc,
                   b.1$CPUE,
                   0)

#create abundance at length for unknown length fish (unum)
b.1$unum <- ifelse(b.1$LENGTH_CM == -9,
                   b.1$CPUE,
                   0)

#create exploited biomass in kilogramS
b.1$ebiom_kg <-ifelse(b.1$LENGTH_CM >= 0,
                b.1$CPUE * (b.1$A * (b.1$LENGTH_CM^b.1$B)),
                 0)

#sum enum, unum, and ebiom_kg by spp by YEAR, BFISH, and PSU
b.2 <- ddply(b.1, .(YEAR, BFISH, PSU, STRATA, SPECIES_CD), summarize,
             biom = sum(ebiom_kg),
             abund = sum(enum),
             uabund = sum(unum))

#remove records with unknown life stage (LENGTH_CM)
b.3 <- subset(b.2, uabund == 0)

#write out file with relative abundance/biomass data by species by PSU
b.3.wide <- dcast(b.3, YEAR + BFISH + PSU ~ SPECIES_CD, fun.aggregate = sum, value.var="biom", fill=0)
write.csv(b.3.wide, file = paste(bfish_id, "Deep7 Biomass by PSU & Species.csv", sep =" "), row.names = F)

#create stratum mean and variance for relative abundance and biomass by YEAR, by STRATA, by Species
d7.biomass.spp.strata <- ddply(b.3, .(YEAR, BFISH, SPECIES_CD, STRATA), summarize,
             n = length(PSU),
             avCPUE = mean(abund),
             avBiom = mean(biom),
             varCPUE = var(abund),
             varBiom = var(biom))

#assign year to data object
assign(paste(bfish_id, "d7.biomass.spp.strata", sep = "."),d7.biomass.spp.strata)

#write out file with relative abundance/biomass data by species by STRATA
d7.biomass.spp.strata.wide <- dcast(d7.biomass.spp.strata, YEAR + BFISH + STRATA ~ SPECIES_CD, fun.aggregate = sum, value.var="avBiom", fill=0)
write.csv(d7.biomass.spp.strata.wide, file = paste(bfish_id, "Deep7 Relative Biomass by Strata & Species.csv", sep =" "), row.names = F)

#bring in STRATA weighting factors
b.5 <- merge(d7.biomass.spp.strata, psu.count, by = "STRATA", all.x = T)

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

#assign year to data object
assign(paste(bfish_id, "d7.biomass.spp", sep = "."),d7.biomass.spp)

#write out file with abundance/biomass data by species
write.csv(d7.biomass.spp, file = paste(bfish_id, "Deep7 Biomass by Species.csv", sep =" "), row.names = F)


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

