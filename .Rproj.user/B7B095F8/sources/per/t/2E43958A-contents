###############################################################################################################
## BFISH data processing ####
## BFISH Survey Performance & Design Analysis##

## This script is for processing BFISH data from years 2020-present ##
#created by Benjamin L. Richards (benjamin.richards@noaa.gov) 
###############################################################################################################

library(ggplot2)
library(gcookbook)
library(plyr)
library(stats)
library(tidyverse)

# Set initial conditions --------------------------------------------------
setwd("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/WORKING")

# read in data tables for current and last 2 years
ar.data.all <- rbind(
                      read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/FINAL/BFISH_2020_F_AR data Both Gears Combined.csv"),
                      read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/FINAL/BFISH_2021_F_AR data Both Gears Combined.csv"),
                      read.csv("BFISH_2022_F_AR data Both Gears Combined.csv")
                      )

#create dataframe with spp and depth ranges
df.spp_domain <- data.frame(spp = c("PRFI", "ETCA", "ETCO"), common_name = c("Opakapaka", "Ehu", "Onaga"), min_depth = c(80, 110, 180), max_depth = c(260, 380, 360), stringsAsFactors = F)

#add in spp lhab
l.hab <- data.frame(SPECIES_CD = c("APRU", "ETCA", "ETCO", "HYQU", "PRFI", "PRSI", "PRZO"), lhab = c(5.0, 29.0, 27.5, 5.0, 22.5, 21.0, 5.0), stringsAsFactors = F)
ar.data.all <- merge(ar.data.all, l.hab, by = "SPECIES_CD")

#create abundance at length for all fishes larger than lhab
ar.data.all$anum <- ifelse(ar.data.all$LENGTH_CM == 0 | ar.data.all$LENGTH_CM >= ar.data.all$lhab,
                           ar.data.all$CPUE,
                           0)

#create abundance at length for unknown length fish (unum)
ar.data.all$unum <- ifelse(ar.data.all$LENGTH_CM == -9,
                           ar.data.all$CPUE,
                           0)

#initialize tables to store output
alloc.table <- c()
nstar.table <- c()

# Survey Performance and Allocation by SPP --------------------------------
for (i in 1:nrow(df.spp_domain)) {
  
  spp <- df.spp_domain$spp[i]
  common_name <- df.spp_domain$common_name[i]
  min_depth <- df.spp_domain$min_depth[i]
  max_depth <- df.spp_domain$max_depth[i]

  #sum abundance (CPUE) over lengths by sample
  d.1 <- ddply(ar.data.all, .(YEAR, BFISH, PSU, STRATA_2020, SAMPLE_DEPTH_M, SPECIES_CD, common_name), summarize,
               abund = sum(anum),
               uabund = sum(unum)
  )
  
  #remove records with unknown life stage (LENGTH_CM)
  d.1 <- subset(d.1, uabund == 0)

  #subset by depth for each species
  d.1 <- subset(d.1, SPECIES_CD == spp & SAMPLE_DEPTH_M >= min_depth & SAMPLE_DEPTH_M < max_depth)
  
  write.csv(d.1, file = paste(spp, "d.1.csv", sep =" "), row.names = F)

  #create mean and variance for abundance and biomass by YEAR, by STRATA_2020, by Species
  d.2 <- ddply(d.1, .(YEAR, SPECIES_CD, STRATA_2020), summarize,
               n = length(PSU),
               avCPUE = mean(abund),
               varCPUE = var(abund)
  )
  
  #replace missing variance with 0
  d.2$varCPUE[is.na(d.2$varCPUE)] <- 0
  
  #create PSU count by species and associated depth range
  domain.input.spp <- subset(domain.input, Depth_MEDIAN_m >= min_depth & Depth_MEDIAN_m < max_depth)

  psu.count.spp <- ddply(domain.input.spp, .(STRATA_2020), summarize,
                         PSU_COUNT = length(PSU))

  psu.count.spp$STRATUM_WEIGHT <- psu.count.spp$PSU_COUNT/(sum(psu.count.spp$PSU_COUNT))
  
  
  #bring in STRATA_2020 weighting factors
  d.3 <- merge(d.2, psu.count.spp, by = "STRATA_2020", all.x = T)
  
  #compute sampling fraction
  d.3$f <- d.3$n / d.3$PSU_COUNT
  
  #compute stratum variance of mean CPUE
  d.3$vbar_cpue <- ((1 - d.3$f) * d.3$varCPUE/d.3$n)
  
  #compute weighted stratum mean CPUE
  d.3$wcpue <- d.3$STRATUM_WEIGHT * d.3$avCPUE
  
  #compute weighted stratum variance of mean CPUE
  d.3$wvbar_cpue <- d.3$STRATUM_WEIGHT^2 * d.3$vbar_cpue
  
  #sum across species for domain computations
  d.4 <- ddply(d.3, .(YEAR, SPECIES_CD), summarize,
               n = sum(n),
               CPUE = sum(wcpue),
               vbar_CPUE = sum(wvbar_cpue)
  )
  
  #compute standard errors and CV
  d.4$SE_CPUE <- sqrt(d.4$vbar_CPUE)
  d.4$CV_CPUE <- (d.4$SE_CPUE / d.4$CPUE) * 100
  
  write.csv(d.4, file = paste(spp, "points for nstar_curve.csv", sep =" "), row.names = F)
  
#Survey Allocation and Plotting ------------------------------------------

  #create mean and variance for abundance and biomass by STRATA_2020, by Species for all years
  alloc.1 <- ddply(d.1, .(SPECIES_CD, STRATA_2020), summarize,
                   n = length(PSU),
                   avCPUE = mean(abund),
                   varCPUE = var(abund)
  )
  
  #bring in STRATA_2020 weighting factors
  alloc.2 <- merge(alloc.1, psu.count.spp, by = "STRATA_2020", all.x = T)
  
  alloc.2$s_var <- ifelse(alloc.2$varCPUE == 0,
                          0.001^2,
                          alloc.2$varCPUE)
  
  alloc.2$avCPUE <- ifelse(alloc.2$avCPUE == 0,
                          0.001,
                          alloc.2$avCPUE)

  alloc.2$STD <- sqrt(alloc.2$s_var)
  
  alloc.2$whSTD <- alloc.2$STRATUM_WEIGHT * alloc.2$STD
  
  alloc.2$whSVAR <- alloc.2$STRATUM_WEIGHT * alloc.2$s_var
  
  alloc.2$whCPUE <- alloc.2$STRATUM_WEIGHT * alloc.2$avCPUE
  
  #compute allocation table
  alloc.table <- rbind(alloc.table, subset(alloc.2, select = c("SPECIES_CD", "STRATA_2020","STRATUM_WEIGHT", "whSTD")))

  # Domain Estimates
  alloc.3 <- ddply(alloc.2, .(SPECIES_CD), summarize,
                   psuTotal = sum(PSU_COUNT),
                   avAbund = sum(whCPUE),
                   vbar_Abund = sum(whSVAR),
                   STD_abund = sum(whSTD))
  
  alloc.3$vc1 <- alloc.3$STD_abund ^ 2
  
  
  alloc.4 <- subset(alloc.3, SPECIES_CD = spp)
  
  # Computation & Plotting of n* curve by SPP
  y <- seq(1,100,0.1)
  mod <- alloc.4$vc1/((((y/100)*alloc.4$avAbund)^2)+alloc.4$vbar_Abund/alloc.4$psuTotal)
  nstar_curve <- as.data.frame(cbind(mod,y))
  write.csv(nstar_curve, file = paste(spp, "nstar_curve.csv", sep =" "), row.names = F)
  
  # nstars Required for 5% CV, 10% CV, 15% CV and 20% CV Survey Precision #
  nstar.1 <- subset(alloc.3, SPECIES_CD %in% c("PRFI", "ETCA", "ETCO"))

  nstar.1$V_20 <- (0.20 * nstar.1$avAbund) ^ 2
  nstar.1$V_15 <- (0.15 * nstar.1$avAbund) ^ 2
  nstar.1$V_10 <- (0.10 * nstar.1$avAbund) ^ 2

  nstar.1$nstar_20 <- nstar.1$vc1 / (nstar.1$V_20 +(nstar.1$vbar_Abund / nstar.1$psuTotal))
  nstar.1$nstar_15 <- nstar.1$vc1 / (nstar.1$V_15 +(nstar.1$vbar_Abund / nstar.1$psuTotal))
  nstar.1$nstar_10 <- nstar.1$vc1 / (nstar.1$V_10 +(nstar.1$vbar_Abund / nstar.1$psuTotal))

  nstar.table <- rbind(nstar.table, subset(nstar.1, select = c("SPECIES_CD", "nstar_20", "nstar_15","nstar_10")))

  #plot survey performance by spp
  ggplot(data = nstar_curve, aes(x = mod, y = y)) +
    theme_bw() +
    geom_line(size = 1, color = "#F26739") +
    geom_hline(yintercept = 15, linetype = "dashed") +
    geom_point(data = subset(d.4, SPECIES_CD == spp & YEAR == "2020"), aes(x = n, y = CV_CPUE), size = 3) +
    geom_text(data = subset(d.4, SPECIES_CD == spp & YEAR == "2020"), aes(x = n+60, y = CV_CPUE, label = YEAR), size = 8) +
    geom_point(data = subset(d.4, SPECIES_CD == spp & YEAR == "2021"), aes(x = n, y = CV_CPUE), size = 3) +
    geom_text(data = subset(d.4, SPECIES_CD == spp & YEAR == "2021"), aes(x = n+60, y = CV_CPUE, label = YEAR), size = 8) +
    geom_point(data = subset(d.4, SPECIES_CD == spp & YEAR == "2022"), aes(x = n, y = CV_CPUE), size = 3) +
    geom_text(data = subset(d.4, SPECIES_CD == spp & YEAR == "2022"), aes(x = n+60, y = CV_CPUE, label = YEAR), size = 8) +
    xlim(0,800) +
    xlim(0,800) +
    ylim(0,50) +
    labs(title = paste0("BFISH Survey Performance (", common_name,")"), x = "Sample Size", y = "CV(CPUE)") +
    theme(plot.title = element_text(hjust = 0.5, size = 24)) +
    theme(axis.text = element_text(size = 24),
          axis.title = element_text(size = 18,
                                    face = "bold"),
          axis.title.y = element_text(margin = margin(r=7.5)))

# Plot survey performance by spp by year ----------------------------------

  
  ggsave(paste0("BFISH Survey Performance (",common_name,").pdf"), width = 11, height = 8.5, units = "in")
  ggsave(paste0("BFISH Survey Performance (",common_name, ").png"), width = 11, height = 8.5, units = "in")
}

write.csv(alloc.table, file = "BFISH pre-allocation table.csv", row.names = F)
write.csv(nstar.table, file = "BFISH nstar_table.csv", row.names = F)