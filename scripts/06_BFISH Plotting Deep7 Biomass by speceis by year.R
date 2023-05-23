###############################################################################################################
## BFISH data processing ####
## BFISH Survey Design Analysis ##

## This script is for processing BFISH data from years 2020-present ##

#this script creates standard plots of species-level metrics and survey performance per year

#created by Benjamin L. Richards (benjaminrichards@noaa.gov) 
###############################################################################################################
setwd("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/WORKING")

#read in biomass by species for all survey years
BFISH_2016 <- read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2016-2019 Data Analysis/FINAL/BFISH_2016_F Deep7 Biomass by Species.csv")
BFISH_2017 <- read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2016-2019 Data Analysis/FINAL/BFISH_2017_F Deep7 Biomass by Species.csv")
BFISH_2018 <- read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2016-2019 Data Analysis/FINAL/BFISH_2018_F Deep7 Biomass by Species.csv")
BFISH_2019 <- read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2016-2019 Data Analysis/FINAL/BFISH_2019_F Deep7 Biomass by Species.csv")
BFISH_2020 <- read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/FINAL/BFISH_2020_F Deep7 Biomass by Species.csv")
BFISH_2021 <- read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/FINAL/BFISH_2021_F Deep7 Biomass by Species.csv")
BFISH_2022 <- read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/WORKING/BFISH_2022_F Deep7 Biomass by Species.csv")

fast.rbind <- function(...,method=c("fill","common"),value=NA){
  if("fill"==method[1]) {
    fun1 <- function(x,y,value=NA){
      x[setdiff(colnames(y),colnames(x))] <- value
      
      y[setdiff(colnames(x),colnames(y))] <- value
      
      return(rbind(x,y))
    }
  }
  
  if("common"==method[1]) {
    fun1 <- function(x,y,value=NULL){
      common_cols <- intersect(colnames(x), colnames(y))
      return(rbind(x[, common_cols,drop=F],y[, common_cols,drop=F]))
    }
  }
  return(Reduce(function(x,y){fun1(x=x,y=y,value=value)},list(...)))
}

bm <- fast.rbind(BFISH_2016,
           BFISH_2017,
           BFISH_2018,
           BFISH_2019,
           BFISH_2020,
           BFISH_2021,
           BFISH_2022,
           method="common")
  
sp <- unique(subset(SPECIES_TABLE, subset = SPECIES_CD %in% d7, select = c("SPECIES_CD","SCIENTIFIC_NAME","COMMON_NAME")))

BFISH_biomass <- merge(bm, sp, "SPECIES_CD")


# Plot Relative Biomass by Species & Year ---------------------------------
# 
formatter <- function(...){
  function(x) format(round(x, 2), ...)
}

ggplot(data = BFISH_biomass, aes(x = as.factor(YEAR), y = avBiomass_kg, group = 1, color = COMMON_NAME)) +
  facet_grid(COMMON_NAME ~ ., scales = "free") +
  scale_y_continuous(labels = formatter(nsmall = 2)) +
  theme_bw() +
  geom_line(color = "grey33") +
  geom_errorbar(aes(ymin = avBiomass_kg - SE_avBiomass_kg, ymax = avBiomass_kg + SE_avBiomass_kg), width = 0.05, color = "grey33") +
  geom_point(size = 3) +
  scale_color_manual(values = c("Ehu" = "#F26739",
                                "Gindai" = "#EF3E24",
                                "Hapuupuu" = "#680B10",
                                "Kalekale" = "#A91E23",
                                "Lehi" = "#D12027",
                                "Onaga" = "#F15B26",
                                "Opakapaka" = "#F47B50"),
                     guide = FALSE) +
  labs(x = "Survey Year", y = "Average Biomass (kg) ±SE)") +
  theme(axis.text = element_text(size = 9),
        axis.title = element_text(size=12,
                                  face = "bold"),
        axis.title.y = element_text(margin = margin(r=7.5)))

ggsave("BFISH Deep 7 Relative Abundance by Species by Year.pdf", width = 8.5, height = 11, units = "in")
ggsave("BFISH Deep 7 Relative Abundance by Species by Year.png", width = 8.5, height = 11, units = "in")


# Compute & Plot biomass anomalies (annual change from running average) ------------------------------

cg.1 <- subset(BFISH_biomass, select = c("COMMON_NAME", "SPECIES_CD", "YEAR", "avBiomass_kg", "SE_avBiomass_kg"))

cg.2 <- ddply(cg.1, .(SPECIES_CD), summarize,
              mean_Biomass_kg = mean(avBiomass_kg))

cg.3 <- merge(cg.1, cg.2,"SPECIES_CD")

cg.3$anom <- cg.3$avBiomass_kg - cg.3$mean_Biomass_kg

ggplot(data = cg.3, aes(x = as.factor(YEAR), y = anom, group = 1, color = COMMON_NAME)) +
  geom_hline(linetype = "longdash", yintercept = 0, size = 0.5, color = "black") +
  facet_grid(COMMON_NAME ~ ., scales = "free") +
  scale_y_continuous(labels = formatter(nsmall = 2)) +
  theme_bw() +
  geom_line(color = "grey33") +
  geom_errorbar(aes(ymin = cg.3$anom - cg.3$SE_avBiomass_kg, ymax = cg.3$anom + cg.3$SE_avBiomass_kg), width = 0.1, color = "grey33") +
  geom_point(size = 3) +
  scale_color_manual(values = c("Ehu" = "#F26739",
                                "Gindai" = "#EF3E24",
                                "Hapuupuu" = "#680B10",
                                "Kalekale" = "#A91E23",
                                "Lehi" = "#D12027",
                                "Onaga" = "#F15B26",
                                "Opakapaka" = "#F47B50"),
                     guide = FALSE) +
  labs(x = "Survey Year", y = "Relative Biomass: Deviation from Running Mean (kg/unit area)") +
  theme(axis.text = element_text(size = 9),
        axis.title = element_text(size=12,
                                  face = "bold"),
        axis.title.y = element_text(margin = margin(r=5)))

ggsave("BFISH Deep 7 Biomass Anomalies by Species by Year.pdf", width = 8.5, height = 11, units = "in")
ggsave("BFISH Deep 7 Biomass Anomalies by Species by Year.png", width = 8.5, height = 11, units = "in")


# Compute & Plot Standard-Normal Stock Biomass ------------------------------

cg.4 <- subset(BFISH_biomass, select = c("COMMON_NAME", "SPECIES_CD", "YEAR", "Biomass_kg", "SE_Biomass_kg"))

cg.5 <- ddply(cg.4, .(SPECIES_CD), summarize,
              mean_Biomass_kg = mean(Biomass_kg),
              SD_Biomass_kg = sd(Biomass_kg))

cg.6 <- merge(cg.4, cg.5,"SPECIES_CD")

cg.6$z <- (cg.6$Biomass_kg - cg.6$mean_Biomass_kg)/cg.6$SD_Biomass_kg

ggplot(data = cg.6, aes(x = as.factor(YEAR), y = z, group = 1, color = COMMON_NAME)) +
  geom_hline(linetype = "longdash", yintercept = 0, size = 0.5, color = "black") +
  facet_grid(COMMON_NAME ~ ., scales = "free") +
  theme_bw() +
  geom_line(color = "grey33") +
  geom_point(size = 3) +
  ylim(-3,3) +
  scale_color_manual(values = c("Ehu" = "#F26739",
                                "Gindai" = "#EF3E24",
                                "Hapuupuu" = "#680B10",
                                "Kalekale" = "#A91E23",
                                "Lehi" = "#D12027",
                                "Onaga" = "#F15B26",
                                "Opakapaka" = "#F47B50"),
                     guide = "none") +
  labs(x = "BFISH Survey Year", y = "Standard-Normal Distribution of Stock Biomass") +
  theme(axis.text = element_text(size = 9, angle = 90),
        axis.title = element_text(size=12,
                                  face = "bold"),
        axis.title.y = element_text(margin = margin(r=5)))

ggsave("BFISH Deep 7 Standard-Normal Biomass by Speces by Year.pdf", width = 8.5, height = 11, units = "in")
ggsave("BFISH Deep 7 Standard-Normal Biomass by Species by Year.png", width = 8.5, height = 11, units = "in")
ggsave("BFISH Deep 7 Standard-Normal Biomass by Species by Year (1-pager).png", width = 2.75, height =7, units = "in", bg = "transparent")

# Compute & Plot Survey CV by Species & Year ------------------------------

#getting list of data files(regex and location may have to be changed based on naming convention and data access)
cv.total<-as.list(dir(pattern = ".*Deep7 Biomass Total.*", path = "~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/WORKING/", full.names = T))
cv.total <- cv.total %>% 
  # read in all of the csv's in the files list
  purrr::map_dfr(read_csv)


ggplot(data = cv.total, aes(x = as.factor(YEAR), y = CV_Biomass_kg, group = 1,)) +
  geom_hline(linetype = "longdash", yintercept = 0, size = 0.5, color = "black") +
  theme_bw() +
  geom_line(color = "grey33") +
  geom_point(size = 3, color = "#F26739") +
  ylim(10,25) +
  labs(x = "BFISH Survey Year", y = "Deep 7 Complex CV of Biomass (kg)") +
  theme(axis.text = element_text(size = 9, angle = 90),
        axis.title = element_text(size=12,
                                  face = "bold"),
        axis.title.y = element_text(margin = margin(r=5)))

ggsave("BFISH Deep 7 CV by Year.pdf", width = 11, height = 8.5, units = "in")
ggsave("BFISH Deep 7 CV by Year.png", width = 11, height = 8.5, units = "in")

# Plotting for 1-pager ----------------------------------------------------

ggplot(data = cg.3, aes(x = as.factor(YEAR), y = anom, group = 1, color = COMMON_NAME)) +
  geom_hline(linetype = "longdash", yintercept = 0, size = 0.5, color = "black") +
  facet_grid(COMMON_NAME ~ ., scales = "free") +
  scale_y_continuous(labels = formatter(nsmall = 2)) +
  theme_bw() +
  geom_line(size = 0.5, color = "grey33") +
  geom_errorbar(aes(ymin = cg.3$anom - cg.3$SE_avBiomass_kg, ymax = cg.3$anom + cg.3$SE_avBiomass_kg), size = 0.4, width = 0.2, color = "grey33") +
  geom_point(size = 1) +
  scale_color_manual(values = c("Ehu" = "#F26739",
                                "Gindai" = "#EF3E24",
                                "Hapuupuu" = "#680B10",
                                "Kalekale" = "#A91E23",
                                "Lehi" = "#D12027",
                                "Onaga" = "#F15B26",
                                "Opakapaka" = "#F47B50"),
                     guide = FALSE) +
  labs(x = "Survey Year", y = "Relative Biomass Anomolies: Deviation from Running Mean (kg/unit area)") +
  theme(axis.text = element_text(size = 9, angle = 90),
        axis.title = element_text(size=10),
        axis.title.y = element_text(margin = margin(r=5)),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA)
        )

ggsave("BFISH Deep 7 Biomass Anomalies by Species by Year (1-pager).png", width = 2.75, height =7, units = "in", bg = "transparent")

# Compute & Plot Species Abundance at Length ------------------------------
ar.data.all <- rbind(
  read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/FINAL/BFISH_2020_F_AR data Both Gears Combined.csv"),
  read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/FINAL/BFISH_2021_F_AR data Both Gears Combined.csv"),
  read.csv("BFISH_2022_F_AR data Both Gears Combined.csv")
)

#remove missing length data
ar.data.all.2 <- subset(ar.data.all, LENGTH_CM >=0)

## Stratum length-frequency computations
ar.data.all.lenf <- ddply(ar.data.all.2, .(SPECIES_CD, STRATA_2020, YEAR, LENGTH_CM), summarize,
                          sum_len = sum(CPUE))

totnum <- ddply(ar.data.all.lenf, .(SPECIES_CD, STRATA_2020, YEAR), summarize,
                totnum = sum(sum_len))


ar.data.all.lenf<-merge(ar.data.all.lenf,totnum, by = c("SPECIES_CD", "STRATA_2020", "YEAR"))

ar.data.all.lenf$rfrq<-with(ar.data.all.lenf, ifelse(totnum==0,0,sum_len/totnum))

ar.data.all.tot <- ddply(ar.data.all, .(YEAR, PSU, STRATA_2020, SPECIES_CD), summarize,
                         num = sum(CPUE))

#### Stratified Random Sampling Survey (StRS) Design Computations  ####
# Calculate mean Density (avdns), Variance of Density (svar), Sample Size (n) and Standard Deviation of Density (std)
ar.data.all.est <- ddply(ar.data.all.tot, .(YEAR, STRATA_2020, SPECIES_CD), summarize, avdns = mean(num))

# Merge ntot with ar.data.all.est 
ar.data.all.est<-merge(ar.data.all.est,psu.count.2020, by="STRATA_2020")

# Stratum-specific Estimates
ar.data.all.est$yt<-with(ar.data.all.est,PSU_COUNT*avdns)

ar.data.all.lenf.2<-merge(ar.data.all.est,ar.data.all.lenf, by= c("SPECIES_CD", "STRATA_2020", "YEAR"))

ar.data.all.lenf.2$ytlen<-ar.data.all.lenf.2$yt*ar.data.all.lenf.2$rfrq

ar.data.all.ytotlen <- ddply(ar.data.all.lenf.2, .(SPECIES_CD, YEAR, LENGTH_CM), summarize, ytotlen = sum(ytlen))

###############################################

# gcookbook routine to create barplot
ar.data.all.lfplot<-arrange(ar.data.all.ytotlen,YEAR,SPECIES_CD,LENGTH_CM,ytotlen)

############################## PLOT PARAMETERS #################################

# Set the species and years we want plotted
spec_to_plot  <- c("ETCA", "PRFI", "ETCO")
yrs_to_plot   <- unique(ar.data.all.lfplot$YEAR)

#set up binning
bin.width <- 5
nbins     <- ceiling(max(ar.data.all.lfplot$LENGTH_CM) / bin.width)
nobs      <- length(ar.data.all.lfplot$LENGTH_CM)
len.cat <- matrix(NA, nrow = nbins, ncol = 1)
mid.len <- matrix(NA, nrow = nbins, ncol = 1)
len.cat[1] <- 0
count <- matrix(NA, nrow = nbins, ncol = 1)
tot <- 0

# subset the data
lfplot <- subset(ar.data.all.lfplot, 
                 SPECIES_CD %in% spec_to_plot & 
                   YEAR %in% yrs_to_plot)

# create the bins and mid.len
for (i in 1:nbins)
{
  len.cat[i+1] <- len.cat[i]+bin.width
  mid.len[i]<-(len.cat[i+1]+len.cat[i])/2
}


# Let's make a holder for abundance data by yr_species_combo
abund_by_yr_spec <- c()

# Loop through species and years to get the counts per yr-species combo
for(spec in spec_to_plot){
  for(yr in yrs_to_plot){
    
    yr_spec_data <- subset(lfplot, YEAR == yr & SPECIES_CD == spec)
    
    for (i in 1:nbins)
    {
      tot <- 0
      for (j in 1:nrow(yr_spec_data))
      {
        ifelse((yr_spec_data$LENGTH_CM[j]) >= len.cat[i] & (yr_spec_data$LENGTH_CM[j]) < len.cat[i+1],
               tot <- tot + yr_spec_data$ytotlen[j],
               tot<-tot)
      }
      count[i] <- tot
    }
    
    ysum <- sum(count)
    count_freq <- count/ysum
    
    if(is.null(abund_by_yr_spec)){
      abund_by_yr_spec <- tibble(YEAR       = yr, 
                                 SPECIES_CD = spec,
                                 mid.len    = as.numeric(mid.len),
                                 count_freq = as.numeric(count_freq))
    } else {
      abund_by_yr_spec <- rbind(abund_by_yr_spec,
                                tibble(YEAR       = yr, 
                                       SPECIES_CD = spec,
                                       mid.len    = as.numeric(mid.len),
                                       count_freq = as.numeric(count_freq)))
    }
  }
}

# merge in the common names to use for plotting
abund_by_yr_spec <- merge(abund_by_yr_spec, 
                          SPECIES_TABLE[, c("COMMON_NAME", "SPECIES_CD")])

# Generate the abundance densities
ggplot(abund_by_yr_spec, aes(x=mid.len, y=count_freq, fill = COMMON_NAME)) + 
  facet_grid(YEAR ~ COMMON_NAME) +
  theme(strip.text.x = element_text(size=14),
        strip.text.y = element_text(size=14)) +
  geom_bar(stat="identity", colour="black", position="dodge") +
  scale_fill_manual(values = c("Ehu" = "#F26739",
                               "Onaga" = "#F15B26",
                               "Opakapaka" = "#F47B50"),
                    guide = FALSE) +
  
  theme(axis.text.x = element_text(angle=0, hjust=1, vjust=1.5)) +
  labs(x = "Fork Length (cm)", y = "Abundance", family="Helvetica",fontface="bold",size=16) +
  theme(axis.line = element_line(colour="black",size=1.0))+
  scale_x_continuous(limits = range(len.cat), 
                     breaks = seq(min(len.cat),max(len.cat), bin.width * 2))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) 

# save the outputs to the working directory
ggsave(paste("Deep 3 Length Frequency.pdf"), width = 11, height = 8.5, units = "in")
ggsave(paste("Deep 3 Length Frequency.png"), width = 11, height = 8.5, units = "in")
