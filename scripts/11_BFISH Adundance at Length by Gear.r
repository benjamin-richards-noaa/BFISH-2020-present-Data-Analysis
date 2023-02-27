# Compute & Plot Species Abundance at Length by gear ------------------------------
ar.data.gear <- rbind(
  read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/FINAL/BFISH_2016_F_AR data Camera.csv"),
  read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/FINAL/BFISH_2016_F_AR data Fishing.csv"),
  read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/FINAL/BFISH_2017_F_AR data Camera.csv"),
  read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/FINAL/BFISH_2017_F_AR data Fishing.csv"),
  read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/FINAL/BFISH_2018_F_AR data Camera.csv"),
  read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/FINAL/BFISH_2018_F_AR data Fishing.csv"),
  read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/FINAL/BFISH_2019_F_AR data Camera.csv"),
  read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/FINAL/BFISH_2019_F_AR data Fishing.csv"),
  read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/FINAL/BFISH_2020_F_AR data Camera.csv"),
  read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/FINAL/BFISH_2020_F_AR data Fishing.csv"),
  read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/FINAL/BFISH_2021_F_AR data Camera.csv"),
  read.csv("~/Bottomfish/Bottomfish Project/BFISH/BFISH Data/Data Analysis/[R]/BFISH 2020-present Data Analysis/FINAL/BFISH_2021_F_AR data Fishing.csv"),
  read.csv("BFISH_2022_F_AR data Camera.csv"),
  read.csv("BFISH_2022_F_AR data Fishing.csv")
)

#remove missing length data
ar.data.gear.2 <- subset(ar.data.gear, LENGTH_CM >=0)

## Stratum length-frequency computations
ar.data.gear.lenf <- ddply(ar.data.gear.2, .(GEAR, STRATA_2020, SPECIES_CD, LENGTH_CM), summarize,
                          sum_len = sum(CPUE))

totnum <- ddply(ar.data.gear.lenf, .(GEAR, STRATA_2020, SPECIES_CD), summarize,
                totnum = sum(sum_len))


ar.data.gear.lenf<-merge(ar.data.gear.lenf,totnum, by = c("GEAR", "STRATA_2020", "SPECIES_CD"))

ar.data.gear.lenf$rfrq<-with(ar.data.gear.lenf, ifelse(totnum==0,0,sum_len/totnum))

ar.data.gear.tot <- ddply(ar.data.gear, .(SPECIES_CD, PSU, STRATA_2020, GEAR), summarize,
                         num = sum(CPUE))

#### Stratified Random Sampling Survey (StRS) Design Computations  ####
# Calculate mean Density (avdns), Variance of Density (svar), Sample Size (n) and Standard Deviation of Density (std)
ar.data.gear.est <- ddply(ar.data.gear.tot, .(SPECIES_CD, STRATA_2020, GEAR), summarize, avdns = mean(num))

# Merge ntot with ar.data.gear.est 
ar.data.gear.est<-merge(ar.data.gear.est,psu.count.2020, by="STRATA_2020")

# Stratum-specific Estimates
ar.data.gear.est$yt<-with(ar.data.gear.est,PSU_COUNT*avdns)

ar.data.gear.lenf.2<-merge(ar.data.gear.est,ar.data.gear.lenf, by= c("GEAR", "STRATA_2020", "SPECIES_CD"))

ar.data.gear.lenf.2$ytlen<-ar.data.gear.lenf.2$yt*ar.data.gear.lenf.2$rfrq

ar.data.gear.ytotlen <- ddply(ar.data.gear.lenf.2, .(GEAR, SPECIES_CD, LENGTH_CM), summarize, ytotlen = sum(ytlen))

###############################################

# gcookbook routine to create barplot
ar.data.gear.lfplot<-arrange(ar.data.gear.ytotlen,SPECIES_CD,GEAR,LENGTH_CM,ytotlen)

############################## PLOT PARAMETERS #################################

# Set the species and SPECIES_CDs we want plotted
spec_to_plot  <- c("Camera", "Fishing")
yrs_to_plot   <- unique(ar.data.gear.lfplot$SPECIES_CD)

#set up binning
bin.width <- 5
nbins     <- ceiling(max(ar.data.gear.lfplot$LENGTH_CM) / bin.width)
nobs      <- length(ar.data.gear.lfplot$LENGTH_CM)
len.cat <- matrix(NA, nrow = nbins, ncol = 1)
mid.len <- matrix(NA, nrow = nbins, ncol = 1)
len.cat[1] <- 0
count <- matrix(NA, nrow = nbins, ncol = 1)
tot <- 0

# subset the data
lfplot <- subset(ar.data.gear.lfplot, 
                 GEAR %in% spec_to_plot & 
                   SPECIES_CD %in% yrs_to_plot)

# create the bins and mid.len
for (i in 1:nbins)
{
  len.cat[i+1] <- len.cat[i]+bin.width
  mid.len[i]<-(len.cat[i+1]+len.cat[i])/2
}


# Let's make a holder for abundance data by yr_species_combo
abund_by_yr_spec <- c()

# Loop through species and SPECIES_CDs to get the counts per yr-species combo
for(spec in spec_to_plot){
  for(yr in yrs_to_plot){
    
    yr_spec_data <- subset(lfplot, SPECIES_CD == yr & GEAR == spec)
    
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
    ysum <- sum(count)
    count_freq <- count/ysum
    
    if(is.null(abund_by_yr_spec)){
      abund_by_yr_spec <- tibble(SPECIES_CD       = yr, 
                                 GEAR = spec,
                                 mid.len    = as.numeric(mid.len),
                                 count_freq = as.numeric(count_freq))
    } else {
      abund_by_yr_spec <- rbind(abund_by_yr_spec,
                                tibble(SPECIES_CD       = yr, 
                                       GEAR = spec,
                                       mid.len    = as.numeric(mid.len),
                                       count_freq = as.numeric(count_freq)))
    }
  }
}

# merge in the common names to use for plotting
abund_by_yr_spec <- merge(abund_by_yr_spec, 
                          SPECIES_TABLE[, c("COMMON_NAME", "SPECIES_CD")])

# Generate the abundance densities
ggplot(abund_by_yr_spec, aes(x=mid.len, y=count_freq, fill = GEAR)) + 
  facet_grid(COMMON_NAME ~ GEAR) +
  theme(strip.text.x = element_text(size=14),
        strip.text.y = element_text(size=14)) +
  geom_bar(stat="identity", colour="black", position="dodge") +
  scale_fill_manual(values = c("Camera" = "#F26739",
                               "Fishing" = "#F15B26"),
                    guide = FALSE) +
  
  theme(axis.text.x = element_text(angle=0, hjust=1, vjust=1.5)) +
  labs(x = "Fork Length (cm)", y = "Abundance", family="Helvetica",fontface="bold",size=16) +
  theme(axis.line = element_line(colour="black",size=1.0))+
  scale_x_continuous(limits = range(len.cat), 
                     breaks = seq(min(len.cat),max(len.cat), bin.width * 2))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold")) 

# save the outputs to the working directory
ggsave(paste("Gear Length Frequency by Species.pdf"), width = 11, height = 8.5, units = "in")
