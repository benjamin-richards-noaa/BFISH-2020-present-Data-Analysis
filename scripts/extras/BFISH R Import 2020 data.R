#Research Fishing
CRF_SAMPLE <- dbGetQuery(con,"select * from BFISH.CRF_SAMPLE")
CRF_CATCH <- dbGetQuery(con,"select * from BFISH.CRF_CATCH") 

#MOUSS
CAM_SAMPLE <- dbGetQuery(con,"select * from BFISH.CAM_SAMPLE")
CAM_MAXN <- dbGetQuery(con,"select * from BFISH.CAM_MAXN")
CAM_LENGTHS <- dbGetQuery(con,"select * from BFISH.CAM_LENGTHS")

#########

CRF_SAMPLE <- read_excel("G:/Documents/Cruises/2020/BFISH_2020_F/Data/CRF/BFISH_2020_F CRF Data (corrected).xlsx", sheet = "CRF_Sample")
CRF_CATCH <- read_excel("G:/Documents/Cruises/2020/BFISH_2020_F/Data/CRF/BFISH_2020_F CRF Data (corrected).xlsx", sheet = "CRF_Catch")

CAM_SAMPLE <- read_excel("G:/Documents/Cruises/2020/BFISH_2020_F/Data/MOUSS/BFISH_2020_F MOUSS data (corrected).xlsx", sheet = "CAM_SAMPLE")
CAM_MAXN <- read_excel("G:/Documents/Cruises/2020/BFISH_2020_F/Data/MOUSS/BFISH_2020_F MOUSS data (corrected).xlsx", sheet = "MaxN")
CAM_LENGTHS <- read_excel("G:/Documents/Cruises/2020/BFISH_2020_F/Data/MOUSS/BFISH_2020_F MOUSS data (corrected).xlsx", sheet = "Lengths")

###############

unique(CRF_SAMPLE$BFISH)
unique(CRF_CATCH$BFISH)

unique(CAM_SAMPLE$BFISH)
unique(CAM_MAXN$BFISH)
unique(CAM_LENGTHS$BFISH)