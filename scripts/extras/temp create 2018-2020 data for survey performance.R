ar.data.2018 <- read.csv("BFISH_2018_F_AR data Both Gears Combined.csv")
ar.data.2019 <- read.csv("BFISH_2019_F_AR data Both Gears Combined.csv")
ar.data.2020 <- read.csv("BFISH_2020_F_AR data Both Gears Combined.csv")

ar.data.20182019 <- rbind(ar.data.2018, ar.data.2019)

ar.1 <- merge(x = ar.data.20182019, y = domain.init[, c("PSU", "acrstrat", "hbstrat")], by = "PSU", all.x = TRUE)

# Recompute depthstrat ----------------------------------------------------
#recompute depthstrat based on SAMPLE_MEAN_DEPTH (mean of mean drift depths for each sample)
ar.1$depthstrat <- ifelse(ar.1$SAMPLE_DEPTH_M < 70,
                                    "DZ",
                                    ifelse(ar.1$SAMPLE_DEPTH_M >= 70 & ar.1$SAMPLE_DEPTH_M < 110,
                                           "D1",
                                           ifelse(ar.1$SAMPLE_DEPTH_M >= 110 & ar.1$SAMPLE_DEPTH_M < 170,
                                                  "D2",
                                                  ifelse(ar.1$SAMPLE_DEPTH_M >= 170 & ar.1$SAMPLE_DEPTH_M < 220,
                                                         "D3",
                                                         ifelse(ar.1$SAMPLE_DEPTH_M >= 220 & ar.1$SAMPLE_DEPTH_M < 330,
                                                                "D4",
                                                                ifelse(ar.1$SAMPLE_DEPTH_M >= 330 & ar.1$SAMPLE_DEPTH_M <= 400,
                                                                       "D5",
                                                                       ifelse(ar.1$SAMPLE_DEPTH_M > 400,
                                                                              "DZ",
                                                                              "DZ")))))))





# STRATA_2020 computation --------------------------------------------------------------------


#build STRATA_2020 catagory for each sample
ar.1$STRATA_2020 <- ifelse(ar.1$depthstrat == "D1" &
                                       (ar.1$acrstrat == "MA1"| ar.1$acrstrat == "MA2") &
                                       (ar.1$hbstrat == "HB1" | ar.1$hbstrat == "HB2" | ar.1$hbstrat == "HB3"), 
                                     "S01",
                                     ifelse(ar.1$depthstrat == "D1"
                                            & ar.1$acrstrat == "MA3" &
                                              ar.1$hbstrat == "HB1",
                                            "S02",
                                            ifelse(ar.1$depthstrat == "D1" &
                                                     ar.1$acrstrat == "MA3" &
                                                     ar.1$hbstrat == "HB2",
                                                   "S03",
                                                   ifelse(ar.1$depthstrat == "D1" &
                                                            ar.1$acrstrat == "MA3" &
                                                            ar.1$hbstrat == "HB3",
                                                          "S04",
                                                          ifelse(ar.1$depthstrat == "D2" &
                                                                   ar.1$acrstrat == "MA1" &
                                                                   (ar.1$hbstrat == "HB1"|ar.1$hbstrat == "HB2"),
                                                                 "S05",
                                                                 ifelse(ar.1$depthstrat == "D2" &
                                                                          ar.1$acrstrat == "MA1" &
                                                                          ar.1$hbstrat == "HB3",
                                                                        "S06",
                                                                        ifelse(ar.1$depthstrat == "D2" &
                                                                                 ar.1$acrstrat == "MA2" &
                                                                                 ar.1$hbstrat == "HB1",
                                                                               "S07",
                                                                               ifelse(ar.1$depthstrat == "D2" &
                                                                                        ar.1$acrstrat == "MA2" &
                                                                                        ar.1$hbstrat == "HB2",
                                                                                      "S08",
                                                                                      ifelse(ar.1$depthstrat == "D2" &
                                                                                               ar.1$acrstrat == "MA2" &
                                                                                               ar.1$hbstrat == "HB3",
                                                                                             "S09",
                                                                                             ifelse(ar.1$depthstrat == "D2" &
                                                                                                      ar.1$acrstrat == "MA3" &
                                                                                                      ar.1$hbstrat == "HB1",
                                                                                                    "S10",
                                                                                                    ifelse(ar.1$depthstrat == "D2" &
                                                                                                             ar.1$acrstrat == "MA3" &
                                                                                                             ar.1$hbstrat == "HB2",
                                                                                                           "S11",
                                                                                                           ifelse(ar.1$depthstrat == "D2" &
                                                                                                                    ar.1$acrstrat == "MA3" &
                                                                                                                    ar.1$hbstrat == "HB3",
                                                                                                                  "S12",
                                                                                                                  ifelse(ar.1$depthstrat == "D3" &
                                                                                                                           (ar.1$acrstrat == "MA1"| ar.1$acrstrat == "MA2") &
                                                                                                                           (ar.1$hbstrat == "HB1" | ar.1$hbstrat == "HB2" | ar.1$hbstrat == "HB3"), 
                                                                                                                         "S13",
                                                                                                                         ifelse(ar.1$depthstrat == "D3" &
                                                                                                                                  ar.1$acrstrat == "MA3" &
                                                                                                                                  ar.1$hbstrat == "HB1",
                                                                                                                                "S14",
                                                                                                                                ifelse(ar.1$depthstrat == "D3" &
                                                                                                                                         ar.1$acrstrat == "MA3" &
                                                                                                                                         ar.1$hbstrat == "HB2",
                                                                                                                                       "S15",
                                                                                                                                       ifelse(ar.1$depthstrat == "D3" &
                                                                                                                                                ar.1$acrstrat == "MA3" &
                                                                                                                                                ar.1$hbstrat == "HB3",
                                                                                                                                              "S16",
                                                                                                                                              ifelse(ar.1$depthstrat == "D4" &
                                                                                                                                                       (ar.1$acrstrat == "MA1"| ar.1$acrstrat == "MA2") &
                                                                                                                                                       (ar.1$hbstrat == "HB1" | ar.1$hbstrat == "HB2"),
                                                                                                                                                     "S17",
                                                                                                                                                     ifelse(ar.1$depthstrat == "D4" &
                                                                                                                                                              ar.1$acrstrat == "MA1" &
                                                                                                                                                              ar.1$hbstrat == "HB3",
                                                                                                                                                            "S18",
                                                                                                                                                            ifelse(ar.1$depthstrat == "D4" &
                                                                                                                                                                     ar.1$acrstrat == "MA2" &
                                                                                                                                                                     ar.1$hbstrat == "HB3",
                                                                                                                                                                   "S19",
                                                                                                                                                                   ifelse(ar.1$depthstrat == "D4" &
                                                                                                                                                                            ar.1$acrstrat == "MA3" &
                                                                                                                                                                            ar.1$hbstrat == "HB1",
                                                                                                                                                                          "S20",
                                                                                                                                                                          ifelse(ar.1$depthstrat == "D4" &
                                                                                                                                                                                   ar.1$acrstrat == "MA3" &
                                                                                                                                                                                   ar.1$hbstrat == "HB2",
                                                                                                                                                                                 "S21",
                                                                                                                                                                                 ifelse(ar.1$depthstrat == "D4" &
                                                                                                                                                                                          ar.1$acrstrat == "MA3" &
                                                                                                                                                                                          ar.1$hbstrat == "HB3",
                                                                                                                                                                                        "S22",
                                                                                                                                                                                        ifelse(ar.1$depthstrat == "D5" &
                                                                                                                                                                                                 (ar.1$acrstrat == "MA1"| ar.1$acrstrat == "MA2") &
                                                                                                                                                                                                 (ar.1$hbstrat == "HB1" | ar.1$hbstrat == "HB2" | ar.1$hbstrat == "HB3"),
                                                                                                                                                                                               "S23",
                                                                                                                                                                                               ifelse(ar.1$depthstrat == "D5" &
                                                                                                                                                                                                        ar.1$acrstrat == "MA3"&
                                                                                                                                                                                                        (ar.1$hbstrat == "HB1" | ar.1$hbstrat == "HB2" | ar.1$hbstrat == "HB3"),
                                                                                                                                                                                                      "S24",
                                                                                                                                                                                                      "SZZ"))))))))))))))))))))))))






ar.data.20182020 <- rbind(ar.1[, c("YEAR","BFISH","Island","STRATA_2020","SAMPLE_DEPTH_M","PSU","SPECIES_CD","LENGTH_CM","CPUE")],
                          ar.data.2020)
View(ar.data.20182020)
write.csv(ar.data.20182020, "BFISH_2018_2020_F_AR data Both Gears Combined STRATA_2020 for allocation.csv", row.names = F)