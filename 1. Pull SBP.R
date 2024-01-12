#------------------------------------------------------------------------------
# Coder: Jilly Hebert
# Date: 12/1/2023

# R code to pull systolic blood pressure (SBP) from RAND dataset.

# Lines 10-14: Load libraries and dataset
# Lines 15-64: Pull all waves of SBP and identify first recorded SBP measure

#------------------------------------------------------------------------------
library(tidyverse)

rand <- read_rds("randhrs1992_2018v1.rds")

#------------------------------------------------------------------------------
# Pull first measure of SBP
#------------------------------------------------------------------------------
sbp <- rand %>%
  dplyr::select(hhid, pn, r8bpsys, r9bpsys, r10bpsys, r11bpsys, r12bpsys,
                r13bpsys, r14bpsys)
sbp$hhidpn <- paste0(sbp$hhid, sbp$pn) #Unique identifier 
sbp <- sbp %>%
  dplyr::select(hhid, pn, hhidpn, r8bpsys, r9bpsys, r10bpsys, r11bpsys,
                r12bpsys, r13bpsys, r14bpsys)


#Find number of recorded values for each person (takes a second)
for(i in 1:nrow(sbp)){
  
  sbp[i, "count"] <- length(which(sbp[i, 4:10] > 0) == TRUE)
  
}

#Distribution of number of values recorded
table(sbp$count) #A lot have none
ggplot(aes(count), data = sbp) +
  geom_histogram(bins = 10)


#Remove people with no sbp values
sbp_clean <- sbp %>%
  dplyr::filter(count > 0)

#Find first value for every participant
for(i in 1:nrow(sbp_clean)){
  
  place <- which(sbp_clean[i, 4:10] > 0)[1]
  sbp_clean[i, "sbp"] <- sbp_clean[i, place + 3] #Skip 3 ID cols
  sbp_clean[i, "wave"] <- place + 7 #place = 1 is wave 8, place = 2 is wave 9, etc.
  
}

#Add year based on wave
sbp_clean$year <- case_when(sbp_clean$wave == 8 ~ 2006,
                            sbp_clean$wave == 9 ~ 2008,
                            sbp_clean$wave == 10 ~ 2010,
                            sbp_clean$wave == 11 ~ 2012,
                            sbp_clean$wave == 12 ~ 2014,
                            sbp_clean$wave == 13 ~ 2016,
                            sbp_clean$wave == 14 ~ 2018)

sbp_fin <- sbp_clean %>%
  dplyr::select(hhid, pn, hhidpn, sbp, wave, year)
#write_rds(sbp_fin, "SBP_Measures.rds")

