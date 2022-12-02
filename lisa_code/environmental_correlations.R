################################################
## initial correlations between US-RRC flux data 
## and environmental variables
## Lisa Haber
## November 2022
###############################################

## load libraries
library(tidyverse)
library(lubridate)

# load data
rrc22 <- read.csv("C:/github/MLABcode/data/timeseries_fluxes_usrrc_2022partial.csv", header = TRUE)
head(rrc22)

## count number of useful flux values (i.e. Foken flag 0 or 1)
high_qual_flux <- rrc22 %>%
  filter(qc_co2_flux == 0 | qc_co2_flux == 1) 

count(high_qual_flux) #5837 of 7061 total flux values can be retained, or 82.7 %

length(unique(high_qual_flux$date)) # 151 days in date set



