## playing around with flux data, USRRC
# sandbox code for flux data processing

# load packages
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


## count values with quality flag of 2 or value of -9999 (based on Mauder & Foken system)
low_qual_flux <- rrc22 %>%
  filter(qc_co2_flux == 2 | qc_co2_flux == -9999) %>%
  summarize(n = n())

low_qual_flux # 1224 of 7061 total flux values need to be discarded

## count questionable but not terrible fluxes, Foken flag = 1
intermed_qual_flux <- rrc22 %>%
  filter(qc_co2_flux == 1) %>%
  summarize(n = n())

intermed_qual_flux

## daylight saving time began March 14, 2021 at 02:00
# this was DOY 73, switched from GMT -5 to GMT -4

# visualize CO2 flux with DOY as the x variable (i.e. time)

plot1 <- high_qual_flux %>%
  filter(co2_flux != -9999) %>%
  ggplot(aes(x = DOY, y = co2_flux)) + 
  geom_point()

plot1

plot2 <- rrc22 %>%
  filter(co2_flux != -9999, 
         co2_flux < 10,
         co2_flux > -10) %>%
  ggplot(aes(x = DOY, y = co2_flux)) + 
  geom_point()

plot2

# now methane flux
plotm1 <- rrc22 %>%
  filter(ch4_flux != -9999) %>%
  ggplot(aes(x = DOY, y = ch4_flux)) + 
  geom_point()

plotm1

# h20 flux
ploth1 <- rrc22 %>%
  filter(h2o_flux != -9999) %>%
  ggplot(aes(x = DOY, y = h2o_flux)) + 
  geom_point()

ploth1

ploth2 <- rrc22 %>%
  filter(h2o_flux != -9999, 
         h2o_flux < 15,
         h2o_flux > -5) %>%
  ggplot(aes(x = DOY, y = h2o_flux)) + 
  geom_point()

ploth2


### histogram for wind direction
# if only a small subset of data points are coming from problematic areas to northeast/east of tower, then....that's good news
as.numeric(high_qual_flux$wind_dir)
hist(high_qual_flux$wind_dir)

# what fraction of high quality fluxes are between directions 0 deg (N) & 135 deg (SE)?
size = length(high_qual_flux$wind_dir)
size

upper_wind_lim = 0
lower_wind_lim = 135
for(i in 1:size)
 
{
  if(high_qual_flux$wind_dir[i] >= upper_wind_lim && high_qual_flux$wind_dir[i] <= lower_wind_lim)
    sum = sum + 1
  
}

print(sum) # 1128 values in this problematic set of directions
(1128/5837)*100 # 19.33 % of values are gonna need to be tossed coming from this area


####### code imported from Sara Knox: https://github.com/sknox01/Manitoba_Flux_Processing/blob/main/Young%20Processing/2.L2_filteringYoung.Rmd
## load libraries
library(plotly)
library(readxl)
library(REddyProc)
library(tidyverse)
library(dplyr)
library(lubridate)
library(gtools)

# load non-filtered data
input<-read.csv("C:/github/MLABcode/data/timeseries_fluxes_usrrc_2022partial.csv", header = TRUE)
input <- input %>%
  mutate(DATE = paste(date, time, sep=' '))

input$DATE<-as.POSIXct(input$DATE,format="%m-%d-%y %H:%M:")

# Define new data frame
Data_QAQC<-input
ls(Data_QAQC) # list variables

## NEED to first filter out the -9999 values or convert to NA
# for H2O flux:
h2o_flux_mean <- Data_QAQC %>%
  filter(h2o_flux > -9999) %>%
  summarise(mean(h2o_flux),
            sd = sd(h2o_flux)) # mean is 2.597, sd is 3.841

# define L2 filters

# pitch_max <- 7 #[deg] ## hmmmm....ask about this, our range is huge. -62.08 to 77.27
# pitch_min <- -7 #[deg]
# WD -> ADD LATER!
qc_flag <- 1 #[#] 
w.ts_cov_max <- 0.5 #[m+1K+1s-1] ## our range is -0.196 to 0.247 ## CHECK for final
# ts_var_max <- 3 #[K+2] ## our range is 0.00219 to 13.324 ## CHECK for final
# mean_value_RSSI_LI.7200_min <- 50 ## ALL our values are -9999, we have no 7200 installed
mean_value_LI.7500_min <- 50 ## our range is 13 to 100 ## CHECK for final
h2o_flux_min <- -15 #[mmol+1s-1m-2] # our actual minimum is -23 or so, but -15 looks reasonable ## CHECK for final
h2o_flux_max <- 25 #[mmol+1s-1m-2] # our max is 25.493 ## CHECK for final
# h2o_var_max <- 9000 # our highest value is 345313, min is -9999 ## CHECK for final
# h2o_mean_max <- 2000 #[mmol/m3]  ## CHECK for final
# co2_mean_min <- 0 #[mmol/m3]  ## CHECK for final
# co2_var_max <- 0.2 # our range is 0.0000773 to 72.805 in the high qual fluxes. CHECK for final
co2_flux_max <- 100 #[µmol+1s-1m-2] ## eyeballing this! ## CHECK for final 
co2_flux_min <- -75 #[µmol+1s-1m-2] ## eyeballing this! ## CHECK for final
rssi_77_mean_min <- 20 #[#] this looks pretty standard, don't expect to change it later
# ch4_mean_min <- 1.7 # [ppm] ## CHECK for final
# ch4_var_max <- 0.0005 ## our range is -9.99e+03 to 3.76 e-03 ## CHECK for final
ch4_flux_min <- -0.5 #[µmol+1s-1m-2] ## our range is -2 to 2, roughly. eyeballing this ## CHECK for final
ch4_flux_max <- 1.5 #[µmol+1s-1m-2] ## CHECK for final
ustar_max <- 1.2 # our range is 0.0056 to 0.8382 ## CHECK for final
wind_max <- 360 #degrees ## eyeballing from map ## CHECK for final
wind_min <- 100 #degrees ## eyeballing from map ## CHECK for final
# L_max <- 10000 # [m] - added 13/08/21 - D. Ng ## not sure what these are ## CHECK for final
# L_min <- -10000 # [m] - added 13/08/21 - D. Ng ## not sure what these are ## CHECK for final

# Plot filtering variable of interest to check values
plot_ly(data = Data_QAQC, x = ~DATE, y = ~co2_var, name = 'pitch', type = 'scatter', mode = 'line') %>%
  layout(yaxis = list(range = c(-1, 1))) %>% 
  plotly::toWebGL()


## what is mean_value_li_7500
range(Data_QAQC$mean_value_LI.7500)
as.numeric(high_qual_flux$mean_value_LI.7500)
hist(high_qual_flux$mean_value_LI.7500)
