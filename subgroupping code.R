install.packages("dplyr")
install.packages("ggplot2")
install.packages("psych")
install.packages("ggfortify")
install.packages("Rmisc")
install.packages("sjstats")
install.packages("car")
install.packages("outliers")
install.packages("reader")

library(reader)
library(dplyr)
library(stats)

#import the data
getwd() 
setwd("Z:/data/FLUXNET/Deciduous Broadleaf Forest/FLX_AU-Lox_FLUXNET2015_FULLSET_2008-2009_1-3") 


rm(list = ls ()) 

FLX_AU_Lox <- read.csv("FLX_AU-Lox_FLUXNET2015_FULLSET_HH_2008-2009_1-3.csv", header = TRUE, sep = ",")
attach(FLX_AU_Lox)


#this part is revised from Elin Jacobs' original script
#filter ET data
LE <- FLX_AU_Lox$LE_F_MDS  #Latent heat flux in  W m-2 (hourly)
#Convert LE to ET rate (mm hr-1)
L <- 2.5 # [MJ kg-1], latent heat of vaporization of water at 0 degC (temperature dependent)
H2Odens <- 1000 # [kg m-3] density of water at 4 degC (temperature dependent) 
FLX_AU_Lox$ET <- LE/H2Odens*L # ET rate [mm hr-1] 
#Filter ET based on conditions in Novick et al. 2016 Nat.Clim.Change
FLX_AU_Lox <- FLX_AU_Lox %>% subset(ET > 0.6 & ET < 2)

#filter out significant outliers temperature<-30, vpd<0, 500<radiation<1500
FLX_AU_Lox %>%
  select(TA_F_MDS, VPD_F_MDS, NETRAD)
FLX_AU_Lox <- subset(FLX_AU_Lox, TA_F_MDS >= -30 & VPD_F_MDS >= 0 & NETRAD > -500 & NETRAD < 1500) 

#filter out night time when net radiation < 50 and Wind speed>1 and VPD >0.6
FLX_AU_Lox %>%
  #WS_F is the wind speed, VPD_F_MDS is VPD and NETRAD is net radiation
  select(WS_F, VPD_F_MDS, NETRAD)
FLX_AU_Lox <- FLX_AU_Lox %>% subset(VPD_F_MDS > 0.6 & WS_F > 1 & NETRAD > 50)

#filter time 2 weeks after average temp larger than 0 and 3 weeks before
  #reformat the dates, split day and day of the year
  FLX_AU_Lox$Dates <- format(as.POSIXct(strptime(FLX_AU_Lox$TIMESTAMP_START,"%Y%m%d%H%M",tz="")))
  unlist(day, month, year(Dates))
  FLX_AU_Lox$yday <- Dates[yday]
  
#The winter is excluded for 3 weeks after and 2 weeks before the first and last day whose mean temp is smaller than 0  
  
#calculate the mean temperature each day
group_by(Dates) %>% 
  meantemp <- mean (TA_F_MDS, na.rm = TRUE)
#find the dates to start and stop
group_by(year) %>%
mindate <- min(yday[which(meantemp > 0)])
startdate <- mindate+20
group_by(year) %>%
maxdate <- max(yday[which(meantemp > 0)])
stopdate <- maxdate-13
#filter the day
group_by(year) %>%
FLX_AU_Lox <- subset(FLX_AU_Lox, yday > startdate, yday < stopdate)