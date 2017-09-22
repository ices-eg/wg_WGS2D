

#===================================================================================================================
# Persistence Forecast of Blue whiting's Spawning Distribution
# 
# Parent file --> Script to run all R files
#===================================================================================================================


# Clean up
rm(list=ls())

# Load required libraries 
library(mgcv);library(RCurl);library(lubridate); library(sp);library(raster); library(ncdf4);library(RColorBrewer); 
library(ggplot2); library(Metrics)

# Enter path of working directory 
working.dir = "~/DTU Aqua Work/Working group - ICES S2D/BW forecast"
setwd(working.dir)


# ===================================================================================================================
# 1A Download & store EN4 (EN4.2.0) Data 
# ===================================================================================================================
# open function called "" that downloads EN4 data
source("Code/1_A_download_EN.4.2.0_data.r")

# enter path name, where data shall be stored  
path <- "Data/EN4.2.0"
# please note that you have to CREATE THESE FOLDERS, otherwise the data cannot be stored 
# & following  error will come up in 1A : .... 'No such file or directory'


# Choose year(s) & month(s) for which  EN4 data shall be downloaded
year <- c(1945:2016) # entire time series   needed to perform each step !
# 2017                 or choose the most recent year
# c(1950:2017)         needed for plotting the timeseries: Figure 3 A & B



# edit input of function    
downloadEN4_fun(yrs= year) # year(s) for which EN4 data shall be downloaded

#  add optional variables within downloadEN4_fun              
               # en.version = ,  # Choose EN4 Version (default= "EN.4.2.0")
               # ana = ,         # Choose whether objective analyses or profiles are required (default= "analyses") 
               # ref = )         # Choose which correction shall be used (default= "g10" = Gouretski and Reseghetti (2010) corrections)
                             #                                              (...l09 =  Levitus et al. (2009) corrections )

# an error message will appear, in case the chose year(s) cannot be downloaded automatically or are not yet available!
# error: #cannot open URL ...."

# or the folders mentioned in the "path", are not existent
# , then following  error will come up in 1A : .... 'No such file or directory'


# the zip folders within "path" can be deleted, when all files are unpacked :)

# ===================================================================================================================
# 1B Set up a Sampling Grid with a 0.25 degree resolution for the extraction of EN4 data
# ===================================================================================================================

source("Code/1_B_sampling_grid_for_EN4_extraction.R")

sampling_grid_fun(yrs= year, month=c(1:12))  # Choose year(s) for which  EN4 data shall be downloaded
                           # months =3,       Choose  month(s) for which  EN4 data shall be extracted 



# sampling_grid_fun(yrs= 2017, month= c(1:4))  


# a summary of the sampling grid is shown 
# and the sampling grid saved in ("Code/Output/samplin_grid.csv")


#=========================================================================================================
# 1C Extract EN4 data for a regularly spaced 0.25 deg sampling grid
# ========================================================================================================
source("Code/1_C_EN4_extraction_onto_sampling_grid.R")

# by default temperature & salinity from the sea surface (SST, SSS) 
# and at blue whitings spawning depth (252-596m; Tdeep,Sdeep)
# will be extracted

# check with files were downloaded
files <- list.files(path = path)
# last element of folder will be used as sample for the file name
file.idx <- substr(files[length(files)],1,24)
file.pth <- paste0(path,"/",file.idx)

# please not that EN4 data for all the years and months needs to be available to run this function
# i.e. if one only has data up to march for the most recent year, choose the previous year as and date
# and then run the current years separately again

extractEN4_fun(data.file.path = file.pth)

# the data is saved as Data/Output/EN4Prediction_min(yrmon)_max(yrmon)



#=========================================================================================================
# 
# ========================================================================================================

# in case only one year is added to an existing data set :

en1 <- read.csv2("Data/Output/EN4Prediction_194501_201612.csv")   # insert path of main(OLD) EN4 data set
en2 <- read.csv2("Data/Output/EN4Prediction_201612_201704.csv")   # insert path of NEW EN4 data set

# maybe following step is necessary
en1$X <- NULL
en2$X <- NULL
en1$X.1 <- NULL
en2$X.1 <- NULL

en.all <- rbind(en1, en2)
summary(en.all)

write.csv2(en.all, file=paste0("Data/Output/EN4Prediction_",min(en.all$yrmon),"_",max(en.all$yrmon),".csv"))


#=========================================================================================================
# 2 Plot Projected spawning habitat for blue whiting in April 
# ========================================================================================================
source("Code/2_Map_of_Suitable_spawning_habitat.R")

map_bw_habitat() 
# yr = max(year) # by default data from March of the most recent year that was chosen is selected
# month = 3, # by default data from march of the most recent year that was chosen is selected
# prob = 0.4 # probability for which an isline will be plotted

# if "null device 1" appears:
# Output saved in: Figures/2_Map_of_projected_blue_whiting_spawning_habitat_April_",max(year),".png

# Figure 2: 
# Projected spawning habitat for blue whiting in April YEAR X.
# The colour scale corresponds to the probability of observing blue whiting larvae by the Continuous Plankton Recorder: 
# probabilities > 0.4 (black contour line) can be considered as the core spawning habitat.



#===============================================================================================================
# 3 Time-series of the area of suitable spawning habitat[10^5 km2](A) & its westward extent (B) of blue whiting 
# ==============================================================================================================
source("Code/3_Suitable_spawning_habitat_area_westward_extent_timeseries.r")

# # note that in order to run this code, EN4 data from March 1950 onwards
# # needs to be extracted
# year <- c(1955:2017)

# calculates the area and westward extent  -------------------------------------------------------------
# of each year within year for a prbability greater or equal prob

# add path of en4 data from 194501 to most recent year-month within data set
en <- read.csv2("Data/Output/EN4Prediction_194501_201704.csv") 

# 1954 intervals not constant.....


area_w.extent_bw_habitat(en.data = en)
  # prob     = 0.4,          # area of probability  greater or equal prob will be plotted

# if NA --> the selected probability might be to large


# plots the 2 time series ------------------------------------------------------------------------------

timeseries_bw_habitat()
# yrs      = year ,        # years inlcuded in the time series -> c(1950:2017)
# prob     = 0.4,          # area of probability  greater or equal prob will be plotted
# plot.yrs = c(1950,2020), # time series start & end date 
# ylim.A   =  c(0.5, 3.5), # ylim for plotting the area [in 10^5 km^2]
# lim.W     = c(-12, -22))  # limit for plotting westward extent [in degrees east]


# Plot A) 
# Time-series of the area of suitable spawning habitat[10^5 km2] of blue whiting 
# in the month of peak larval presence (April), with model extrapolations  indicated by the dashed line.
# Suitable spawning habitat is defined as a probability (p) of observing larvae larger or equal 0.4

# Plot B)
# Time-series of the westward extent [longitude in degrees °E] of suitable spawning habitat of blue whiting 
# in the month of peak larval presence (April), with model extrapolations  indicated by the dashed line.
# Suitable spawning habitat is defined as a probability (p) of observing larvae larger or equal 0.4




#===============================================================================================================
# 4) Create &  Plot persistence forecast skill 
# ==============================================================================================================

# A ) Create Presistence forecast --------------------------------------------------------------------------


#en <- read.csv2("Data/Output/EN4Prediction_194501_201704.csv") # add path of en4 data from 194501 to most recent year-month within data set

source("Code/4_Persistence_forecast_skill.R")

max.yr.ID <- 201704

en <- read.csv2(paste0("Data/Output/EN4Prediction_194501_",yrmon.end,".csv"))

persistence_skill_area_min_lon(yrmon.end = max.yr.ID) # most recent data within en4 (Data/Output/EN4Prediction_194501_201703.csv)

#  prob = 0.4,         # probability (p) of observing larvae with p greater or equal prob -- SAME AS IN PLOT 2
#  yrmon.start=194501, # starting yrmon index from which en4 dataset was extracted, e.g. "Data/Output/EN4Prediction_195001_201703" --> yrmon1 = 195001, yrmon2= 201703
# yrmon.end            # ending yrmon 


# data output --------------------------

# predictions of the size of the main spawning area (p> prob) in April for each month of each year
    # "Code/Output/Area_of_probability_greater_or_equal_",prob,"_monthly_from_",yrmon.start,"_to_",yrmon.end,".csv"))

# observed area & westward extent:
    #" Code/Output/obs.dat_area_min.lon_probability_greater_or_equal_",prob,"_",yrmon.start,"_",yrmon.end,".csv"))

# forecasts of
# the suitable spawning area [in km2]  (p> prob) based on march salinity
    # "Code/Output/eval.datA_",prob,"_",yrmon.start,"_",yrmon.end,".csv"  

# & westward extent of suitable spawning area  (p> prob)  based on march salinity
    # "Code/Output/eval.datW_",prob,"_",yrmon.start,"_",yrmon.end,".csv"





# FIGURES
  # "Figures/3_A_5yr_Persistence_Forecast_cor_skill.png"
  # "Figures/3_B_5yr_Persistence_Forecast_RMSE_skill.png"

