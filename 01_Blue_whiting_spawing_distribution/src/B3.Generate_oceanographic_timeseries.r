###########################################################################
# Generate_oceanographic_timeseries
# ==========================================================================
#
# by Mark R Payne  
# DTU-Aqua, Kgs. Lyngby, Denmark  
# http://www.staff.dtu.dk/mpay  
#
# Created Wed Feb 28 17:02:50 2018
# 
# Generates time series of oceanographic data averaged over a region
# of interest.
#
# This work is subject to a Creative Commons "Attribution" "ShareALike" License.
# You are largely free to do what you like with it, so long as you "attribute" 
# me for my contribution. See the fine print at the end for exact details.
#
# To do:
#
# Notes:
#
###########################################################################

#==========================================================================
# Initialise system
#==========================================================================
cat(sprintf("\n%s\n","Generate_oceanographic_timeseries"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
source("src/00.Common_elements.r")
library(tidyverse)
library(tibble)
library(stringr)
library(lubridate)

#==========================================================================
# Setup file lists
#==========================================================================
#Get list of EN4 data files
EN4.fnames <- dir( EN4.data.dir,full.names = TRUE,pattern="nc$")
EN4.meta <- tibble(source="EN4",
                   fname=EN4.fnames,
                      date.str=str_extract(EN4.fnames,"[0-9]{6}"),
                      date=ymd(paste0(date.str,"01")))

#Get list of PSY4 files
PSY4.fnames <- dir(PSY4.data.dir,full.names = TRUE,pattern="nc$")
PSY4.meta <- tibble(source="PSY4",
                    fname=PSY4.fnames,
                   date.str=str_extract(PSY4.fnames,"[0-9]{8}"),
                   date=ymd(date.str))

#Merge
meta.df <- rbind(EN4.meta,PSY4.meta)

#==========================================================================
# Process files
#==========================================================================
#Setup polygon to delineate depth regions
load("objects/bathymetry.RData")

#Loop over files
meta.df$salinity <- as.double(NA)
for(i in seq(nrow(meta.df))) {
  log.msg("Processing %s...\n",basename(meta.df$fname[i]))
  #Setup raster
  r <- raster(meta.df$fname[i])
  
  #Resize to appropriate ROI
  r <- raster::crop(r,oceanography.ROI)
  
  #Mask with bathymetry
  r.masked <- mask(r,bath.poly)
  
  #Perform averaging
  area.r <- area(r.masked)
  area.norm <- cellStats(area.r*!is.na(r.masked),sum)
  mean.val <- cellStats(area.r*r.masked,sum)/area.norm
  
  #Store
  meta.df$salinity[i] <- mean.val

}

ocean.dat.df <- meta.df

save(ocean.dat.df,file="objects/ocean_data.RData")

#==========================================================================
# Complete
#==========================================================================
#Turn off the lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log.msg("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date())

# -----------
# This work by Mark R Payne is licensed under a  Creative Commons
# Attribution-NonCommercial-ShareAlike 3.0 Unported License. 
# For details, see http://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US
# Basically, this means that you are free to "share" and "remix" for 
# non-commerical purposes as you see fit, so long as you "attribute" me for my
# contribution. Derivatives can be distributed under the same or 
# similar license.
#
# This work comes with ABSOLUTELY NO WARRANTY or support.
#
# This work should also be considered as BEER-WARE. For details, see
# http://en.wikipedia.org/wiki/Beerware
# -----------

