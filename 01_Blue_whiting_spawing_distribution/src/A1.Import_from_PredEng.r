#'========================================================================
# A1.Import from PredEng
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Thu Mar 19 08:12:00 2020
#
# Imports the results of an analysis performed with the PredEng
# Prediction Engine
#
# This work is subject to a Creative Commons "Attribution" "ShareALike" License.
# You are largely free to do what you like with it, so long as you "attribute"
# me for my contribution. See the fine print at the end for exact details.
#
# To do:
#
# Notes:
#
#'========================================================================

#'========================================================================
# Initialise system ####
#'========================================================================
cat(sprintf("\n%s\n","A1.Import from PredEng"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

#Helper functions, externals and libraries
source("src/00.Common_elements.r")
library(tidyverse)
library(magrittr)
library(lubridate)
library(here)
library(RSQLite)
library(raster)

#'========================================================================
# Configuration ####
#'========================================================================
predEng.db <- here("data/PredEng/Blue_whiting_WGS2D.sqlite")  #Directory containing results for BW from PredEng

#==========================================================================
# Setup
#==========================================================================
#Get and filter list of statistics
this.db <- dbConnect(RSQLite::SQLite(), predEng.db)
stats.tbl <- 
  tbl(this.db,"statistics") 

obs.stats <- 
  stats.tbl %>%
  filter(srcType =="Observations")

#'========================================================================
# Field predictions ####
#'========================================================================
deblob <- function(x) {
  x %>%
    mutate(field=map(field,unserialize))
}

SDM.res <-
  obs.stats %>%
  filter(statName=="SDM15Apr") %>%
  collect() %>%
  deblob()

EN4.res <-
  obs.stats %>%
  filter(srcName=="EN4",
         statName=="Mean-salinity") %>%
  collect() 

#Calculate the climatology field
SDM.clim.df <-
  SDM.res %>%
  filter(srcType=="Observations",
         year(date) %in% climatology.yrs)

SDM.clim <-
  brick(SDM.clim.df$field) %>%
  mean()


#Save results
saveRDS(SDM.res,file="objects/PredEng_SDM.rds")
saveRDS(EN4.res,file="objects/EN4_mean_salinity.rds")

saveRDS(SDM.clim,file="objects/SDM_climatology.rds")

#'========================================================================
# Extracted salinity field  ####
#'========================================================================
#Most recent EN4 salinity
mr.EN4 <- 
  tbl(this.db,"calibration") %>%
  filter(srcType=="Observations",
         srcName=="EN4") %>%
  collect() %>%
  filter(date==max(date)) %>%
  mutate(field=map(data,unserialize))

saveRDS(mr.EN4,file="objects/EN4_salinity_field.rds")

#==========================================================================
# Complete
#==========================================================================
#Turn off the lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log.msg("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date())

# .............
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
# .............
