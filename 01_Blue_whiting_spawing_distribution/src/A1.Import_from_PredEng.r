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

#'========================================================================
# Time series values ####
#'========================================================================
#Setup scalar table
obs.stats <- 
  tbl(this.db,"statistics") %>%
  filter(srcType =="Observations") %>%
  select(-field,-calibrationMethod,-realization,-startDate,-leadIdx)

#SDM results
SDM.res <-
  obs.stats %>%
  filter(statName=="SDM",
         !is.na(value)) %>%
  collect() 
saveRDS(SDM.res,file="objects/PredEng_SDM.rds")

EN4.res <-
  obs.stats %>%
  filter(srcName=="EN4",
         statName=="Mean-salinity") %>%
  collect() 
saveRDS(EN4.res,file="objects/PredEng_EN4_mean_salinity.rds")


#'========================================================================
# Most recent fields  ####
#'========================================================================
# Get mr date first
mr.date <- as.character(max(ymd(EN4.res$date)))

deblob <- function(x) {
  x %>%
    mutate(field=map(field,unserialize))
}

#Most recent EN4 salinity
mr.EN4 <- 
  tbl(this.db,"extraction") %>%
  filter(srcType=="Observations",
         srcName=="EN4",
         date==mr.date) %>%
  collect() %>%
  deblob()
saveRDS(mr.EN4,file="objects/PredEng_MR_EN4_salinity_field.rds")

#Selected SDM fields
sel.SDM <-
  tbl(this.db,"statistics") %>%
  filter(srcType =="Observations",
         statName =="SDM",
         date %in% c(mr.date,"2007-03-16","2013-03-16"),
         is.na(value)) %>%
  collect() %>%
  deblob() %>%
  select(-calibrationMethod,-realization,-startDate,-leadIdx)
saveRDS(sel.SDM,file="objects/PredEng_Sel_SDM_fields.rds")  

#'========================================================================
# Persistence metrics ####
#'========================================================================
metrics <- 
  tbl(this.db,"metrics") %>%
  filter(srcType =="Persistence") %>%
  collect()
saveRDS(metrics,file="objects/PredEng_metrics.rds")


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
