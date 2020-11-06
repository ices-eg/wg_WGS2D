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
library(stringr)
library(lubridate)

#'========================================================================
# Configuration ####
#'========================================================================
predEng.dir <- "resources/BW-Salinity//"  #Directory containing results for BW from PredEng

#==========================================================================
# Setup
#==========================================================================
#Get and filter list of statistics
stats.cfg <- 
  readRDS(file.path(predEng.dir,"Statistics","Stats_configuration.rds"))  %>%
  unnest(data) 

stats.l <- 
  stats.cfg %>%
  filter(src.type %in% c("Observations","Persistence")) %>%
  mutate(stats= purrr::map(res.fname, ~ readRDS(file.path(predEng.dir,"Statistics",.x)))) 


#'========================================================================
# Field predictions ####
#'========================================================================
#Extract the SDM predictions 
stat.extract <- function(...) {
  stats.l %>%
    filter(...) %>% 
    select(stats) %>%
    unnest(stats)
}

SDM.res <-
  stat.extract(stat.name=="SDM15Apr") 

EN4.res <-
  stat.extract(src.name=="EN4",
               stat.name=="Mean-salinity") 

EN4.SalField <-
  stat.extract(src.name=="EN4",
               stat.name=="SalField") 

#Extract ROIs
global.ROI <-
  EN4.SalField %>%
  pull(field) %>%
  extract2(1)

#Calculate the climatology field
SDM.clim.df <-
  SDM.res %>%
  filter(src.type=="Observations",
    year(date) %in% climatology.yrs)

SDM.clim <-
  brick(SDM.clim.df$field) %>%
  mean()




# meta.df <- subset(meta.df.all,month(date)==spawn.month &
#                               year(date) %in% climatology.yrs)
# 
# #Process by type
# pred.clim <- list()
# for(typ in unique(meta.df$type)){
#   #Select
#   typ.sel <- filter(meta.df,type==typ)
#   preds.s <- stack(typ.sel$fname)
#   
#   #And average
#   pred.clim[[typ]] <- mean(preds.s)
# }

#Save results
saveRDS(SDM.res,file="objects/PredEng_SDM.rds")
saveRDS(EN4.res,file="objects/EN4_mean_salinity.rds")
saveRDS(EN4.SalField,file="objects/EN4_salinity_field.rds")
saveRDS(SDM.clim,file="objects/SDM_climatology.rds")
saveRDS(global.ROI,file="objects/global_ROI.rds")

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
