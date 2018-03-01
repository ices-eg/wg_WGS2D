###########################################################################
# Generate oceanographic climatology
# ==========================================================================
#
# by Mark R Payne  
# DTU-Aqua, Kgs. Lyngby, Denmark  
# http://www.staff.dtu.dk/mpay  
#
# Created Wed Feb 28 17:02:50 2018
# 
# Generates the climatology of oceanographic data
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
# Process
#==========================================================================
#Get list of EN4 data files
EN4.fnames <- dir( EN4.data.dir,full.names = TRUE,pattern="nc$")
EN4.meta <- tibble(source="EN4",
                   fname=EN4.fnames,
                   date.str=str_extract(EN4.fnames,"[0-9]{6}"),
                   date=ymd(paste0(date.str,"01")),
                   year=year(date))

#Subset list to get the climatology
clim.meta.df <- subset(EN4.meta,year %in% climatology.yrs)

#Now make one big stack over the whole lot
data.stack <- stack(clim.meta.df$fname)

#And average
data.clim <- mean(data.stack)

#Save results
save(data.clim,file="objects/Oceanographic_climatology.RData")

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

