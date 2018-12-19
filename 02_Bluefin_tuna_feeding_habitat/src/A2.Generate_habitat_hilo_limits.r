#'========================================================================
# A2.Generate_habitat_hilo_limits
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Wed Dec 19 09:14:13 2018
#
# Generates the high and low habitat boundary limits based for use in
# plotting
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
cat(sprintf("\n%s\n","A2.Generate_habitat_hilo_limits"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
  flush.console();return(invisible(NULL))}

library(tidyverse)
library(lubridate)
library(raster)

#'========================================================================
# Configure ####
#'========================================================================
#HadISST data source
HadISST.b <- brick("data/HadISST_sst.nc")

#Habitat model
habitat.thresh <- 11
focus.month <- 8

#Output conditions
range.years <- 1960:2015

#Import other data as a reference
load("outputs/Prob_forecast.RData")

#'========================================================================
# Setup ####
#'========================================================================
#Subset data spatially and temporally
ROI.b <- crop(HadISST.b,clim.r)
dat.b <- ROI.b[[which(month(getZ(ROI.b))==focus.month & year(getZ(ROI.b)) %in% range.years)]]

#Interpolate to match resolutions
dat.hires <- disaggregate(dat.b,fact=res(dat.b)/res(clim.r),method="bilinear")

#Apply habitat model
hab.b <- dat.b> habitat.thresh

#Average 
prop.suit.hab <- mean(hab.b)



#Calculate maximum and minimum extents
min.extent <- prop.suit.hab<1
max.extent <- prop.suit.hab>0

#'========================================================================
# Complete ####
#'========================================================================
#Save results
hab.range <- brick(list(prop.suit.hab=prop.suit.hab,min.extent=min.extent,max.extent=max.extent))
save(hab.range,file="outputs/Habitat.variability.RData")

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
