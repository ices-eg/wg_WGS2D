###########################################################################
# Prepare bathymetry_data
# ==========================================================================
#
# by Mark R Payne  
# DTU-Aqua, Kgs. Lyngby, Denmark  
# http://www.staff.dtu.dk/mpay  
#
# Created Mon Jan  8 12:06:41 2018
# 
# Prepares a polygon mask for use in extracting a region of interest
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
cat(sprintf("\n%s\n","Prepare bathymetry data"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
start.time <- proc.time()[3]; 

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
  flush.console();return(invisible(NULL))}

library(raster)
library(sf)
library(tidyverse)
source("src/00.Common_elements.r")

#==========================================================================
# Configure
#==========================================================================
#ETOPO1 directory
ETOPO1.fname <- "resources/ETOPO1_Bed_c_gmt4.grd"

#==========================================================================
# Process raster
#==========================================================================
log.msg("Processing raster...\n")
#Setup raster
bath.raw <- raster(ETOPO1.fname)

#Crop - with a bit of fluff around the outside
spatial.ROI.fluff <- extend(plot.ROI,y = c(5,5))
bath.crop <- raster::crop(bath.raw,spatial.ROI.fluff,snap="out")

#==========================================================================
# Process polygon
#==========================================================================
log.msg("Creating bathymetry polygon...\n")
#Re-aggregate to a relatively fine resolution, chosen arbitrarily
#in the sake of sanity e.g. 0.125
bath.coarse <- aggregate(bath.crop,fact=0.125/res(bath.raw),fun=median)

#Create a closed polygon at the min spawn depth for extraction
bath.coarse[] <- ifelse(bath.coarse[]< -min(spawn.depth),1,NA)
bath.poly <- 
  rasterToPolygons(bath.coarse,dissolve = TRUE) %>%
  st_as_sf()

#Extract an open polyline for use in plotting
bath.plot <- 
  rasterToContour(bath.crop,levels=c(-1000,-2000)) %>%
  st_as_sf()

#Save objects
saveRDS(bath.poly,file="objects/bathymetry_poly_spawn.rds")
saveRDS(bath.plot,file="objects/bathymetry_poly_plot.rds")

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

