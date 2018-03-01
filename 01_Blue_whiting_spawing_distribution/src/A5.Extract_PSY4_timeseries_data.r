###########################################################################
# Extract PSY4 timeseries data
# ==========================================================================
#
# by Mark R Payne  
# DTU-Aqua, Kgs. Lyngby, Denmark  
# http://www.staff.dtu.dk/mpay  
#
# Created Wed Feb 28 17:24:40 2018
# 
# Processes data downloaded from Copernicus by performing depth-averaging
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
cat(sprintf("\n%s\n","Extract PSY4 timeseries data"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
source("src/00.Common_elements.r")
library(tidyverse)
library(ncdf4)

#==========================================================================
# And go!
#==========================================================================
#==========================================================================
# Configure
#==========================================================================
#Directories
tmp.dir <- tempdir()

#PSY4 configuration
PSY4.db.dir <- file.path(PSY4.data.dir,"database")
PSY4.vars <- c("so")
wt.vertical.ave <- TRUE   #Do the vertical averaging with weighted means? Or simple arithmetics?

#==========================================================================
# Process EN4 files
#==========================================================================
#Get list of files
PSY4.db <- data.frame(fname=dir(PSY4.db.dir,
                                pattern="nc$",full.names = TRUE))

#Loop over files
for(f in PSY4.db$fname) {
    log.msg("Processing %s...",basename(f))
    #Loop over variables
    for(v in PSY4.vars) {
      log.msg("%s...",v)
      #Process using raster
      b.raw <- brick(f,varname=v,lvar=4)
      
      #Get vertical layers
      layer.midpoints <- getZ(b.raw)
      layer.bnds <- c(0,approx(seq(layer.midpoints)-0.5,layer.midpoints,
                                seq(layer.midpoints))$y)
      layer.thickness <- diff(layer.bnds)
      layer.idxs <- which(layer.midpoints > min(spawn.depth) & layer.midpoints < max(spawn.depth))
      
      #Drop vertical layers that we don't need
      b <- b.raw[[layer.idxs]]
      #Average in the vertical
      if(wt.vertical.ave) {
        b <- weighted.mean(b,layer.thickness[layer.idxs])
      } else {
        b <- mean(b,na.rm=TRUE)
      }
      #For cases where we are shallower than the shallowest layer, we use the bottom salinity
      bottom.idx <- sum(!is.na(b.raw))
      bottom.val <- stackSelect(b.raw,bottom.idx,type="index")
      b <- cover(b,bottom.val)   #Replaces NAs in b with bottom values
      #Write the raster out for further use
      fname <- gsub("nc$",sprintf("%s.nc",v),basename(f))
      writeRaster(b,filename = file.path(PSY4.data.dir, fname),overwrite=TRUE)
    }
    log.msg("Done.\n")
}


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

