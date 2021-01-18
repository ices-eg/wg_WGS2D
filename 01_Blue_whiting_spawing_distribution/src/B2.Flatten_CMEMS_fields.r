###########################################################################
# Flatten CMEMS data
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
cat(sprintf("\n%s\n","Flatten CMEMS Data"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
source("src/00.Common_elements.r")
library(tidyverse)
library(ncdf4)
library(RCMEMS)
library(ClimateOperators)
library(here)
library(magrittr)

#==========================================================================
# Configure
#==========================================================================
#Directories
tmp.dir <- tempdir()

#'========================================================================
# Setup ####
#'========================================================================
#Import timeseries configurations
CMEMS.cfgs <- readRDS("objects/CMEMS_cfgs.rds")

#Setup vertical coordinates
vert.layers <- seq(min(spawn.depth),max(spawn.depth),by=5)

#Setup grid description by copying the EN4 grid
EN4.dat <- 
  readRDS(here("objects/PredEng_MR_EN4_salinity_field.rds")) %>%
  pull(field) %>%
  extract2(1)
griddes.fname <- "data/grid_descriptor.txt"
writeLines(griddes(extent(EN4.dat),grid.res),griddes.fname)

#Rewrite all
reprocess.all <- FALSE

#==========================================================================
# Process files
#==========================================================================
#Loop over models
for(mdl in names(CMEMS.cfgs)) {
  #Import meta data
  this.CMEMS <- CMEMS.cfgs[[mdl]]
  meta.db <- readRDS(file.path(this.CMEMS@out.dir,"..","database_metadata.rds"))
  #Get list of available files
  flat.dir <- file.path("data",mdl,"flattened")
  mdl.db <- tibble(fname=dir(this.CMEMS@out.dir,pattern="nc$",full.names = TRUE),
                   src.date=file.mtime(fname),
                   ex.fname=file.path(flat.dir,basename(fname)),
                   ex.exists=file.exists(ex.fname),
                   ex.date=file.mtime(ex.fname))
  
  #Compare databases
  if(reprocess.all) {
    src.to.process <- mdl.db
  } else {
    src.to.process <- filter(mdl.db,!ex.exists | src.date > ex.date )
  }
  n.to.process <- nrow(src.to.process) 
  if(n.to.process==0) {
    log.msg("No files to process for %s...\n",mdl)
    next
  }
  
  #Check that directory exists
  if(!dir.exists(flat.dir)) dir.create(flat.dir,recursive=TRUE)
  
  
  pb <- progress_estimated(n.to.process)
  log.msg("Processing %i files for %s...",n.to.process,mdl)

  #Loop over files
  for(i in seq(n.to.process)) {
    pb$print()
    f <- src.to.process[i,]
    
    #Interpolate vertically, average and remap
    cmd <- cdo("--silent -W",
               csl("remapbil",griddes.fname),
               "-vertmean",
               csl("-intlevel",vert.layers),
               f$fname,
               f$ex.fname)
    
    pb$tick()
  }
  #Finish off timer
  pb$stop()$print()
  log.msg("\n")
  
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

