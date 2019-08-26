###########################################################################
# EN4_data_extraction
# ==========================================================================
#
# by Mark R Payne  
# DTU-Aqua, Kgs. Lyngby, Denmark  
# http://www.staff.dtu.dk/mpay  
#
# Created Fri Jan  5 15:52:34 2018
# 
# Extracts data of interest from the compressed EN4 dataset and stores it for further
# processing 
#
# This work is subject to a Creative Commons "Attribution" "ShareALike" License.
# You are largely free to do what you like with it, so long as you "attribute" 
# me for my contribution. See the fine print at the end for exact details.
#
# To do:
# * Account for preliminary products too 
# * Resample to prediction resolution here, rather than in the prediction script
#
# Notes:
#  * Files in the EN4 database should either be either as compressed archive (*.zip) 
#    or as fully decompressed files (*nc). Gzipped files (*.nz.gz) are not supported
#    and need to be decompressed manually prior to running this script.
#
###########################################################################

#==========================================================================
# Initialise system
#==========================================================================
cat(sprintf("\n%s\n","EN4_data_extraction"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
source("src/00.Common_elements.r")
library(raster)
library(ncdf4)
library(tidyverse)
library(lubridate)

#==========================================================================
# Configure
#==========================================================================
#Directories
tmp.dir <- tempdir()

#EN4 configuration
EN4.db.dir <- file.path(EN4.data.dir,"database")
EN4.ex.dir <- file.path(EN4.data.dir,"extraction")
EN4.vars <- c("salinity")
EN4.layers <- 19:23   #Layers from the EN4 product to extract
wt.vertical.ave <- FALSE   #Do the vertical averaging with weighted means? Or simple arithmetics?

#==========================================================================
# Setup processing EN4 files
#==========================================================================
#Check
if(length(EN4.vars)!=1) stop("Cannot process multiple variables (yet)")

#Get list of files and archives
EN4.db.zip.src <- dir(EN4.db.dir,pattern="zip$",full.names = TRUE)
EN4.db.zip <- lapply(EN4.db.zip.src,function(f) data.frame(src=f,unzip(f,list=TRUE))) %>%
                  bind_rows() %>% as_tibble() %>%
              select(src,db.fname=Name)
EN4.nc.src <- dir(EN4.db.dir,pattern="nc$",full.names = TRUE)
EN4.db <- rbind(EN4.db.zip,
                data.frame(src=EN4.nc.src,
                           db.fname=basename(EN4.nc.src))) %>%
          mutate(src.mtime=file.mtime(src))

#Extract date-time stamp and test for duplicates
EN4.db <- add_column(EN4.db,date=ymd(gsub(".*\\.([0-9]{6})\\.nc$$","\\115",EN4.db$db.fname)),.before = 1)
if(any(duplicated(EN4.db$date))) stop("Duplicate entries in EN4 database")

#Add in previous extractions
EN4.db <- mutate(EN4.db,
                 ex.fname=file.path(EN4.ex.dir,db.fname),
                 ex.exists=file.exists(ex.fname),
                 ex.mtime=file.mtime(ex.fname))

#Compare databases
EN4.to.process <- filter(EN4.db,!ex.exists | src.mtime > ex.mtime )
n.to.process <- nrow(EN4.to.process) 

if(n.to.process==0) stop("No data to process.")

#'========================================================================
# Process EN4 files ####
#'========================================================================
#Loop over files
pb <- progress_estimated(n.to.process)
log.msg("Processing %i files...\n",n.to.process)

for(i in seq(n.to.process)) {
  pb$tick()$print()

  f <- EN4.to.process[i,]
#  log.msg("Now processing %s...",basename(f$file.name))
  
  #Decompress contents of ZIP archive, if necessary
  #Preliminary files are provided as gzipped files, which is not supported natively from inside
  #R. These files should be decompressed by hand prior to running this script. 
  #Contents of ZIP files are extract here

  #Decompress the individual files inside the archive
  if(grepl(".*zip$",f$src)) {
    unzip(f$src,files = f$db.fname,exdir = tmp.dir)
    g <- file.path(tmp.dir,f$db.fname)
  } else {
    g <- f$src
  }
  
  #Get the layer thicknesses
  ncid <- nc_open(g)
  layer.bnds <- ncvar_get(ncid,"depth_bnds")
  layer.thickness <- layer.bnds[2,]-layer.bnds[1,]
  nc_close(ncid)
  
  #Loop over variables
  #If necessary to extract multiple variables loop, this is where we would do it
  v <- EN4.vars
  #Process using raster
  b.raw <- brick(g,varname=v,lvar=4)
  #Drop vertical layers that we don't need
  b <- b.raw[[EN4.layers]]
  #Average in the vertical
  if(wt.vertical.ave) {
    b <- weighted.mean(b,layer.thickness[EN4.layers])
  } else {
    b <- mean(b,na.rm=TRUE)
  }
  #For cases where we are shallower than the shallowest layer, we use the bottom salinity
  bottom.idx <- sum(!is.na(b.raw))
  bottom.val <- stackSelect(b.raw,bottom.idx,type="index")
  b <- cover(b,bottom.val)   #Replaces NAs in b with bottom values
  #Rotate x-axes  #Rotating can be very slow, so its best to leave it to as late as possible
  b <- rotate(b)
  #Now subset spatially
  b <- crop(b,spatial.ROI)
  #Write the raster out for further use
  writeRaster(b,filename = f$ex.fname,overwrite=TRUE)
  
  #Make some space by deleting the file
  file.remove(file.path(g))
}

#Finish off timer
pb$stop()

#==========================================================================
# Complete
#==========================================================================
#Save meta data
saveRDS(EN4.db,file=file.path(EN4.data.dir,"metadata.rds"))

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

