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

#==========================================================================
# Configure
#==========================================================================
#Directories
tmp.dir <- tempdir()

#EN4 configuration
EN4.db.dir <- file.path(EN4.data.dir,"database")
EN4.vars <- c("salinity")
EN4.layers <- 19:23   #Layers from the EN4 product to extract
wt.vertical.ave <- FALSE   #Do the vertical averaging with weighted means? Or simple arithmetics?

#==========================================================================
# Process EN4 files
#==========================================================================
#Get list of files
EN4.db <- data.frame(fname=dir(EN4.db.dir,pattern="zip$|nc$",full.names = TRUE))
EN4.db$year <- gsub(".*\\.([0-9]{4})[0-9]*\\..*$","\\1",EN4.db$fname)

#But we don't want to process all files - only those that are within the
#temporal Region-of-interest
EN4.to.process <- subset(EN4.db,year %in% years.ROI)
n.to.process <- nrow(EN4.to.process)

#Loop over files
for(f in EN4.to.process$fname) {
  #Identify the actin to take, based on the files  extension
  #Preliminary files are provided as gzipped files, which is not supported natively from inside
  #R. These files should be decompressed by hand prior to running this script. 
  file.ext <- gsub(".*\\.(.+?)$","\\1",basename(f))
  if(file.ext=="nc") {   
    archive.fnames <- data.frame(Name=basename(f))
  } else if(file.ext=="zip") {
    archive.fnames <- unzip(f,list = TRUE)
  } else {
    stop(sprintf('Unknown file extension, "%s", in file "%s".\n',file.ext,basename(f)))
  }
  #Get contents of archival file
  #Loop over individual files 
  for(g in archive.fnames$Name) {
    log.msg("Now processing %s...",g)
    #Decompress the individual files inside the archive
    if(file.ext=="zip") {
      unzip(f,files = g,exdir = tmp.dir)
      g.path <- file.path(tmp.dir,g)
    } else {
      g.path <- file.path(EN4.db.dir,g)
    }
    #Get the layer thicknesses
    ncid <- nc_open(g.path)
    layer.bnds <- ncvar_get(ncid,"depth_bnds")
    layer.thickness <- layer.bnds[2,]-layer.bnds[1,]
    nc_close(ncid)

    #Loop over variables
    for(v in EN4.vars) {
      log.msg("%s...",v)
      #Process using raster
      b.raw <- brick(g.path,varname=v,lvar=4)
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
      fname <- gsub("nc$",sprintf("%s.nc",v),g)
      writeRaster(b,filename = file.path(EN4.data.dir, fname),overwrite=TRUE)
    }
    #Make some space by deleting the file
    file.remove(file.path(tmp.dir,g))
    log.msg("Done.\n")
  }  
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

