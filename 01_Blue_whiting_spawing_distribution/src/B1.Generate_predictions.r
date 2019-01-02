###########################################################################
# Generate_predictions
# ==========================================================================
#
# by Mark R Payne  
# DTU-Aqua, Kgs. Lyngby, Denmark  
# http://www.staff.dtu.dk/mpay  
#
# Created Mon Jan  8 11:46:58 2018
# 
# Generates a predicted Blue whiting distribution for a given oceanographic
# situation
#
# This work is subject to a Creative Commons "Attribution" "ShareALike" License.
# You are largely free to do what you like with it, so long as you "attribute" 
# me for my contribution. See the fine print at the end for exact details.
#
# To do:
#
# Notes:
#  * This script depends on the presence of a fitted species distribution model
#    in the ./models/ directory. The file is not included in the repository due
#    to its large size, so if you don't have it, it can be obtained by
#    contacting WGS2D directly.
#
###########################################################################

#==========================================================================
# Initialise system
#==========================================================================
cat(sprintf("\n%s\n","Generate predictions"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
  flush.console();return(invisible(NULL))}

source("src/00.Common_elements.r")

#==========================================================================
# Configure
#==========================================================================
#External files
mdl.fname <- "models/GAM_model.RData"
bath.fname <- "objects/bathymetry.RData"

#Prediction variables
pred.consts <- expand.grid(doy=seq(30,180,by=1),             
                      theta=0)            #Solar angle corresponds to sunrise/sunset
pred.sel.doy <- c(105) # April 15 is the 105th day of the year 

#==========================================================================
# Setup for predictions
#==========================================================================
#Load externals
load(mdl.fname)
load(bath.fname)

#Get list of EN4 files to predict for
files.df <- data.frame(salinity=dir(EN4.data.dir,pattern=".*salinity.*",full.names=TRUE))
files.df$pred.fname <- file.path(pred.dir,sprintf("pred.%s",basename(files.df$salinity)))
files.df$year <- gsub("^.*\\.([0-9]{4})[0-9]{2}\\..*$","\\1",basename(files.df$pred.fname))

#But only process those that are missing from the output
process.files <- subset(files.df,!file.exists(files.df$pred.fname) & year %in% years.ROI)
if(nrow(process.files)==0) {
  stop("No files to process!")
}

#Create a latitude raster
lat.rast <- log10bath
lat.rast[] <- yFromCell(lat.rast,1:ncell(log10bath))

#==========================================================================
# Loop over files
#==========================================================================
for(i in seq(nrow(process.files))) {
  f <- process.files[i,]
  log.msg("Now making predictions for %s...\n",basename(f$pred.fname))
  #Import file
  sal.b.raw <- raster(f$salinity)
  #Adjust resolution to the prediction scale - should probably be done in the
  #extraction phase if this is too slow, but lets just leave it for the moment
  sal.b <- disaggregate(sal.b.raw,fact=res(sal.b.raw)/pred.res,method="bilinear")
  
  #Crop everything down to the same size, and collate into one object
  pred.dat <- brick(list(latitude=crop(lat.rast,sal.b),
                         logdepth=crop(log10bath,sal.b),
                         Sspawn=sal.b))

  #Now, we're ready to predict. Loop over day of year
  pred.l <- list()
  for(i in seq(nrow(pred.consts))) {
    pred.l[[i]] <- predict(pred.dat, bw.model,const=pred.consts[i,],type="response")
  }
  
  #Process results
  sel.doy.idx <- which(pred.consts$doy==pred.sel.doy)
  sel.b <- pred.l[[sel.doy.idx]]
  pred.b <- brick(pred.l)
  rtn <- brick(list(mean=mean(pred.b),max=max(pred.b),sel=sel.b))  
  crs(rtn) <-"+proj=longlat"

  #Save output to temp file
  writeRaster(rtn,file=f$pred.fname,overwrite=TRUE,bylayer=TRUE,suffix=names(rtn))
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

