###########################################################################
# Prepare_ETOPO1_bathymetry_data
# ==========================================================================
#
# by Mark R Payne  
# DTU-Aqua, Kgs. Lyngby, Denmark  
# http://www.staff.dtu.dk/mpay  
#
# Created Mon Jan  8 12:06:41 2018
# 
# Extracts 
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
cat(sprintf("\n%s\n","Prepare_ETOPO1_bathymetry_data"))
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
#ETOPO1 directory
ETOPO1.fname <- "data/ETOPO1_Bed_c_gmt4.grd"

#==========================================================================
# Process raster
#==========================================================================
log.msg("Processing raster...\n")
#Setup raster
bath.raw <- raster(ETOPO1.fname)

#Crop - with a bit of fluff around the outside
spatial.ROI.fluff <- extend(spatial.ROI,y = c(2,2))
bath.crop <- raster::crop(bath.raw,spatial.ROI.fluff,snap="out")

#Reduce resolution
bath <- aggregate(bath.crop,fact=pred.res/res(bath.raw),fun=median)

#Remove land and zeros
bath[bath>=0] <- NA
log10bath <- log10(-1*bath)

#==========================================================================
# Process polygon
#==========================================================================
log.msg("Creating bathymetry polygon...\n")
#Re-aggregate to a relatively fine resolution, chosen arbitrarily
#in the sake of sanity e.g. 0.125
bath2 <- aggregate(bath.crop,fact=0.125/res(bath.raw),fun=median)

#Make a polygon delineating the minimum spawning depth
bath2[bath2>=0] <- NA
bath2[] <- ifelse(abs(bath2[])> min(spawn.depth),1,NA)
bath.poly <- rasterToPolygons(bath2,dissolve = TRUE)

#Save object
save(bath,log10bath,bath.poly,file="objects/bathymetry.RData")

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

