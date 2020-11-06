###########################################################################
# Common_elements
# ==========================================================================
#
# by Mark R Payne  
# DTU-Aqua, Kgs. Lyngby, Denmark  
# http://www.staff.dtu.dk/mpay  
#
# Created Fri Jan  5 15:45:17 2018
# 
# Defines common variables of interest for use across the various other
# scripts. This script is intended to be source by other scripts during
# their startup phase.
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
# Helper Functions / Libraries
#==========================================================================
library(raster)

#Simple helper function to generate custom messages
log.msg <- function(fmt,...) {message(sprintf(fmt,...));
  flush.console();return(invisible(NULL))}

#Helper function to convert a raster into a dataframe, with additional
#columns provided as ... arguments
raster2df <- function(x,...){data.frame(coordinates(x),value=x[],...)}

#A windowed factorisation function, that discretises a vector x into
#a ordered factor, with levels given by the lvls argument. fmt controls
#the formatting applied to levels
discretise <- function(x,lvls,fmt,limits=c(TRUE,TRUE)) {
  nearest.lvl <- apply(abs(outer(x,lvls,"-")),1,which.min)
  lvl.str <- sprintf(fmt,lvls)
  if(limits[1]) {
      lvl.str[1] <- sprintf(paste0("<",fmt),lvls[1])
  }
  if(limits[2]) {
    lvl.str[length(lvls)] <- sprintf(paste0(">",fmt),lvls[length(lvls)])
  }
  rtn <- factor(lvl.str[nearest.lvl],lvl.str,ordered = TRUE)
  return(rtn)
}

#==========================================================================
# Configuration
#==========================================================================
#Region of interest
years.ROI <- 1950:2018
climatology.yrs <- 1960:2010
spawn.month <- 3  #Peak spawning in march, even though we observe peak larvae
                  #later in April.
spawn.depth <- c(250,600)
ts.ROI <- extent(-20,-5,50,60)  #Focus on spawning region. Note that this needs to be matched
                                #with analysis from PredEng

#Directories
EN4.data.dir <- "data/EN4"
PSY4.data.dir <- "data/PSY4V3R1/database"
pred.dir <- file.path("outputs","predictions")

#Resolution of predicted distribution
pred.res <- c(0.25,0.25)

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

