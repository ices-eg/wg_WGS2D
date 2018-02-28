###########################################################################
# Download_Copernicus_data
# ==========================================================================
#
# by Mark R Payne  
# DTU-Aqua, Kgs. Lyngby, Denmark  
# http://www.staff.dtu.dk/mpay  
#
# Created Wed Feb 28 11:10:04 2018
# 
# Downloads data from CMEMS for use in the Blue whiting spawning distribution
# forecasts
#
# This work is subject to a Creative Commons "Attribution" "ShareALike" License.
# You are largely free to do what you like with it, so long as you "attribute" 
# me for my contribution. See the fine print at the end for exact details.
#
# To do:
#
# Notes:
# * This script relies on the RCMEMS package, which is available from GitHub:
#   https://github.com/markpayneatwork/RCMEMS
#
###########################################################################

#==========================================================================
# Initialise system
#==========================================================================
cat(sprintf("\n%s\n","Download_Copernicus_data"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
source("src/00.Common_elements.r")
library(tidyverse)
library(lubridate)
if(!require(RCMEMS)){
  log.msg("Installing RCMEMS package...\n")
  devtools::install_github("markpayneatwork/RCMEMS")
}
library(stringr)

#==========================================================================
# Configure
#==========================================================================
#Model name
mdl.name <- "PSY4V3R1"

#Time series script
ts.script <- 'python <PATH_TO_MOTUCLIENT_DIR>/motu-client.py --user <USERNAME> --pwd <PASSWORD> --motu http://nrtcmems.mercator-ocean.fr/motu-web/Motu --service-id GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS --product-id global-analysis-forecast-phy-001-024-monthly --longitude-min -180 --longitude-max 179.91667175293 --latitude-min -80 --latitude-max 90 --date-min "2007-01-16 12:00:00" --date-max "2018-01-16 12:00:00" --depth-min 186.1255 --depth-max 763.3333 --variable so -out-dir <OUTPUT_DIR> --out-name <OUTPUT_FILENAME>'

#Misc
motu.client <- "objects/motu-client-python/motu-client.py"

#==========================================================================
# Download time series data 
#==========================================================================
#Setup
ts.cfg <- parse.CMEMS.script(ts.script)
ts.cfg <- update(ts.cfg,
                 depth.min="Surface",
                 script=motu.client,
                 out.dir=file.path(PSY4.data.dir,"database"))

#Get the list of available time steps for this product
timesteps <- product.description(ts.cfg,"times")

#See what is already available in the database
db.fnames <- dir(PSY4.data.dir,full.names = TRUE,pattern="nc$")
db.meta <- tibble(fname=db.fnames,
                    date=ymd(str_extract(db.fnames,"[0-9]{8}")))

#Select missing timesteps
missing.timesteps <- subset(timesteps,!as.Date(timesteps) %in% db.meta$date )

#Loop over individual timesteps
for(t in missing.timesteps) {
  output.fname <- sprintf("%s_%s.nc",mdl.name,format(as.Date(t),"%Y%m%d"))
  ts.cfg <- update(ts.cfg,out.name=output.fname)
  CMEMS.download(ts.cfg,ROI=spatial.ROI,date.min=t,date.max=t)
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

