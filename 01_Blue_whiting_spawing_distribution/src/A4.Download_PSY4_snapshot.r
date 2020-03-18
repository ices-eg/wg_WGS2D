###########################################################################
# Download PSY4 snapshot
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
cat(sprintf("\n%s\n","Download_PSY4_snapshot"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
source("src/00.Common_elements.r")
library(tidyverse)
library(lubridate)
if(!require(RCMEMS)){
  stop(paste("This function requiers the RCMEMS package to download data from Copernicus.",
             "It can be installed using the following command:\n",
             'devtools::install_github("markpayneatwork/RCMEMS")\n',
             "Stopping."))
}
library(stringr)

#==========================================================================
# Configure
#==========================================================================
#Model name
mdl.name <- "PSY4V3R1"

#Snapshot script of the most recent value
#snapshot.script <- 'python <PATH_TO_MOTUCLIENT_DIR>/motu-client.py --user <USERNAME> --pwd <PASSWORD> --motu http://nrtcmems.mercator-ocean.fr/motu-web/Motu --service-id GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS --product-id global-analysis-forecast-phy-001-024 --longitude-min -180 --longitude-max 179.91667175293 --latitude-min -80 --latitude-max 90 --date-min "2018-03-01 12:00:00" --date-max "2018-03-01 12:00:00" --depth-min 222.4751 --depth-max 763.3333 --variable so -out-dir <OUTPUT_DIR> --out-name <OUTPUT_FILENAME> '
snapshot.script <- paste("python <PATH_TO_MOTUCLIENT_DIR>/motu-client.py --user <USERNAME> --pwd <PASSWORD> ",
                     "--motu http://nrt.cmems-du.eu/motu-web/Motu --service-id GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS ",
                     "--product-id global-analysis-forecast-phy-001-024-monthly --longitude-min -180 ",
                     "--longitude-max 179.9166717529297 --latitude-min -80 --latitude-max 90 ",
                     #--date-min "2018-11-16 00:00:00" --date-max "2018-11-16 00:00:00" 
                     "--depth-min 0.493 --depth-max 0.4942 --variable so --out-dir <OUTPUT_DIR> --out-name <OUTPUT_FILENAME>")

snapshot.date <- as.Date("2020-02-15")

#Misc
motu.client <- "resources/motuclient-python/motuclient.py"

#==========================================================================
# Download snapshot 
#==========================================================================
#Setup RCMEMS objects
snp.cfg <- parse.CMEMS.script(snapshot.script)
snp.cfg <- update(snp.cfg,
                  
                  out.dir=file.path(PSY4.data.dir,"database"),
              out.name=sprintf("%s_snapshot.nc",mdl.name),
              script=motu.client,
              depth.min="Surface",depth.max="1000")

#Download snapshot
CMEMS.download(snp.cfg,ROI=spatial.ROI,
               date.range=c(snapshot.date,snapshot.date+1)) #Bracket the time in qns

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

