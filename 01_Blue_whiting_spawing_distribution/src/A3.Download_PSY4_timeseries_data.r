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
  stop(paste("This function requiers the RCMEMS package to download data from Copernicus.",
            "It can be installed using the following command:\n",
            'devtools::install_github("markpayneatwork/RCMEMS")\n',
            "Stopping."))
}
library(stringr)

#==========================================================================
# Configure
#==========================================================================
#Time series script
PSY4.script <- paste("python <PATH_TO_MOTUCLIENT_DIR>/motu-client.py --user <USERNAME> --pwd <PASSWORD> ",
                     "--motu http://nrt.cmems-du.eu/motu-web/Motu --service-id GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS ",
                     "--product-id global-analysis-forecast-phy-001-024-monthly --longitude-min -180 ",
                     "--longitude-max 179.9166717529297 --latitude-min -80 --latitude-max 90 ",
                     #--date-min "2018-11-16 00:00:00" --date-max "2018-11-16 00:00:00" 
                     "--depth-min 0.493 --depth-max 0.4942 --variable so --out-dir <OUTPUT_DIR> --out-name <OUTPUT_FILENAME>")
  
CORIOLIS.OA.script <- paste("python <PATH_TO_MOTUCLIENT_DIR>/motu-client.py --user <USERNAME> --pwd <PASSWORD> ",
                         "--motu http://nrt.cmems-du.eu/motu-web/Motu --service-id INSITU_GLO_TS_OA_NRT_OBSERVATIONS_013_002_a-TDS ",
                         "--product-id CORIOLIS-GLOBAL-NRTOA-OBS_TIME_SERIE --longitude-min -180 --longitude-max 179.5",
                         "--latitude-min -77.0104751586914 --latitude-max 89.8962631225586 ",
                         #--date-min "2018-09-15 00:00:00" --date-max "2018-09-15 00:00:00" 
                         "--depth-min -1 --depth-max 1 --variable PSAL --out-dir <OUTPUT_DIR> --out-name <OUTPUT_FILENAME>")


#"python <PATH_TO_MOTUCLIENT_DIR>/motu-client.py --user <USERNAME> --pwd <PASSWORD> --motu http://my.cmems-du.eu/motu-web/Motu --service-id INSITU_GLO_TS_OA_REP_OBSERVATIONS_013_002_b-TDS --product-id CORIOLIS-GLOBAL-CORA05.0-OBS_FULL_TIME_SERIE --longitude-min -180 --longitude-max 179.5 --latitude-min -77.0104751586914 --latitude-max 89.8962631225586 --date-min "2015-11-15 00:00:00" --date-max "2015-11-15 00:00:00" --depth-min -1 --depth-max 1 --variable PSAL --out-dir <OUTPUT_DIR> --out-name <OUTPUT_FILENAME>
  

ts.scripts <- list("PSY4V3R1"=PSY4.script,CORIOLIS.OA=CORIOLIS.OA.script)
  
#Misc
motu.client <- "objects/motu-client-python/motuclient.py"

#==========================================================================
# Download time series data 
#==========================================================================
#Parse scripts
CMEMS.cfgs <- lapply(ts.scripts,parse.CMEMS.script)


for(mdl.name in names(ts.scripts)) {
  log.msg("Now downloading %s...\n",mdl.name)
  #Setup
  ts.cfg <- update(CMEMS.cfgs[[mdl.name]],
                   depth.min="Surface",
                   script=motu.client,
                   out.dir=file.path("data",mdl.name,"database"),
                   date.max=NULL,date.min=NULL)
  
  CMEMS.download(ts.cfg,ROI=spatial.ROI,debug=TRUE)
  
  
  # #Get the list of available time steps for this product
  # timesteps <- product.description(ts.cfg,"times")
  # 
  # #See what is already available in the database
  # db.fnames <- dir(PSY4.data.dir,full.names = TRUE,pattern="nc$")
  # db.meta <- tibble(fname=db.fnames,
  #                   date=ymd(str_extract(db.fnames,"[0-9]{8}")))
  # 
  # #Select missing timesteps
  # missing.timesteps <- subset(timesteps,!as.Date(timesteps) %in% db.meta$date )
  # 
  # 
  # #Loop over individual timesteps
  # for(t in missing.timesteps) {
  #   output.fname <- sprintf("%s_%s.nc",mdl.name,format(as.Date(t),"%Y%m%d"))
  #   ts.cfg <- update(ts.cfg,out.name=output.fname)
  #   CMEMS.download(ts.cfg,ROI=spatial.ROI,date.min=t,date.max=t)
  # }
}


#==========================================================================
# Complete
#==========================================================================
#Save 

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

