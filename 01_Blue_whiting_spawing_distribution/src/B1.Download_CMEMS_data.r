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
start.time <- proc.time()[3];

#Helper functions, externals and libraries
source("src/00.Common_elements.r")
library(tidyverse)
library(lubridate)
library(raster)
library(RCMEMS)
library(magrittr)
library(here)

#==========================================================================
# Configure
#==========================================================================
global.raster <- readRDS(here("objects/global_ROI.rds"))

#Time series script
cfg.scripts <- list()
cfg.scripts$PSY4 <- 
  paste("python <PATH_TO_MOTUCLIENT_DIR>/motu-client.py --user <USERNAME> --pwd <PASSWORD> ",
        "--motu http://nrt.cmems-du.eu/motu-web/Motu --service-id GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS ",
        "--product-id global-analysis-forecast-phy-001-024-monthly --longitude-min -180 ",
        "--longitude-max 179.9166717529297 --latitude-min -80 --latitude-max 90 ",
        #--date-min "2018-11-16 00:00:00" --date-max "2018-11-16 00:00:00" 
        "--depth-min 0.493 --depth-max 0.4942 --variable so --out-dir <OUTPUT_DIR> --out-name <OUTPUT_FILENAME>")

#CORIOLIS is on an irregular latitudinal grid, so it doesn't play very nicely with the rest of the processing chain
#which is based on the raster package
cfg.scripts$CORIOLIS.NRT <-
  paste("python -m motuclient",
        "--motu http://nrt.cmems-du.eu/motu-web/Motu --service-id INSITU_GLO_TS_OA_NRT_OBSERVATIONS_013_002_a-TDS",
        "--product-id CORIOLIS-GLOBAL-NRTOA-OBS_TIME_SERIE --longitude-min -180 --longitude-max 179.5",
        "--latitude-min -77.0104751586914 --latitude-max 89.8962631225586",
        #--date-min "2020-02-15 00:00:00" --date-max "2020-02-15 00:00:00"
        "--depth-min -1 --depth-max 1 --variable PSAL",
        "--out-dir <OUTPUT_DIRECTORY> --out-name <OUTPUT_FILENAME> --user <USERNAME> --pwd <PASSWORD>")

cfg.scripts$CORIOLIS.REP <-
  paste("python -m motuclient",
        "--motu http://my.cmems-du.eu/motu-web/Motu --service-id INSITU_GLO_TS_OA_REP_OBSERVATIONS_013_002_b-TDS",
        "--product-id CORIOLIS-GLOBAL-CORA-OBS_FULL_TIME_SERIE --longitude-min -180 --longitude-max 179.5",
        "--latitude-min -77.0104751586914 --latitude-max 89.8962631225586",
        #--date-min "2018-11-15 00:00:00" --date-max "2018-11-15 00:00:00"
        "--depth-min 0 --depth-max 2  --variable PSAL",
        "--out-dir <OUTPUT_DIRECTORY> --out-name <OUTPUT_FILENAME> --user <USERNAME> --pwd <PASSWORD> ")

cfg.scripts$ARMOR3D.NRT <- 
  paste("python <PATH_TO_MOTUCLIENT_DIR>/motu-client.py --user <USERNAME> --pwd <PASSWORD> ",
        "--motu http://nrt.cmems-du.eu/motu-web/Motu --service-id MULTIOBS_GLO_PHY_NRT_015_001-TDS ",
        "--product-id dataset-armor-3d-nrt-weekly --longitude-min 0.125 --longitude-max -0.125 ",
        "--latitude-min -82.125 --latitude-max 89.875 ",
        #--date-min "2019-02-27 00:00:00" --date-max "2019-02-27 00:00:00" 
        "--depth-min -1 --depth-max 1 --variable so --out-dir <OUTPUT_DIR> --out-name <OUTPUT_FILENAME>")

cfg.scripts$ARMOR3D.REP <- 
  paste("python <PATH_TO_MOTUCLIENT_DIR>/motu-client.py --user <USERNAME> --pwd <PASSWORD> ",
        "--motu http://my.cmems-du.eu/motu-web/Motu --service-id MULTIOBS_GLO_PHY_REP_015_002-TDS ",
        "--product-id dataset-armor-3d-rep-monthly --longitude-min 0.125 --longitude-max -0.125 ",
        "--latitude-min -82.125 --latitude-max 89.875 ",
        #--date-min "2017-11-15 00:00:00" --date-max "2017-11-15 00:00:00" 
        "--depth-min -1 --depth-max 1 --variable so --out-dir <OUTPUT_DIR> --out-name <OUTPUT_FILENAME>")

cfg.scripts$GLO.CPL <- 
  paste("python -m motuclient",
        "--motu http://nrt.cmems-du.eu/motu-web/Motu --service-id GLOBAL_ANALYSISFORECAST_PHY_CPL_001_015-TDS",
        "--product-id MetO-GLO-PHY-CPL-dm-SAL --longitude-min 0 --longitude-max -0.25",
        "--latitude-min -83 --latitude-max 89.75",
        #--date-min "2020-11-14 12:00:00" --date-max "2020-11-14 12:00:00" 
        "--depth-min -1 --depth-max 1 --variable so --out-dir <OUTPUT_DIRECTORY> --out-name <OUTPUT_FILENAME>",
        "--user <USERNAME> --pwd <PASSWORD> ")

#==========================================================================
# Download time series data 
#==========================================================================
#Parse scripts
CMEMS.cfgs <- lapply(cfg.scripts,parse.CMEMS.script)

#Download
for(mdl.name in names(cfg.scripts)) {
  log.msg("Now downloading %s...\n",mdl.name)
  #Setup
  ts.cfg <- CMEMS.cfgs[[mdl.name]] <- 
    update(CMEMS.cfgs[[mdl.name]],
           script=as.character(NA),
           depth.min="-1",
           depth.max="1000",
           out.dir=file.path("data",mdl.name,"database"))
  
  #Check that directory exists
  if(!dir.exists(ts.cfg@out.dir)) dir.create(ts.cfg@out.dir,recursive = TRUE)
  
  #Get the list of available time steps for this product
  timesteps <- product.description(ts.cfg,"times",quiet=TRUE)
  ts.raw <- 
    tibble(raw=str_split(timesteps,",",simplify = TRUE)[1,]) %>%
    tidyr::separate(raw,c("start","end","duration"),sep = "/",remove=FALSE) %>%
    mutate(end=ifelse(is.na(end),start,end),
           start=ymd_hms(start),
           end=ymd_hms(end)) %>%
    tidyr::extract(duration,remove=FALSE,
            c("n.years","n.months","n.days","n.hours","n.minutes","n.seconds","n.millisecs"),
            "^P([0-9]+Y)?([0-9]+M)?([0-9]+D)?T?([0-9]+H)?([0-9]+M)?([0-9]+S)?\\.?([0-9]+)?$") %>%
    mutate_at(vars(starts_with("n.")),function(x) as.numeric(gsub("[[:alpha:]]","",x))) %>%
    mutate_at(vars(starts_with("n.")),function(x) ifelse(is.na(x),0,x)) %>%
    mutate(int=days(365*n.years+30*n.months+n.days)+hours(n.hours)+minutes(n.minutes)+seconds(n.seconds),
           int.seconds=seconds(int))%>%
    rowwise() %>%
    mutate(dates=list(seq(start,end,by=int.seconds)))
  ts.l <- as.Date(do.call(c,ts.raw$dates))
  
  #Create a list of metadata based on the available timesteps
  db.meta <- tibble(date=ts.l,
                    src=file.path(ts.cfg@out.dir,sprintf("%s_%s.nc",mdl.name,format(date,"%Y%m%d"))),
                    src.exists=file.exists(src))
  
  #Select missing timesteps
  missing.db <- filter(db.meta,!src.exists)
  n.missing <- nrow(missing.db)
  if(n.missing==0) {
    log.msg("All data present.")
  } else {
    #Loop over individual timesteps
    for(i in seq(n.missing)) {
      f <- missing.db[i,]
      log.msg("Downloading %s...\n",f$date)
      ts.cfg <- update(ts.cfg,out.name=basename(f$src))
      CMEMS.download(ts.cfg,
                     ROI=extent(global.raster),
                     date.min=f$date,date.max=f$date)
    }
  }

  #Save meta data
  db.meta$src.exists <- NULL
  saveRDS(db.meta,file=file.path("data",mdl.name,"database_metadata.rds"))

}

#Save 
saveRDS(CMEMS.cfgs,file="objects/CMEMS_cfgs.rds")

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

