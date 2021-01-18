###########################################################################
# Generate_oceanographic_timeseries
# ==========================================================================
#
# by Mark R Payne  
# DTU-Aqua, Kgs. Lyngby, Denmark  
# http://www.staff.dtu.dk/mpay  
#
# Created Wed Feb 28 17:02:50 2018
# 
# Generates time series of oceanographic data averaged over a region
# of interest.
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
cat(sprintf("\n%s\n","Generate_oceanographic_timeseries"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
source("src/00.Common_elements.r")
library(tidyverse)
library(tibble)
library(stringr)
library(lubridate)
library(here)
library(sf)
library(here)

#==========================================================================
# Setup file lists
#==========================================================================
#Import configuration
CMEMS.cfgs <- readRDS("objects/CMEMS_cfgs.rds")

#Get list of files for each source
meta.df <- 
  tibble(name=names(CMEMS.cfgs),
         dat.dir=map_chr(CMEMS.cfgs,slot,"out.dir"),
         flat.dir=file.path(dat.dir,"..","flattened"),
         fname=map(flat.dir,dir,full.names=TRUE,pattern="nc$")) %>%
  unnest(fname) %>%
  mutate(date.str=str_extract(basename(fname),"[0-9]{8}"),
         date=ymd(date.str)) %>%
  select(-dat.dir,-flat.dir,-date.str) %>%
  separate(name,c("model","type"),sep="\\.",remove=FALSE) 

#==========================================================================
# Process files
#==========================================================================
#Setup polygon to delineate depth regions
bath.poly <- readRDS(here("objects/bathymetry_poly_spawn.rds"))

#Loop over files
meta.df$salinity <- as.double(NA)
pb <- progress_estimated(nrow(meta.df))
for(i in seq(nrow(meta.df))) {
  pb$print()

  #Setup raster
  r <- raster(meta.df$fname[i])
  
  #Mask with bathymetry and ROI
  r.masked <- 
    r %>%
    mask(bath.poly) %>%
    crop(ts.ROI)

  #Perform averaging
  area.r <- area(r.masked)
  area.norm <- cellStats(area.r*!is.na(r.masked),sum)
  mean.val <- cellStats(area.r*r.masked,sum)/area.norm
  
  #Store
  meta.df$salinity[i] <- mean.val
  
  pb$tick()

}

meta.df %>%
  saveRDS(file=here("objects/ocean_data.rds"))

ggplot(meta.df,aes(x=date,y=salinity,colour=name))+
  geom_point()+
  facet_wrap(~name)

#'========================================================================
# Extract snapshots ####
#'========================================================================
#Add in EN4
mr.EN4.dat <- 
  readRDS(here("objects/PredEng_MR_EN4_salinity_field.rds"))%>%
  mutate(date=ymd(date)) %>%
  select(model=srcName,date,field)
  

#Get most recent data
mr.dat <- 
  meta.df %>%
  group_by(model) %>%
  filter(date==max(date)) %>%
  mutate(field=map(fname,raster)) %>%
  select(model,date,field) %>%
  bind_rows(mr.EN4.dat)

saveRDS(mr.dat,file=here("objects/Salinity_snapshots.rds"))

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

