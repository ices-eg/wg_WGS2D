#'========================================================================
# A2.PCA_analysis
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Mon Jan 18 07:13:06 2021
#
# Performs a PCA analysis on the predicted distribution fields, as a way of 
# characterising the variability. This is probably favourable to trying to
# calculate an area based on some threshold, for example.
#
# This work is subject to a Creative Commons "Attribution" "ShareALike" License.
# You are largely free to do what you like with it, so long as you "attribute"
# me for my contribution. See the fine print at the end for exact details.
#
# To do:
#
# Notes:
#
#'========================================================================

#'========================================================================
# Initialise system ####
#'========================================================================
cat(sprintf("\n%s\n","A2.PCA_analysis"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
  flush.console();return(invisible(NULL))}

library(raster)
library(tidyverse)
library(RSQLite)
library(here)
library(lubridate)
source("src/00.Common_elements.r")

#'========================================================================
# Configure ####
#'========================================================================

#'========================================================================
# And Go ####
#'========================================================================
#Import data
this.db <- dbConnect(SQLite(),here("data/PredEng/Blue_whiting_WGS2D.sqlite"))

SDM.res <-
  tbl(this.db,"statistics") %>%
  filter(statName=="SDM") %>%
  collect() %>%
  mutate(field=map(field,unserialize),
         mt.field=map_lgl(field,is.null)) %>%
  filter(!mt.field) %>%
  mutate(date=ymd(date),
         year=year(date),
         month=month(date))

#Merge into individual bricks for the month of interest
SDM.b <-
  SDM.res %>%
  #Subset full list
  filter(month==3,
         year %in%climatology.yrs) %>%
  #Build bricks
  group_by(spName,resultName) %>%
  summarise(brick=list(brick(field)),
            .groups="drop") 

# RasterToPCAdat function
rasterToPCAdat <- function(x) {
  t(rasterToPoints(x)[,-c(1,2)])  
}


#Apply PCA to brick
PCA.clim <-
  SDM.b %>%
  #Extract data
  mutate(dat=map(brick,rasterToPCAdat)) %>%
  #Apply PCA
  mutate(PCA=map(dat,prcomp),
         raster=map(brick,~.x[[1]])) %>%
  #Extract useful bits
  dplyr::select(spName,resultName,PCA,raster)

#Apply PCA to predicting the full time series
PCA.pred <- 
  SDM.res %>%
  left_join(y=PCA.clim) %>%
  mutate(dat=map(field,rasterToPCAdat),
         pred=map2(PCA,dat,predict))

#Reshape into a tidy format
PCA.out <- 
  PCA.pred %>%
  dplyr::select(-field,-PCA,-dat,-value,-raster) %>%
  mutate(pred=map(pred,~ .x %>% drop() %>% enframe(name="PC"))) %>%
  unnest(pred) %>%
  #Retain top three PCs
  filter(grepl("^PC[1-3]$",PC))

#'========================================================================
# Complete ####
#'========================================================================
saveRDS(PCA.out,file="objects/PCA_full.rds")
saveRDS(PCA.clim,file="objects/PCA_clim.rds")

#Turn off the lights
if(length(warnings())!=0) print(warnings())
log.msg("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,base::date())

# .............
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
# .............
