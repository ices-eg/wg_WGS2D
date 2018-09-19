#'========================================================================
# A1.Generate_probabilistic_forecasts
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Tue Sep 18 21:45:21 2018
#
# Generates probabilistic forecasts
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
cat(sprintf("\n%s\n","A1.Generate_probabilistic_forecasts"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
  flush.console();return(invisible(NULL))}

library(tidyverse)
library(raster)

#'========================================================================
# Configure ####
#'========================================================================
#Directories
src.dir <- "PredEng"
bluefin.dir <- file.path(src.dir,"scratch","Bluefin")
obs.dir <- file.path(bluefin.dir,"Observations","HadISST")
NMME.dir <- file.path(bluefin.dir,"NMME")

#SST Threshold
habitat.thresh <- 11

#'========================================================================
# Setup ####
#'========================================================================
#Get list of NMME forecasts metadata
anom.meta.fnames <- dir(NMME.dir,pattern="Anom_metadata.RData",
                   recursive = TRUE,full.names = TRUE)

#Load metadata
anom.meta.l <- lapply(anom.meta.fnames,function(f) {
              load(f)
              return(anom.meta)})
anom.meta <- bind_rows(anom.meta.l)

#Find the most recent forecast
MR.start <- max(anom.meta$start.date)
MR.forecasts <- subset(anom.meta,start.date==MR.start) %>%
                mutate(fname=file.path(src.dir,fname))

#Setup observational climatology
load(file.path(obs.dir,"Z.Misc.meta/Climatology_metadata.RData"))
clim.r <- raster(file.path(src.dir,clim.meta$fname))

#'========================================================================
# Process forecasts ####
#'========================================================================
#Convert the anomaly forecasts into SST forecasts
sst.b.l <- lapply(MR.forecasts$fname,function(f) {
                  brick(f)+clim.r})

#Convert SSTs into probability 
prob.l <- lapply(sst.b.l,function(b) {
            suit.hab <- b>habitat.thresh
            hab.prob <- mean(suit.hab)
            return(hab.prob)
})

#Average over models
prob.b <- brick(prob.l)
prob.forecast <- mean(prob.b,na.rm=TRUE)

#Save results
save(prob.forecast,clim.r,file="outputs/Prob_forecast.RData")

#'========================================================================
# Complete ####
#'========================================================================
#Turn off the lights
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
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
