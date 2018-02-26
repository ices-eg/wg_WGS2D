###########################################################################
# Extract_metrics_from_predicted_distributions
# ==========================================================================
#
# by Mark R Payne  
# DTU-Aqua, Kgs. Lyngby, Denmark  
# http://www.staff.dtu.dk/mpay  
#
# Created Mon Jan  8 16:02:37 2018
# 
# Processes predicted distributions and extracts key metrics (e.g. westward
# extension, area) from them
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
cat(sprintf("\n%s\n","Extract_metrics_from_predicted_distributions"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
  flush.console();return(invisible(NULL))}

source("src/00.Common_elements.r")
library(reshape2)

#==========================================================================
# Configure
#==========================================================================
#Thresholds
high.dens.thresh <- 0.4   #Choosen arbitrarily by Anna
core.percentiles <- seq(0.1,0.9,by=0.1)    #Core area based on xx% percentile

#==========================================================================
# Iterate over files
#==========================================================================
#Get list of prediction files
pred.fnames <- dir(pred.dir,full.names = TRUE,pattern="nc$")

#Setup results storage
res.l <- list()

#Loop over files
for(p in pred.fnames){
  log.msg("Now processing %s...\n",basename(p))
  res <- list(fname=basename(p))
  
  #Import predicion
  p.b <- raster(p)
  area.pxl <- area(p.b)
  
  #Threshold-based approach
  #-------------------------
  #Identify high spawning probability region
  thresh.core <- p.b > high.dens.thresh

  #Get area of high density region
  res$thresh.area <- cellStats(thresh.core * area.pxl,stat = "sum")
  
  #Get westward extent
  res$thresh.westward <-  min(xFromCell(thresh.core,which(thresh.core[])))
  
  #Based on the the "core" xx%
  #----------------------------
  #Identify core spawning area based on percentage of total spawning activity
  spawn.intens.pxl <- area.pxl*p.b
  spawn.intens.sort <- rev(sort(spawn.intens.pxl[]))
  spawn.intens.norm <- cumsum(spawn.intens.sort)/sum(spawn.intens.sort)
  spawn.intens.thresh.idx <- approx(spawn.intens.norm,seq(spawn.intens.norm),
                                xout = core.percentiles,method = "constant")$y
  ptile.cores.l <-  lapply(spawn.intens.thresh.idx, function(i) {
    spawn.intens.pxl > spawn.intens.sort[i]
  } )
  names(ptile.cores.l) <- sprintf("p=%0.2f",core.percentiles)
  ptile.cores <- brick(ptile.cores.l)
  
  #Area of the core
  res$ptile.area <- cellStats(ptile.cores*area.pxl,stat="sum")

  #Westerward extent we base on the meridonal sums
  merid.dist <- colSums(spawn.intens.pxl*ptile.cores,na.rm=TRUE)
  merid.cumdist <- apply(merid.dist,2,function(x) cumsum(x)/sum(x))
  merid.exceed.thresh.idx <- pmax(1,colSums(merid.cumdist < 0.1))  #If out of bounds, use boundary
  res$ptile.westward <- xFromCol(p.b,merid.exceed.thresh.idx)

  #Store results
  res$core.percentiles <- core.percentiles
  res.l[[p]] <- as.data.frame(res)
}

#Combine results
metrics <- do.call(rbind,res.l)
rownames(metrics) <- NULL
metrics$year <- as.numeric(gsub(".*([0-9]{4})[0-9]{2}.*","\\1",metrics$fname))
metrics$month <- as.numeric(gsub(".*[0-9]{4}([0-9]{2}).*","\\1",metrics$fname))
metrics$date <- with(metrics,ISOdate(year,month,15))

#Save results
save(metrics,file="objects/metrics.RData")

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

