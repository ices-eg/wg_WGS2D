###########################################################################
# Explore_metrics
# ==========================================================================
#
# by Mark R Payne  
# DTU-Aqua, Kgs. Lyngby, Denmark  
# http://www.staff.dtu.dk/mpay  
#
# Created Wed Jan 10 23:17:53 2018
# 
# Does a basic data exploration on some of the spawning area metrics to
# get a better understanding of how they depend on, for example, the 
# definition of core spawning percentile.
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
cat(sprintf("\n%s\n","Explore_metrics"))
cat(sprintf("Analysis performed %s\n\n",base::date()))

#Do house cleaning
rm(list = ls(all.names=TRUE));  graphics.off();
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
  flush.console();return(invisible(NULL))}

source("src/00.Common_elements.r")
library(ggplot2)
library(tidyr)
library(dplyr)
library(tibble)

#==========================================================================
# Configure
#==========================================================================
#Load metrics
load("objects/metrics.RData")

#==========================================================================
# Some basic visualisation plots
#==========================================================================
#Visualise the time series for each core percentile
plt.dat <- metrics %>% select(starts_with("ptile"),core.percentiles,date) %>%
            gather(variable,value,-date,-core.percentiles) %>%
            transform(core=factor(core.percentiles))
g <- ggplot(plt.dat,aes(x=date,y=value,
                        col=core,linetype=core)) +
      geom_line() +
      facet_grid(variable~.,scale="free_y")
print(g)

#Now calculate the variance of each time series for each percentile
sd.df <- tapply(plt.dat$value,plt.dat[,c("core","variable")],sd) %>%
          as.data.frame() %>%
          rownames_to_column("core")
mean.df <- tapply(plt.dat$value,plt.dat[,c("core","variable")],mean) %>%
  as.data.frame() %>%
  rownames_to_column("core")
dat.df <- merge(sd.df,mean.df,by="core",suffixes=c(".sd",".mean"))
dat.df$area.sd.norm <- dat.df$ptile.area.sd / dat.df$ptile.area.mean

#Now plot
plot(dat.df$area.sd.norm,dat.df$ptile.westward.sd,type="b",pch=NA)
text(dat.df$area.sd.norm,dat.df$ptile.westward.sd,dat.df$core,cex=0.8)


                        


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

