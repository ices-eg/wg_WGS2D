#'========================================================================
# B4.Smooth_oceanographic_data
#'========================================================================
#
# by Mark R Payne
# DTU-Aqua, Kgs. Lyngby, Denmark
# http://www.staff.dtu.dk/mpay
#
# Created Mon Nov  2 08:07:46 2020
#
# Smooths the oceanographic timeseries by applying GAM models to remove any
# seasonal cycle
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
cat(sprintf("\n%s\n","B4.Smooth_oceanographic_data"))
cat(sprintf("Analysis performed %s\n\n",base::date()))
start.time <- proc.time()[3];

#Helper functions, externals and libraries
log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
  flush.console();return(invisible(NULL))}

library(tidyverse)
library(lubridate)
library(mgcv)

#'========================================================================
# Setup ####
#'========================================================================
ref.date <- as.Date("1990-01-01")

#Import data and tweak
ocean.dat <- 
  readRDS("objects/ocean_data.rds") %>%
  select(-fname,-name)# %>%
  #Drop CORIOLOS.REP after 2010, as there is clearly something wrong in the file format
  #filter(!(type=="REP" & model=="CORIOLIS" &date > as.Date("2010-01-01")))

EN4.dat <- 
  readRDS(file="objects/PredEng_EN4_mean_salinity.rds") %>%
  mutate(type=as.character(NA),
         date=ymd(date)) %>%
  select(model=srcName,type,date,salinity=value)

# Merge data sources
dat.merge <-
  bind_rows(ocean.dat,EN4.dat) %>%
  group_by(model,date) %>%
  summarise(salinity=mean(salinity),
            range=max(salinity)-min(salinity)) %>%
  mutate(month=month(date),
         t=as.numeric(difftime(date,ref.date,units="days")),
         model=factor(model))
  
#'========================================================================
# And Go ####
#'========================================================================
#Fit GAM models
#Our GAMs have a seasonal component and a annually varying component
#Note that we force the seasonal component to be zero in March - all
#Salinities are therefore adjusted to be March equivalents.
mdls <- 
  dat.merge %>%
  nest(dat=-model) %>%
  mutate(mdl=map(dat,~gamm(formula=salinity ~ s(t,k=50)+s(month,k=5,bs = "cc",pc=3),
                          knots=list(month=c(0,12)),
                          family=scat,
                          #correlation = corARMA(form = ~ 1, p = 3),
                          correlation = corAR1(),
                          data=.x)),
         pred=map(mdl,~ bind_cols(predict(.x$gam,exclude="s(month)",se.fit=TRUE))))

out.dat <-
  mdls %>%
  select(-mdl) %>%
  unnest(cols=c(dat,pred)) %>%
  mutate(ubnd=fit+1.96*se.fit,
         lbnd=fit-1.96*se.fit)

most.recent.dat <- 
  out.dat %>%
  group_by(model) %>%
  filter(date==max(date))

out.dat %>%
  ggplot(aes(x=date,group=model))+
  geom_point(mapping=aes(y=salinity,colour=model))+
  geom_line(mapping=aes(y=fit))

out.dat %>%
  ggplot(aes(x=date,group=model))+
  geom_ribbon(mapping=aes(ymin=lbnd,ymax=ubnd,fill=model),alpha=0.25)+
  geom_line(mapping=aes(y=fit,colour=model))

out.dat %>%
  ggplot(aes(x=date,group=model))+
  geom_point(mapping=aes(y=salinity,colour=model),alpha=0.4)+
  geom_line(mapping=aes(y=fit,colour=model),size=1) +
  geom_point(mapping=aes(y=fit,colour=model),data=most.recent.dat,size=2)+
  geom_errorbar(mapping=aes(y=fit,ymin=lbnd,ymax=ubnd,colour=model),data=most.recent.dat,size=1,width=500)

#Alternative approach is to use one model, and share the smoothering parameters
#so that we get similar degrees of smoothness across all data sets
# mdl.grand <- 
#   gam(formula=salinity ~ s(t,by=model,id="A")+s(month,by=model,k=5,bs = "cc",pc=3,id="B"),
#       knots=list(month=c(0,12)),
#       family=scat(),
#       data=dat.merge)
# smooth.dat <-
#   dat.merge %>%
#   ungroup()%>%
#   mutate(smoothed=predict(mdl.grand,
#                           exclude=grep("month",rownames(summary(mdl.grand)$s.table),value = TRUE)))
# ggplot(smooth.dat,aes(x=date,group=model))+
#   #geom_point(mapping=aes(y=salinity,colour=model))+
#   geom_line(mapping=aes(y=salinity,colour=model))+
#   geom_line(mapping=aes(y=smoothed))

#'========================================================================
# Complete ####
#'========================================================================
saveRDS(out.dat,file="objects/Smoothed_oceanography.rds")

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
