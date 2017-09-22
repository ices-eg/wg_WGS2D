
# =================================================================================================================================
# Persistence Forecast PLOTS
# to evaluate the model's predictive skill

# of the area of of suitable spawning habitat of blue whiting in the month of peak larval presence (April), 
# with suitable spawning habitat defined as a probability (p) of observing larvae with p greater or equal prob
# ==================================================================================================================================

persistence_skill_area_min_lon <- function(prob   = 0.4, # probability (p) of observing larvae with p greater or equal prob
                                           yrmon.start = 194501,     # starting yrmon index from which en4 dataset was extracted, e.g. "Data/Output/EN4Prediction_195001_201703" --> yrmon.start = 195001, yrmon.end= 201703
                                           yrmon.end)     # ending yrmon 
{

log.msg <- function(fmt,...) {cat(sprintf(fmt,...));flush.console();return(invisible(NULL))}


#===================================================================================================================================
# Make predictions for each month in each year (yrmon)
#===================================================================================================================================

# edit enironmental data used for making predictions

# remove variables that are not needed anymore...
en$X <- NULL
en$X.1<-NULL
  en$Tdeep<- NULL
  en$Tspawn<- NULL
  en$SSS<- NULL
  en$SST<- NULL

# set doy to mid april, since all predictions will be made for this day but based on the
# Salinity of different months
en$doy <- 105

# use Sdeep as Sspawn in the prediction data set
# (in this way the yrmon in dat corresponds to the date the salinity was extracted and used for modelling the spawning distribution)
en$Sspawn <- en$Sdeep
en$Sdeep <- NULL

# create column for date
en$date  <- as.Date(paste(en$year, en$month, 15, sep="-"), format= "%Y-%m-%d")
en$year  <- NULL
en$month <- NULL
#write.csv2(en, file="Data/Output/en_variables_for_persistence_forec.csv")

# load prediction data  -------------------------------------------------------------------------------------------------------------
#en   <- read.csv2("Data/Output/en_variables_for_persistence_forec.csv")
#en$X <-NULL
#en$date  <- as.Date(en$date)

# create storage for predictions ---------------------------------------------------------------------------------------------------
# library(lubridate) 
dat <- data.frame(date=seq.Date(min(ymd(en$date)),max(ymd(en$date)), by="month"))
dat$area <- NA
dat$min.lon <- NA



# Area [km2] of suitable spawning habitat of blue whiting 
# and the area's maximum westard extend (deg W)
# for every month in every year (--> yrmon)
# modelled based on S spawn (--> the salinity at spawning depth averaged over the study region) of each month
# Suitable spawning habitat is defined as a probability (p) of observing larvae with p larger than or equal to prob

# load the model for making predictions  -------------------------------------------------------------------------------------------

# Load the GAM used to project the spawning habitat of blue whiting
#  VARI3 ->pa ~ te(latitude,doy) + s(theta, bs="cs") + s(logdepth, bs="cs") + s(Sspawn, bs="cs")
load("Code/Output/blue_whiting_SDM.r")


# LOOP to
# make predictions of the size of the main spawning area (p> prob) in April --------------------------------------------------------
# for every month in every year (--> each date)

for (i in 1: nrow(dat))
  
{
  print(paste0("Now processing: ", dat$date[i],"...    "))
  
  # prediciton data set of each month in each year 
  prediction.data <- subset(en, date==dat$date[i])
  
  # make predictions
  prediction <- predict(bw.model, prediction.data, type="response")
  
  #------  make data frame with coordinates and probabilities of the predictions   -------------------------
  l.pred <- cbind(prediction.data$latitude,
                  prediction.data$longitude,
                  as.data.frame(prediction))
  
  colnames(l.pred)  <- c("lat","lon", "prob")
  
  # make spatial object out of predictions
  sp.pred              <- as.data.frame(l.pred)
  coordinates(sp.pred) <- ~ lon + lat
  crs(sp.pred) <- "+proj=longlat +datum=WGS84"

  # only probabilities of greater or equal than prob will be considered
  # remaining probabilities -> NA
  sp.pred[which(sp.pred@data$prob < prob),] <-NA
  
  # coerce spatial data of predicitons to SpatialPixelsDataFrame
  gridded(sp.pred) <- TRUE
  
  # transform into raster brick object 
  r.pred <- brick(sp.pred)
  
  # ------ Compute the approximate surface area of each pixel/cell (in km2)   ------------------------             # see link for manual calculation http://www.nhc.noaa.gov/gccalc.shtml
  size <- area(r.pred, na.rm=T)
  # remove NAs
  size.clean<- size[!is.na(size)]
  
  # calculate the approximate size of the region in km2
  # that has a probability higher/equal than 0.45
  dat$area[i] <- round(sum(size.clean),0)
  
  # ------ Compute extent of the high probability region   ---------------------------------------------
  # Trim (shrink) a Raster* object by removing outer rows and columns that all have the same value (e.g. NA).
  #  the minimum longitude / westward extent
  ifelse(dat$area[i]==0,
         dat$min.lon[i] <- NA,
         dat$min.lon[i] <- xmin(trim(size,values=NA)) )
  
}



write.csv2(dat, file=paste0("Code/Output/Area_of_probability_greater_or_equal_",prob,"_monthly_from_",yrmon.start,"_to_",yrmon.end,".csv"))



#===================================================================================================================================
# Create Forecasts
#===================================================================================================================================
#'Now we need to split our data into forecasts and observations. First the forecasts.
#'
#'In terms of forecast systems, we typically talk about three different types of time dimensions
#' * Forecast date: The time at some point into the future that the forecast is actually about
#' * Forecast initialisation: The time when the forecast model is initialised (or when the forecast is issued)
#' * Lead time: the time difference between the first two


dat <- read.csv2(paste0("Code/Output/Area_of_probability_greater_or_equal_",prob,"_monthly_from_",yrmon.start,"_to_",yrmon.end,".csv"))
dat$X <-NULL
dat$date <- as.Date(dat$date)
dat <- na.omit(dat)
#summary(dat)


#' Now we want to simulate a set of forecasts with lead times out to five years and a rolling set of initialisation dates. 
#' We can do this most easily using expand.grid
lead.times <- 0:c(12*5)  #In months: 

forecast.dat <- expand.grid(forecast.init=dat$date,lead=lead.times) 

#' Now given the lead time and the forecast.initialisation we can work out the forecast date
#' ie the date that the forecast actually refers to

forecast.dat$forecast.date <- forecast.dat$forecast.init + months(forecast.dat$lead)
#head(forecast.dat)
#tail(forecast.dat)

#' Because our forecast system is based on persistence, the value of area/MIN.LON at the time we are trying to forecast
#' is the same as the value when the forecast was initialised. We can therefore merge the time series
#' into our forecast data frame
forecast.dat <- merge(forecast.dat, dat ,by.x="forecast.init",by.y="date")
# head(forecast.dat)



#===================================================================================================================================
# Merge Observations
#===================================================================================================================================

# the suitable spawning area [in km2]  & westward extent (p>0.45) based on march salinity,
# represents the best estimate for the spawning area/werstward extend during the peak spawning month (APRIL)

# (modelled march data is used as "surrogate for observations", since it represents the best estimate of observed truth)

#'Now, lets say we are interested in the ability to forecast the March area and min.lon
#' i.e. our month of interest is 3.
#' moreover only values between 1951 - 2005 are observations since they are calibrated with CPR observations

obs.dat <- subset(dat,lubridate::month(date)==3 
                  #& year(date) %in% c(1951:2005) # uncomment to get values until 2015
)
head(obs.dat)

write.csv2(obs.dat, file=paste0("Code/Output/obs.dat_area_min.lon_probability_greater_or_equal_",prob,"_",yrmon.start,"_",yrmon.end,".csv"))


#----------------------------------------------------------------

# plot(obs.dat$date,obs.dat$area,type="l",
#      xlab= "Date",
#      ylab=expression("Area of Suitable Spawning Habitat [km"^2*"]"))
# 
# plot(obs.dat$date,obs.dat$min.lon,type="l",
#      xlab= "Date",
#      ylab="Westward Extent of Suitable Spawning Habitat [°W]")


#----------------------------------------------------------------


# create 2 data frames, one for area (A) and one for min.lon(maximum westward extent, W)
# area
obs.datA <- obs.dat
obs.datA$min.lon <-NULL
forecast.datA <- forecast.dat
forecast.datA$min.lon <-NULL 


# min.lon
obs.datW <- obs.dat
obs.datW$area <-NULL
forecast.datW <- forecast.dat
forecast.datW$area <-NULL


#' Now in our forecast data we need to know the truth/observed values as well, so
#' we merge in the truth from our observations. Note that these are the true values 
#' at the actual time that the forecast relates to i.e. forecast.date
eval.datA <- merge(forecast.datA, obs.datA,
                   by.x="forecast.date",by.y="date",
                   suffixes = c(".forecast",".obs"))
#head(eval.datA)

eval.datW <- merge(forecast.datW, obs.datW,
                   by.x="forecast.date",by.y="date",
                   suffixes = c(".forecast",".obs"))
#head(eval.datW)



write.csv2(eval.datA, file=paste0("Code/Output/eval.datA_",prob,"_",yrmon.start,"_",yrmon.end,".csv"))
write.csv2(eval.datW, file=paste0("Code/Output/eval.datW_",prob,"_",yrmon.start,"_",yrmon.end,".csv"))

#eval.datA<- read.csv2("Code/Output/eval.datA_0.4_194501_201704.csv")
#eval.datW<- read.csv2("Code/Output/eval.datW_0.4_194501_201704.csv")


# PLOT SKILL  =================================================================================================
# Persistence Forecast PLOTS
# to evaluate the models predictive skill


# Calculate COR and RMSE between observation and hindcast ----------------------------------
# to plot skill as a function of lead time. 



rmse.cor.funct <- function(data){  # data is a data frame  containing values of observations and forecast for different lead times
  
  #  make a list where each element in data corresponds to a lead time using split()
  dat <- split(data, data$lead)
  
  #storage for cor &b rmse
  skill<- data.frame(lead=c(0:60),cor=NA,rmse=NA)
  
  # calculate COR between observation and forecast at different lead times
  skill$cor <- sapply(dat,function(d) {
    cor(d$var.obs,d$var.forecast,method = "pearson")})
  
  # ... and the RMSE
  skill$rmse <- sapply(dat,function(d) {
    rmse(d$var.obs,d$var.forecast)})
  
  return(skill)
  
}



# calulate COR and RMSE for AREA ----------------------------------------------------------
# # data containing forecast and observations of the suitable spawning area of BW

eval.datA$var.obs <- eval.datA$area.obs 
eval.datA$var.forecast<-eval.datA$area.forecast
area.skill <- rmse.cor.funct(data=eval.datA) 

# calulate COR and RMSE for the westward extend of the suitable area
#  min.lon ---------------------------------------------------------------------------------
eval.datW$var.obs <- eval.datW$min.lon.obs 
eval.datW$var.forecast<-eval.datW$min.lon.forecast
minlon.skill <- rmse.cor.funct(data=eval.datW) 




#================================================================================================
# plot skill as a function of lead time:
# Pearson Correlation between the observed area of suitable spawning of 
# blue whiting and the ensemble mean hindcast for different lead times.
#================================================================================================

 

# Since the sampling distribution of Pearson's r is not normally distributed,
# Pearson's r is converted to Fisher's z' 
# and the confidence interval is computed using Fisher's z'
fisher.r2z <- function(r) { 1/2 * log((1+r)/(1-r))}  # in R log is the natural logarithm (base e)
# same as same as psych::fisherz(area.skill$cor)
z.A <- fisher.r2z(r=area.skill$cor)

#standard error of the sampling distribution of z' 
n <- length(z.A)                                             
se.A <- 1/sqrt(n-3)

conf1 <- z.A + se.A*1.96 #  1.96 in the case of a 95% confidence interval
conf2 <- z.A - se.A*1.96

# translate from z-space back to r-space
upper <- (exp(2*conf1)-1) / (exp(2*conf1)+1)   # same as psych::fisherz2r(conf1)
lower <- (exp(2*conf2)-1) / (exp(2*conf2)+1)


# find out where the distribution (e.g. area.skill$cor is significantly different from 0 (i.e greater than 0))
# using a 95% confidence interval
sign <- 0 + 1/sqrt(n-3)*1.96 #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  Please check this, Mark...

# # same as .....................
# library(psych)
# n<- nrow(area.skill)
# r <- area.skill$cor
# test <- r.test(n=n, r12= r) #Correlation to be tested) 
# 
# r.rc <- data.frame(r=r,
#                    z=fisherz(r),
#                    lower=test$ci[1:n],
#                    upper=test$ci[(n+1):(2*n)],
#                    t=test$t,
#                    p=test$p)
# 
# upper <- r.rc$upper
# lower <- r.rc$lower



# Pearson Correlation coefficient ----------------------------------------------------------

# FIGURE - monthly oceanographic persistence
minmax <- c(0,1)


png(file="Figures/3_A_5yr_Persistence_Forecast_cor_skill.png",  
    width=170, height=80, pointsize=10, units="mm", res=300)


# PLOT
par(mfrow=c(1,2),
    mar=c(3,2.8,1.5,0.01), # margin of plot c(bottom, left, top, right) default:c(5, 4, 4, 2) + 0.1.
    oma=c(1,1,1,1), # outer margin of text
    mgp=c(2, 0.6, 0), cex=1)# margin of axis title default:  c(3, 1, 0) c(lab, axis nr, lines)


#A) AREA - COR --------------------------------------------------------------

# hindcast experiment
plot(area.skill$lead,area.skill$cor,
     type="l",ylim=minmax,
     ylab="Pearson correlation",xlab="Lead time [year]",
     main="Area",
     xaxt="n",
     las=2,
     lwd=2,
     yaxs="i",
     col="darkblue")
axis(1, at=c(0,12,24,36,48,60), labels=c("0","1","2","3","4","5"))

grid(nx=NULL, ny=NULL,lty="solid",lwd=0.05)


# upper & lower 95% confidence interval 
# lines(y=upper , x= area.skill$lead, lty="dotted", col="darkblue", lwd=1) 
# lines(y=lower , x= area.skill$lead, lty="dotted", col="darkblue", lwd=1) 

# add a line with the upper 95% confidence interval 
 abline(h= sign , lty="dotted",lwd=2)


mtext("a)", adj=1)

#B) westward extend COR -----------------------------------------------------


# hindcast experiment
plot(minlon.skill$lead,minlon.skill$cor,
     type="l",ylim=minmax,
     yaxt="n",ylab="",
     # ylab="Pearson correlation",
     xlab="Lead time [year]",
     main="Westward Extent",
     xaxt="n",las=2,
     lwd=2,
     yaxs="i",
     col="darkblue")
axis(1, at=c(0,12,24,36,48,60), labels=c("0","1","2","3","4","5"))

grid(nx=NULL, ny=NULL,lty="solid",lwd=0.05)

# add a line with the upper 95% confidence interval
abline(h= sign , lty="dotted",lwd=2)


mtext("b)", adj=1)


legend("topright",
       c("Persistence Forecast", "Significance (95%)"), #5 % significance level
       lty=c("solid","dotted"),
       col=c("darkblue",  "black"),
       text.col =c("darkblue",  "black"),
       box.lwd = 0, box.col = "white",cex=1,inset=0.005,lwd=2)


dev.off()



# one sample t-test
# Since only positive correlations indicate predictive skill, the statistical significance is computed using a one-sided t-test.
# and the upper confidence interval will be plotted
tA <- t.test(area.skill$rmse,conf.level = 0.95)

tW <- t.test(minlon.skill$rmse,conf.level = 0.95)


png(file="Figures/3_B_5yr_Persistence_Forecast_RMSE_skill.png", 
    width=170, height=80, pointsize=10, units="mm", res=300)


# PLOT
par(mfrow=c(1,2),
    mar=c(3,2.8,1.5,0.01), # margin of plot c(bottom, left, top, right) default:c(5, 4, 4, 2) + 0.1.
    oma=c(1,2,1,1), # outer margin of text
    mgp=c(2, 0.6, 0), cex=1)# margin of axis title default:  c(3, 1, 0) c(lab, axis nr, lines)


#A) AREA --------------------------------------------------------------

# hindcast experiment
plot(area.skill$lead,area.skill$rmse,
     type="l",
     ylab="",xlab="Lead time [year]",
     main="Area",
     xaxt="n",
     las=2,
     lwd=2,
     yaxs="i",
     col="darkblue")
axis(1, at=c(0,12,24,36,48,60), labels=c("0","1","2","3","4","5"))

mtext("Root Mean Square Error (RMSE)", side=2, line=3.5)

grid(nx=NULL, ny=NULL,lty="solid",lwd=0.05)


# add a line with the upper 95% confidence interval (dashed line in acf plot)
## +/- 2/sqrt(N));  where N is the sample size
abline(h= tA$conf.int[[2]] , lty="dotted",lwd=2)


mtext("a)", adj=1)


legend("bottomright",
       c("Persistence Forecast", "Significance (95%)"), #5 % significance level
       lty=c("solid","dotted"),
       col=c("darkblue",  "black"),
       text.col =c("darkblue",  "black"),
       box.lwd = 0, box.col = "white",cex=1,inset=0.005,lwd=2)

#B) westward extend COR -----------------------------------------------------

# hindcast experiment
plot(minlon.skill$lead,minlon.skill$rmse,
     type="l",
     ylab="",
     xlab="Lead time [year]",
     main="Westward Extent",
     xaxt="n",las=2,
     lwd=2,
     yaxs="i",
     col="darkblue")
axis(1, at=c(0,12,24,36,48,60), labels=c("0","1","2","3","4","5"))

grid(nx=NULL, ny=NULL,lty="solid",lwd=0.05)

# add a line with the upper 95% confidence interval (dashed line in acf plot)
## +/- 2/sqrt(N));  where N is the sample size
abline(h= tW$conf.int[[2]] , lty="dotted",lwd=2)


mtext("b)", adj=1)



dev.off()



}











