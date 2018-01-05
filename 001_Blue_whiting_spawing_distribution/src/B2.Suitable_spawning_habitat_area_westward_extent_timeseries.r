

###################################################################################################################
# Area Based Analysis of Model Predictions
# probabilities >= 0.4  are considered as the core spawning habitat
#
# AK Miesner [amie@aqua.dtu.dk] 
# 04/07/2017
###################################################################################################################


area_w.extent_bw_habitat <- function(en.data,
                                     prob = 0.4)  # area of probability greater or equal prob will be plotted
 
{
  
yrs = seq(min(en.data$year) ,max(en.data$year),1)

yrmon <- paste0(yrs,"0",3) # March of each year is chosen

#====================================================================================================
# Make predictions with best model for April of each year
#====================================================================================================

# Load the GAM used to project the spawning habitat of blue whiting
#  VARI3 ->pa ~ te(latitude,doy) + s(theta, bs="cs") + s(logdepth, bs="cs") + s(Sspawn, bs="cs")
load("Code/Output/blue_whiting_SDM.r")


# since a projection of the spawning distrbution of blue whiting in APRIL,
# uses the salinity at Spawning depth (Sspawn) in MARCH
# follwing might be used
en.data$Sspawn <- en.data$Sdeep   # Sspawn is the salinity at blue whting's spawning depth (Sdeep) one month prior to the observation
en.data$doy    <- 105             # April 15 is the 105th day of the year  


# save size of regions,
# with probabilities of greater or equal than prob
# in data frame

area.prob <- data.frame(year=yrs, size=NA, min.lon=NA, min.lat=NA,max.lat=NA)

for (i in 11:length(yrs))
{
  print(paste0("Now processing year: ", yrs[i],"........    "))
  
  # prediciton data set 
  prediction.data <- subset(en.data, year == yrs[i])
  
  # make predictions
  prediction <- predict(bw.model, prediction.data, type="response")
  
  #------  make data frame with coordinates and probabilities of the predictions   ------------------------   
  l.pred <- cbind(prediction.data$latitude,
                  prediction.data$longitude, 
                  as.data.frame(prediction)) 
  
  colnames(l.pred)  <- c("lat","lon", "prob")
  
  # make spatial object out of predictions
  sp.pred             <- as.data.frame(l.pred)
  coordinates(sp.pred)<- ~ lon + lat
  crs(sp.pred)        <- "+proj=longlat +datum=WGS84"
  
  # only probabilities of greater or equal than prob will be considered
  # remaining probabilities -> NA
  sp.pred[which(sp.pred@data$prob < prob),] <-NA
  
  # coerce spatial data of predicitons to SpatialPixelsDataFrame               
  gridded(sp.pred) <- TRUE
  
  # transform into raster brick object 
  r.pred <- brick(sp.pred)
  
  # save predictions
  #save(r.pred, file=paste0("Code/Output/predictions/prediction_APRIL_allyrs_",set,"_",yrs[i],".r"))
  
  # ------ Compute the approximate surface area of each pixel/cell (in km2)   ------------------------             # see link for manual calculation http://www.nhc.noaa.gov/gccalc.shtml
  size <- area(r.pred, na.rm=T)
  
  # calculate the approximate size of the region in km2
  # that has a probability higher/equal than prob
  area.prob$size[i] <- round(sum(size@data@values, na.rm=T),0)
  
  # ------ Compute extent of the high probability region   ---------------------------------------------
  trimmed.r <- trim(r.pred,values=NA)    # Trim (shrink) a Raster* object by removing outer rows and columns that all have the same value (e.g. NA).
  
  #  the minimum longitude / westward extent
  area.prob$min.lon[i] <- xmin(trimmed.r)    
  
  #  the minimum latitude / southward extent
  area.prob$min.lat[i] <- ymin(trimmed.r)  
  
  #  the maximum latitude / northward extent
  area.prob$max.lat[i] <- ymax(trimmed.r) 
  
}

# transfrom region to 10^5 km2
area.prob$area.small <- area.prob$size/100000

write.csv2(area.prob, file=paste0("Code/Output/Area_of_probability_greater_or_equal",prob,".csv"))

}



timeseries_bw_habitat <- function(
  yrs      = year ,
  prob     = 0.4,          # area of probability  greater or equal prob will be plotted
  plot.yrs = c(1950,2020), # time series start & end date 
  ylim.A   =  c(0.5, 3.5), # ylim for plotting the area [in 10^5 km^2]
  lim.W     = c(-12, -22))  # limit for plotting westward extent [in degrees east]
  
{
  
area.prob <- read.csv2(file=paste0("Code/Output/Area_of_probability_greater_or_equal",prob,".csv"))
  
#=====================================================================================================
# Plot A) 
# Time-series of the area of suitable spawning habitat[10^5 km2] of blue whiting 
# in the month of peak larval presence (April), with model extrapolations  indicated by the dashed line.
# Suitable spawning habitat is defined as a probability (p) of observing larvae larger or equal 0.4

# Plot B)
# Time-series of the westward extent [longitude in degrees °E] of suitable spawning habitat of blue whiting 
# in the month of peak larval presence (April), with model extrapolations  indicated by the dashed line.
# Suitable spawning habitat is defined as a probability (p) of observing larvae larger or equal 0.4

#=====================================================================================================


png(file=paste0("Figures/3_A_Timeseries_observed_BW_spawning_area_prob_greater_or_equal",prob,".png"), width=150, height=85, pointsize=10, units="mm", res=600)

# set layout -------------------------------------------------------------------------------
par(mar=c(4,4,0,3), # margin of plot c(bottom, left, top, right) default:c(5, 4, 4, 2) + 0.1.
    oma=c(0,0,1,2), # outer margin of text
    mgp=c(2.5, 1, 0), cex=0.8) # margin of axis title default:  c(3, 1, 0) c(lab, axis nr, lines)


plot(area.prob$year,
     area.prob$area.small, 
     xaxt="n",
     xaxs="i",
     yaxs="i",
     # xlab= "Date",
     xlab="",
     xlim= plot.yrs,
     ylim=ylim.A,
     ylab=expression("Potential Spawning Area [10"^5*"km"^2*"]"),
     type="n", las=2, lwd=1.2)

axis(1, at=seq(1950,2020,by=10), labels=as.character(seq(1950,2020,10)))

par(new=TRUE)

# plot "observations" -----------------------------------------------------------------
obs <- subset(area.prob, year %in% c(1951:2005))

plot(obs$year,
     obs$area.small, 
     xaxt="n",
     xaxs="i",yaxs="i",
     yaxt="n",
     xlab= "",
     xlim= plot.yrs,
     ylim= ylim.A,
     ylab="",
     type="l", las=2, lwd=1.8)

par(new=TRUE)


# model extrapolation -----------------------------------------------------------------
model.extrap <- subset(area.prob, year %in% c(1950:max(area.prob$year)))

plot(model.extrap$year,
     model.extrap$area.small,
     xaxt="n",
     xaxs="i",yaxs="i",
     yaxt="n",
     xlab= "",
     xlim= plot.yrs,
     ylim= ylim.A,
     ylab="",
     type="l", las=2, lwd=1.8,
     lty="dotted")

#legend -------------------------------------------------------------------------------
legend("bottomleft",legend = c("Model calibrated with CPR data","Model extrapolation"), 
       lty=c("solid", "dotted"),
       border = "white", bty="n") 




# WESTWARD EXTENT ===========================================================================================


png(file=paste0("Figures/3_B_Timeseries_observed_BW_spawning_areas_westward_extent__prob_greater_or_equal",prob,".png"),
    width=70, height=100, pointsize=10, units="mm", res=600)


# set layout -------------------------------------------------------------------------------
par(mar=c(4,3.5,0,0), # margin of plot c(bottom, left, top, right) default:c(5, 4, 4, 2) + 0.1.
    oma=c(0,0,1,2), # outer margin of text
    mgp=c(2.5, 1, 0), cex=0.8) # margin of axis title default:  c(3, 1, 0) c(lab, axis nr, lines)

newdata <- area.prob[order(area.prob$year,decreasing =T),] 

plot(newdata$min.lon,
     newdata$year, 
     xaxt="n",
     xaxs="i",yaxs="i",
     xlim= lim.W,
     ylim=plot.yrs,
     xlab="Westward Extent", ylab="",
     type="n", las=2, lwd=1.2)

axis(1, at=seq(-12,-22,by=-2), labels=c("12°W","14°W","16°W","18°W","20°W", "22°W"))

par(new=TRUE)

# plot "observations" -----------------------------------------------------------------
obs <- subset(newdata, year %in% c(1951:2005))

plot(obs$min.lon,
     obs$year, 
     xaxt="n",
     xaxs="i",yaxs="i",
     yaxt="n",
     xlab= "",
     xlim= lim.W,
     ylim= plot.yrs,
     ylab="",
     type="l", las=2, lwd=1.8)

par(new=TRUE)

# model extrapolation -----------------------------------------------------------------
model.extrap <- subset(newdata, year %in% c(1950:max(newdata$year)))

plot(model.extrap$min.lon,
     model.extrap$year,
     xaxt="n",
     xaxs="i",yaxs="i",
     yaxt="n",
     xlab= "",
     xlim= lim.W,
     ylim= plot.yrs,
     ylab="",
     type="l", las=2, lwd=1.8,
     lty="dotted")

dev.off()


dev.off()


}
























