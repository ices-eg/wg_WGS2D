
###################################################################################################################
# Projected spawning habitat for blue whiting in April in year of interest
# if the most recent year avaliable in the 
# e.g. the most recent year where envrionmental data of March is available
# (The salinity at blue whiting's spawning depth in March is used to project the spawning habitat in April)
#
# AK Miesner [amie@aqua.dtu.dk]
# 04/07/2017
###################################################################################################################

# Setup

map_bw_habitat <- function(yr = max(year),                    
                          month = 3, # by default data from march of the most recent year that was chosen is selected
                          prob =0.4) # probability for which an isline will be plotted
{
  
  yrmon <-  paste0(yr,"0",month)
  
#====================================================================================================
# Make predictions with best model for April in year of interest
#====================================================================================================

# Load the GAM used to project the spawning habitat of blue whiting (bw.model)
#  VARI3 -> pa ~ te(latitude,doy) + s(theta, bs="cs") + s(logdepth, bs="cs") + s(Sspawn, bs="cs")
load("Code/Output/blue_whiting_SDM.r")

# load prediction data set (created in the previous R file)
preddat <- read.csv2(paste0("Data/Output/EN4Prediction_",yrmon,"_",yrmon,".csv"))

# since a projection of the spawning distrbution of blue whiting in APRIL,
# uses the salinity at Spawning depth (Sspawn) in MARCH
# follwing might be used
preddat$Sspawn <- preddat$Sdeep   # Sspawn is the salinity at blue whting's spawning depth (Sdeep) one month prior to the observation
preddat$doy    <- 105             # April 15 is the 105th day of the year 

# MAKE PREDICTIONS ---------------------------------------------------------------------------------------

preddat.yr <- subset(preddat, year == yr)

prediction <- predict(bw.model, preddat.yr, type="response")

#  make matrix with coordinates and probabilities of the predictions
l.pred <- cbind(preddat.yr$latitude,preddat.yr$longitude,prediction) 
colnames(l.pred) <- c("lat","lon", "prob")

# make data frame of predictions
pred.df <- as.data.frame(l.pred)

# prepare data for plotting  -----------------------------------------------------------------------------

# make spatial object out of predictions
sp.pred <- pred.df
coordinates(sp.pred) <- ~ lon + lat 
crs(sp.pred) <- "+proj=longlat +datum=WGS84"

# # coerce data to SpatialPixelsDataFrame
gridded(sp.pred) <- TRUE
# transform into raster object 
r.data <- raster(sp.pred)


# set Colour Scheme & maximum probability _-------------------- ----------------------------
# maximum probability encountered in year of interest  
maxp <- round(max(pred.df$prob),1)

colour <- c("white", brewer.pal(length(seq(0.1,maxp,0.1)),"YlOrRd"))

# Etopo1 data -------------------------------------------------------------------------------

# Etopo1 Bathymetry Data 
top <- raster("Data/Output/etopo_no_land_NorthEastAtlantic.nc")

#  Etopo1 land only 
land <- raster("Data/Output/etopo_land_NorthEastAtlantic.nc")
land.col <- colorRampPalette(c("gray10", "gray88"))(40)



#==================================================================================================================
# Figure 2: 
# Projected spawning habitat for blue whiting in April YEAR X.
# The colour scale corresponds to the probability of observing blue whiting larvae by the Continuous Plankton Recorder: 
# probabilities > prob (black contour line) can be considered as the core spawning habitat.
#==================================================================================================================

png(file=paste0("Figures/2_Map_of_projected_blue_whiting_spawning_habitat_April_",yr,".png"), 
    width=55, height=110, pointsize=8.5, units="mm", res=600)

par(mfrow=c(2,1), 
    oma=c(1,2.5,0,0),  # margin of plot c(bottom, left, top, right) default:c(5, 4, 4, 2) + 0.1.
    mar=c(1.1,0.7,1,1))# margin of plot c(bottom, left, top, right) default:c(5, 4, 4, 2) + 0.1.

# par(mfrow=c(2,1),oma=c(1,3,2,0),
#     mar=c(1,0.8,1.2,3))# margin of plot c(bottom, left, top, right) default:c(5, 4, 4, 2) + 0.1.

plot(1,1,
     xlim = c(-20,0),
     ylim = c(44,65),
     type="n",
     xaxs = "i", yaxs = "i",  #xaxs = i --> exact x limits ( r usuall adds 4%)
     xaxt = "n", yaxt ="n",
     ylab = "", xlab = "")

# # plot projection of spawning habitat
plot(r.data,add=T, zlim=c(0,maxp),  # z lim ensures each plot has the same colour for the same variable (z) value
     legend=F,
     col= colour)

# bathymetry
contour(top, add = T, levels=-700,  drawlabels = F,lwd=0.5, col="grey30")

# plot CORE spawning habitat
contour(r.data, add=T, levels=prob,  drawlabels = F, lwd=1.2, col="black")

# land
plot(land, col=land.col, add=T,legend=F)


#AXIS
degAxis(1, at=seq(-40,20, by = 5))
degAxis(2, las = 2) # las   labels are parallel (=0) or perpendicular(=2) to axis
degAxis(4,  las = 2,labels = NA) 
box()

# LEGEND of Probabilities  ------------------------------------------------------------------------
plot(11,1, type = "n", axes=FALSE, xlab="", ylab="")

plot(r.data, zlim=c(0,maxp),  # z lim ensures each plot has the same colour for the same variable (z) value
     legend.only =T, horizontal=T,
     smallplot= c(0.06, 0.9, 0.8, 0.85),  # c(min % from left, max % from left, min % from bottom, max % from bottom).
     legend.width = 1, 
     col= colour,
     axis.args=list(at= seq(0,maxp,0.1),
                    labels=seq(0,maxp,0.1)),
     legend.args=list(text="Probability", side=1,line=2.5))

dev.off()

}

