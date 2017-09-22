###########################################################################################################
#   Setting up a Sampling Grid with a 0.25 degree resolution (4 points per degree)
#   including the variables:
#     yrmon, month, year
#     doy (day of the year)
#     depth(in m),
#     logdepth,
#     theta (solar elevation angle, in degrees), 
# used for extracting EN4 data at each grid point within sampling.grid
###########################################################################################################

# Setup

sampling_grid_fun <- function(yrs, # Choose year(s) & month(s) for which  EN4 data shall be downloaded & extracted 
                              months=3) 
{
  
                              
  log.msg <- function(fmt,...) {cat(sprintf(fmt,...));flush.console();return(invisible(NULL))}
  
  
  # load bathymetry data (ETOPO1)
  top <- raster("Data/ETOPO 1 minute/ETOPO1_Bed_g_gmt4.grd") 
  
      # ETOPO1 Data Source:  
        # https://www.ngdc.noaa.gov/mgg/global/relief/ETOPO1/data/bedrock/grid_registered/netcdf/
        # Amante, C. and B.W. Eakins, 2009. ETOPO1 1 Arc-Minute Global Relief Model: Procedures, 
        # Data Sources and Analysis. NOAA Technical Memorandum NESDIS NGDC-24. 
        # National Geophysical Data Center, NOAA. doi:10.7289/V5C8276M.
  
  
  
  # =================================================================================================
  # Create Sampling Points within a defined geographical region
  # for each year-month combination
  # =================================================================================================
  # create sampling points with a 0.25 degree resolution (4 points per degree)
  s.er <- expand.grid(x = seq(-21, 0, by = 0.25), y = seq(44, 65, by = 0.25))
  coordinates(s.er) <- ~x + y
  
  #-------------------------------------------------------------------------------------------------
    # # PLOT of sampling grid
    #
    # plot(1,1,
    #      xlim = c(-20,-18),
    #      ylim = c(44,46),
    #      type="n",
    #      xaxs = "i", yaxs = "i",  #xaxs = i --> exact x limits ( r usuall adds 4%)
    #      xaxt = "n", yaxt ="n",
    #      ylab = "", xlab = "",cex= 0.9)
    #  
    # plot(s.er, pch="x", cex = 0.5, add=T)
  # 
  # #AXIS
  # degAxis(1, cex.axis= 0.9)
  # degAxis(2, cex.axis= 0.9, las = 2) # las   labels are parallel (=0) or perpendicular(=2) to axis
  
  
  #-------------------------------------------------------------------------------------------------
  
  # create storage for environmental data
    # for each point within the sampling grid (s.er)
    # and at each yrmon combi
    # in a new dataframe
   
  
  # --------Get Year Month Information----------------------------------------------------------------- 
  
  # create unique year-month combinations, at which data will be extracted  
  yrmon    <- merge(months,yrs)
  yrmon.id <-  as.numeric(sprintf("%i%02i",yrmon$y,yrmon$x))
  
  # create data frame 
  samplingpoints <- as.data.frame(s.er)
  
  samplingp <- list ()
  
  for (i in 1: length(yrmon.id))
  {
    samplingp[[i]] <- cbind(samplingpoints,yrmon.id[i])
    colnames(samplingp[[i]]) <- c("longitude","latitude","yrmon")
  }
  
  samplingp  <- do.call(rbind.data.frame, samplingp)
  
  samplingp$year  <- as.numeric(substr(samplingp$yrmon,1,4))
  samplingp$month <-as.numeric( substr(samplingp$yrmon,5,6))
  samplingp$yrmon <- as.numeric(samplingp$yrmon)
  
  # Day of Year  ----------------------------------------------------------------------------------------
  
  samplingp$date <- c(paste0(samplingp$yrmon,15))
  samplingp$date <-ymd(samplingp$date)
  
  samplingp$doy <- yday(samplingp$date)
  samplingp$date<-NULL
  
  
  #====================================================================================================
  # MATCH-UP Etopo Bathymetry Data to Sampling Grid
  #====================================================================================================
  
  # transform sampling points to spatial object  --------------------------------------------------------
  sampling.sp <- samplingp
  coordinates(sampling.sp) <- ~ longitude+ latitude
  crs(sampling.sp) <- "+proj=longlat +datum=WGS84"
  
  
  # crop extent to match sampling grid (with 1 degree extra at each edge to allow for bilinear interpolation at edges)
  samplingresolution <- c(min(samplingp$longitude)-1, 
                          max(samplingp$longitude)+1,
                          min(samplingp$latitude)-1, 
                          max(samplingp$latitude)+1)
  
  top.s <- crop(top,extent(samplingresolution))
  
  # remove land
  top.s[top.s>=0 ] <- 0
  NAvalue(top.s) <-0
  crs(top.s) <- "+proj=longlat +datum=WGS84"
  
  #-------depth---------------------------------------------------------------------
  # extract depth at data points of samplingp
  z <- raster::extract(top.s, sampling.sp, method = "bilinear")
  
  samplingp$depth <- z*(-1)          # make new column with depth data within samplingp
  
  # log-10-transformed depth 
  samplingp$logdepth <- log10(samplingp$depth)
  
  samplingp[which(samplingp$logdepth==-Inf),]$logdepth <-0
  samplingp[which(samplingp$logdepth<0),]$logdepth     <-0
  
  
  #====================================================================================================
  # SOLAR ELEVATION ANGLE
  #====================================================================================================
  # fixed solar elevation angle to 0 degrees -> sunrise/sunset
  samplingp$theta <- 0
  
  
  write.csv2(samplingp, file="Code/Output/sampling_grid.csv")
  
  #====================================================================================================
  #-----save file---------------------------------------------------------------------
  #====================================================================================================
  
  return(summary(samplingp))
  

}
