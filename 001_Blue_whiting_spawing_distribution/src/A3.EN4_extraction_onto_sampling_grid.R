###########################################################################################################
#  Extract EN4 data for regularly spaced 0.25 deg sampling grid
###########################################################################################################
# This Script extracts the variables defined in settings for the data defined in data.file.path
# for each year-month combination of the extr.data (here a regularly spaced sampling grid)
# and saves them wihin the extr.data (sampling grid)

# the Output will serve as the prediction/ calibration data set for the SDM

# References
#   Good, S. A., M. J. Martin and N. A. Rayner, 2013. EN4: quality controlled ocean temperature and salinity profiles 
#   and monthly objective analyses with uncertainty estimates, 
#   Journal of Geophysical Research: Oceans, 118, 6704-6716, doi:10.1002/2013JC009067
#  
#   Viktor Gouretski and Franco Reseghetti, 2010: On depth and temperature biases in bathythermograph data:
#   development of a new correction scheme based on analysis of a global ocean database.
#    Deep-Sea Research I, 57, 6. doi: http://dx.doi.org/10.1016/j.dsr.2010.03.011
###########################################################################################################

# Setup

extractEN4_fun <- function(data.file.path = "Data/EN4.2.0/EN.4.2.0.f.analysis.g10.")
  
{
  
  log.msg <- function(fmt,...) {cat(sprintf(fmt,...));flush.console();return(invisible(NULL))}
  
  #############################################################################################
  # SETUP
  #############################################################################################
  
  
  # Choose Envrionmental Data
  #-----------------------------------------------------------------------------------------------
  # insert the common name of data files containing the environmental parameters
  
  # for EN4.2.0 
  
  
  # create list with settings of the data files containing the environmental parameters
  # here variable names of EN and the data 
  
  settings <- list(list(variable = "temperature",              dat= c("SST", "Tdeep")),
                   list(variable = "salinity",                 dat= c("SSS", "Sdeep")))
  
  
  # Choose DATA for MATCH-UP 
  #---------------------------------------------------------------------------------------------
  extr.data   <- read.csv2("Code/Output/sampling_grid.csv")
  extr.data$X <- NULL
  
  # make space for data to be stored within extr.data
  #temperature
  extr.data$SST<- NA ; extr.data$Tdeep<- NA 
  
  # "salinity"                      
  extr.data$SSS<- NA ; extr.data$Sdeep<- NA 
  
  #############################################################################################
  # Start loop for making Match-Ups
  #############################################################################################
  
  #-----------------------------------------------------------------------------------------------
  #---------- Match-Up extr.data with each variables within settings ------------------------
  #---------- at the same point in space and time as extr.data ------------------------------
  #-----------------------------------------------------------------------------------------------
  
  
  # first loop enters each of the variables of interest (within the settings list)
  for (j in 1:length(settings))
    {
      #-------Create List of Data Files---------------------------------------------------------------
      # create vector with unique year month combinations that are within the extraction dataset/CPR data
      yrmon <- sort(unique(extr.data$yrmon))
      
      #create list of all file directories of EN4
      en.l <- list()
      
      #-------Loop each yrmon combination found within extr.data----------------------------------------
      
      for (i in 1:length(yrmon))
        { en.l[[i]] <-  paste0(data.file.path,yrmon[i],".nc")  }
      
      
      #-------Loop each Variable within Settings-----------------------------------------------------
      
     log.msg("Processing %s ...", i=settings[[j]]$variable)

      for (i in 1:length(en.l))
        {
        
          #-------Setup Data--------------------------------------------------------------------------
          en.file <- en.l[[i]]
          en <- nc_open(en.file)
          dmp <- nc_close(en)      # removes names from envrionment
          
          tID <-as.numeric(substr(en$filename,38,43))
          
          log.msg("... %s in %s 
                  ", i=settings[[j]]$variable, i=tID)
          
          #-------Create  Brick ---------------------------------------------------------------------
          
          en.data <- brick(en.file,     # loop over all EN files
                           varname= settings[[j]]$variable,
                           lvar= 4,           # (default=3) --> if the file has 4 dimensions -> lvar=4 
                           level= 1) 
          
          en.data <- crop(en.data,c(360 + min(extr.data$longitude)-1, 
                                    360 + max(extr.data$longitude)+1,
                                    min(extr.data$latitude)-1, 
                                    max(extr.data$latitude)+1))
          
          #-------Choose Subset of Extr.Data and Match-UP Coordinate System--------------------------
          
          #Subset extraction
          extr.data.index <- which(extr.data$yrmon==tID)  # Which rows are the yrmon that we want?
          extr.data.set   <- extr.data[extr.data.index,]
          
          # en.data has coordinates from 0 - 360 
          # adjust extr.data (ranging from -180 to +180) to match en-datas geographic representation
          extr.data.set$en.lon <- 360+extr.data.set$longitude
          
          # make spatial points object of extraction data point subset incl en.  coordinates
          extr.data.sp <- extr.data.set                         # copy extr.data
          coordinates(extr.data.sp) <- ~ en.lon + latitude  # transfroms it into spatial points object
          crs(extr.data.sp) <- crs(en.data)
          
          #-------Extract Data--------------------------------------------------------------------
          
          # extract data from en.data at each extraction point by means of bilinear interpolation      
          res <- raster:: extract(en.data, extr.data.sp, method="bilinear")  
          
          # depth/vertical level for data extraction
          # note that the vertival layers correspond to to the midpoint of the level 
          # (it's not exactly the midpoint as the levels were decided when being brought over from a slightly different gridding system). 
          # Depth_bnds will tell you which observations are incorporated into each level, 
          # so, for example, the first level takes any observations from 0 to 10.05m
          
          # data.frame of mean temperatures within spawning depth 
          # spawning depth ,  EN (level 19:23 (271-541 m) -> actual depth bound = 252-596m
          average <- data.frame(res[,19],res[,20],res[,21],res[,22],res[,23])
          average$mean <- rowMeans(average, na.rm = T) 
          
          # if depth of ocean is shallower than 252 m 
          # -> use temperature recorded at deepest point 
          shallow <- data.frame(res[,1],res[,2],res[,3],res[,4],res[,5],res[,6],res[,7],res[,8],res[,9],
                                res[,10],res[,11],res[,12],res[,13],res[,14],res[,15],res[,16],res[,17],res[,18])
          
          for (i in 1:nrow(extr.data.set))
          {
            idx <- sum(!is.na(shallow[i,])) # which is the last column with a value (not na)
            idx<- ifelse(idx==1,  2, idx-1) # incase there is no data keep NA
            shallow$mean[i] <- shallow[i,idx]
           }
        
        # combine Mean Temp data frames, 
        # if depth of ocean is shallower than 252 m m --> NAs in  average$mean  
        # --> use T til bottom of sea 
        na.idx <- which(is.na(average$mean)) # which rows have nas = shallower that 252 m 
        average$mean[na.idx] <- shallow$mean[na.idx] # add mean T from surface to bottom of sea
        
        # values from sea surface to 596 m
        entire <- data.frame(res[,1],res[,2],res[,3],res[,4],res[,5],res[,6],res[,7],res[,8],res[,9],
                             res[,10],res[,11],res[,12],res[,13],res[,14],res[,15],res[,16],res[,17],
                             res[,18],res[,19],res[,20],res[,21],res[,22],res[,23])
        entire$mean <- rowMeans(entire, na.rm = T) 
        
        
        #--------Add results to CPR/Extraction Data frame-------------------------------------------------------
        
        # SST / SSS/ uncertainty at sea surface 
        col.idx <- which(colnames(extr.data)== settings[[j]]$dat[1]) # SST / SSS/ uncertainty at surface
        extr.data[extr.data.index,col.idx] <- res[,1]  #  extracts T of upper layer -> EN (level 1 = 5 m)
        
        #Tdeep / Sdeep/ uncertainty at depth (average between 252-596m
        col.idx <- which(colnames(extr.data)== settings[[j]]$dat[2]) 
        extr.data[extr.data.index, col.idx] <- average$mean 
        
      }
  }
  
  
  #====================================================================================================
  
  # --------remove points on land -----------------------------------------------------------------------
  # a plot of NA values showed that all NAs are lying on top of land
  # hence they were removed:
  extr.data <- na.omit(extr.data)
  
  
  # --------Transfrom T data from Kelvin to Celcius-------------------------------------------------------
  
  extr.data$SST    <-extr.data$SST   -273.15 
  extr.data$Tdeep  <-extr.data$Tdeep -273.15 
  
  
  
  # -------- Vaiables during BW Spawning -----------------------------------------------------------------
  # are defined as the variables found one month prior to the actual observation
  # extr.data$Tspawn        <- NA
  # extr.data$Sspawn        <- NA
  # 
  
  years <- unique(extr.data$year)
  
  for (j in 2: length(years))
  {
    #print(years[j])
    for (i in 1) # in January
    {
      sub <- subset(extr.data, month==12 & year==years[j]-1)
      
      Jidx <- which(extr.data$year==years[j] & extr.data$month==1)
      extr.data[Jidx,]$Tspawn <- sub$Tdeep
      extr.data[Jidx,]$Sspawn <- sub$Sdeep
      
    }
    
    for (i in 2: length(unique(extr.data$month))) # during all other month
    {
      sub <- subset(extr.data, month==i-1 & year==years[j])
      
      idx <- which(extr.data$year==years[j] & extr.data$month== i)
      extr.data[idx,]$Tspawn <-sub$Tspawn
      
      extr.data[idx,]$Tspawn <- sub$Tdeep
      extr.data[idx,]$Sspawn <- sub$Sdeep
      extr.data[idx,]$density.spawn <- sub$density.deep
    }
    
  }
  
  
  
  
  #############################################################################################
  # --------Save Output-------------------------------------------------------
  #############################################################################################

  #save
  write.csv2(extr.data, file=paste0("Data/Output/EN4Prediction_",min(extr.data$yrmon),"_",max(extr.data$yrmon),".csv"))
  
  # review output data file
  return(summary(extr.data))

}
