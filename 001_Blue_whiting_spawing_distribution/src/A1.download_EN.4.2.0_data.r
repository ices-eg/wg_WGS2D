
#=============================================================================================================
# This script downloads & stores EN4 (EN4.2.0) Data 
# http://www.metoffice.gov.uk/hadobs/en4/download-en4-2-0.html
# 
# References
#   Good, S. A., M. J. Martin and N. A. Rayner, 2013. EN4: quality controlled ocean temperature and salinity profiles 
#   and monthly objective analyses with uncertainty estimates, 
#   Journal of Geophysical Research: Oceans, 118, 6704-6716, doi:10.1002/2013JC009067
#  
#   Viktor Gouretski and Franco Reseghetti, 2010: On depth and temperature biases in bathythermograph data:
#   development of a new correction scheme based on analysis of a global ocean database.
#    Deep-Sea Research I, 57, 6. doi: http://dx.doi.org/10.1016/j.dsr.2010.03.011
#=============================================================================================================


# Set up ======================================================================================================

downloadEN4_fun <- function(yrs,                     # Coose year(s) for which EN4 data shall be downloaded
                            en.version=  "EN.4.2.0", # Downloads EN4.2.0  from  http://www.metoffice.gov.uk/hadobs/en4/download-en4-2-0.html
                            ana = "analyses",        # Choose whether objective analyses or profiles are required (default= "analyses")
                            ref = "g10")             # Choose which correction shall be used (default= "g10" = Gouretski and Reseghetti (2010) corrections)
                                                                                                   # (...l09 =  Levitus et al. (2009) corrections )
{

# =======================================================================================================

  substr(en.version,4,4)
  substr(en.version,6,6)
  substr(en.version,6,6)
  
 "http://www.metoffice.gov.uk/hadobs/en4/data/en4-2-0/EN.4.2.0.analyses.g10.2017.zip"
  
  download.df <-data.frame(year=yrs, file=NA)
  
  for (i in 1: length(yrs)) 
  {
    download.df$file[i]<- paste0("http://www.metoffice.gov.uk/hadobs/en4/data/en4-2-0/",en.version,".",ana,".",ref,".",yrs[i],".zip")
  }
  
  #if(nrow(download.df)== 0) return("ERROR :(     The selected year cannot be downloaded automatically -> download file manually from www.metoffice.gov.uk/hadobs/en4/...  unzip the file and store within Data/EN4.2.0/")
  
  require(RCurl)
  
  for (i in 1: length(yrs)) 
  {
    en.file <- download.df$file[i]
    yr.id <- substr(en.file,53,85)
    print(yr.id)
    
    download.file(en.file, destfile = paste0(path,"/", yr.id))
    unzip(paste0(path,"/", yr.id), exdir=path)
  }
  
  # the zip folders can be deleted, when all are unpacked :)
  
  
  # in case the most recent year is not yet in the download list, 
  # check whether the preliminary version is online (right column, UPDATES) and download file manually, if it is available
  

}

