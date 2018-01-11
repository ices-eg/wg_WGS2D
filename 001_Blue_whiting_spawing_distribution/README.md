# Blue Whiting Spatial Distribution Forecasts

WGS2D forecast product 001

## Description

Generates historical and forecasted estimates of the spawning distribution of blue whiting (*Micromesistius poutassou*) in the North-East Atlantic based on a species distribution. Full details of the model are provided in Miesner and Payne (2018).

## Data Sources / Resources

* A copy of the fitted Blue whiting SDM model object, GAM_model.RData, is required to run the script and should be placed in ./models/. This file can be obtained from WGS2D if you don't have it already.
* A copy of the ETOPO1 bathymetric dataset, placed in /data/ - can be obtained from https://www.ngdc.noaa.gov/mgg/global/ - we recommend the cell-registered, bedrock product, 
* A copy of the EN4 database should be accessible - the database can be obtained by downloading it from the EN4 website, here: https://www.metoffice.gov.uk/hadobs/en4/ and should be stored in ./data/EN4/compressed/ Files can either be uncompressed, or remain in their .zip archive - however, R does not support internal decompression of gzipped files (e.g. preliminary files) and these have to be decompressed manually prior to use here.

## Workflow

Note that all scripts are based on the assumption that the working directory is set to the same directory as this README file.

## References

Miesner, A.K., and Payne, M.R. (2018) Oceanographic variability shapes the spawning distribution of blue whiting (*Micromesistius poutassou*). Fisheries Oceanography. In press.

## Change Log

2018.01.11 Rewrite of original code provided and updated to v02. 
