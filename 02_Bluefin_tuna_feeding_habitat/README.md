# Bluefin Tuna Summer Feeding Habitat Forecasts

WGS2D forecast product 02

## Description

Generates historical and forecasted estimates of the feeding habitat distribution of Atlantic bluefin tuna (*Thynnus thunnus*) in the North-East Atlantic based on a simple definition of thermal niche. Details of much of the analysis approach are provided in MacKenzie et al 2014.

## Data Sources / Resources

* Data extraction and processing is done externally, outside of this set of code, using the Predictability Engine  developed by Mark R. Payne. A softlink to this directory should be created in the project directory with the name "PredEng"

## Workflow

* Link in external files
* Run script src/A1.Generate_probabilistic_forecasts.r to generate probabilistic forecasts for use in the forecast sheet.
* The forecast sheet can be produced by  "knitting" the .Rmd file in the ./forecast_sheet/ directory - this is most easily done in RStudio.

## References

* MacKenzie, B. R., Payne, M. R., Boje, J., Høyer, J. L., & Siegstad, H. (2014). A cascade of warming impacts brings bluefin tuna to Greenland waters. Global Change Biology, 20(8), 2484–2491. https://doi.org/10.1111/gcb.12597

## Change Log

2018.09.18 First version