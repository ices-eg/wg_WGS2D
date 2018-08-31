saithe <- read.csv('C:/Users/Ryan/Desktop/ns_swc.csv')
saithe
setwd("saithe")
par(mfrow=c(1,1))
plot(saithe$lon,saithe$lat)
plot(saithe$time)
plot(saithe$lon)
plot(saithe$lat)
plot(saithe$Year,saithe$doy,)

install.packages("ggplot2")
install.packages("plotly")
library(plotly)
p <- plot_ly(
  x = saithe$Year, y = saithe$doy,
  z = saithe$StatRec, type = "heatmap")
  