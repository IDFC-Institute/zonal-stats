library(rgdal) # To import raster data
library(maptools) # To plot the data
library(proj4) # T reproject the shapefile
require(raster) # Required for rgdal
library(xtable) # To export data to html tables
require(spatstat) # To analyse spatial point pattern
require(tiff) # Required for rgdal
require(sp) # Required for maptools
require(doBy) # To calculate groupwise statistics
require(data.table) # To modify columns
require(modeest) # To calculate mode value for the zone
require(foreign) # Required for maptools
require(rgeos) # Required for maptools

memory.limit(size=100000)

## To read town boundaries of India in shapefile format and assign to variable "zone1"
Zone1<-readOGR("D:/IDFC work/raja/GDP/SHP","AllIndiaClass1_563_TownBoundaries2014_15_reproject")  
plot(Zone1)

## To read Market exchange rate(MER) of world in raster format and assign to variable MER1
MER1 <- raster("D:/IDFC work/raja/GDP/Dataset/Yale_GEcon_(sedac)/MER/mer1990sum.tif")
plot(MER1)

## Assign the projection of MER data to a variable "raster_proj"
raster_proj <- MER1@crs
raster_proj

## Reproject zone1 to the projection of MER
Zone1<-spTransform(Zone1,raster_proj)
plot(Zone1)

## Similarly read Market exchange rate and Purchasing power parity(PPP) for 1990 to 2005 in raster format and assign to unique variables
MER2 <- raster("D:/IDFC work/raja/GDP/Dataset/Yale_GEcon_(sedac)/MER/Mer1995sum_data_more_than_zero.tif")
MER3 <- raster("D:/IDFC work/raja/GDP/Dataset/Yale_GEcon_(sedac)/MER/Mer2000sum_data_more_than_zero.tif")
MER4 <- raster("D:/IDFC work/raja/GDP/Dataset/Yale_GEcon_(sedac)/MER/Mer2005sum_data_more_than_zero.tif")

PPP1 <- raster("D:/IDFC work/raja/GDP/Dataset/Yale_GEcon_(sedac)/PPP/ppp1990sum.tif")
PPP2 <- raster("D:/IDFC work/raja/GDP/Dataset/Yale_GEcon_(sedac)/PPP/PPP1995sum_data_more_than_zero.tif")
PPP3 <- raster("D:/IDFC work/raja/GDP/Dataset/Yale_GEcon_(sedac)/PPP/PPP2000sum_data_more_than_zero.tif")
PPP4 <- raster("D:/IDFC work/raja/GDP/Dataset/Yale_GEcon_(sedac)/PPP/PPP2005sum_data_more_than_zero.tif")

## Calculate the total MER and PPP for all towns in India 
out1 <- extract(MER1, Zone1, fun = sum, na.rm = T, small = T, df = T)
out2 <- extract(MER2, Zone1, fun = sum, na.rm = T, small = T, df = T)
out3 <- extract(MER3, Zone1, fun = sum, na.rm = T, small = T, df = T)
out4 <- extract(MER4, Zone1, fun = sum, na.rm = T, small = T, df = T)
out5 <- extract(PPP1, Zone1, fun = sum, na.rm = T, small = T, df = T)
out6 <- extract(PPP2, Zone1, fun = sum, na.rm = T, small = T, df = T)
out7 <- extract(PPP3, Zone1, fun = sum, na.rm = T, small = T, df = T)
out8 <- extract(PPP4, Zone1, fun = sum, na.rm = T, small = T, df = T)

z1 <- Zone1@data
MER_output <- cbind(z1,out1,out2,out3,out4)
PPP_output <- cbind(z1, out5,out6,out7,out8)
write.csv(MER_output,"D:/IDFC work/raja/GDP/Output/MER_output.csv", na="NA") # Enter Output csv file name and path
write.csv(PPP_output,"D:/IDFC work/raja/GDP/Output/PPP_output.csv", na="NA") # Enter Output csv file name and path