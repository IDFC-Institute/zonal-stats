library(rgdal)
library(maptools)
library(lattice)
library(proj4)
require(raster)
library(geoR)
library(xtable)
library(fields)
require(gstat)
require(spatstat)
require(tiff)
require(sp)
require(doBy)
require(data.table)
require(modeest)
require(foreign)

Zone<-readOGR("F:/Original SHP/original shp","Dist_2001") # To read shapesile (zone)
Lumin <- raster("F:/Raster Data/GPW population/Count/gpw-v4-population-count-2020/gpw-v4-population-count_2020.tif") # To read Raster data
out <- extract(Lumin, Zone, fun = sum, na.rm = T, small = T, df = T)
#out[out == 0] <- NA
z <- Zone@data
M <- cbind(z,out)
write.csv(M,"F:/Raster Data/GPW population/Count/ZS_output/zonal_stat_GPW_2020_count_Dist_2001.csv", na="NA") # Enter Output csv file name and path

