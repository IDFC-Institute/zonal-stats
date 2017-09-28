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
Lumin <- raster("F:/Raster Data/worlpop - india population/popmap15.tif") # To read Raster data
out <- extract(Lumin, Zone, fun = sum, na.rm = T, small = T, df = T)
#out[out == 0] <- NA
z <- Zone@data
M <- cbind(z,out)
write.csv(M,"F:/Raster Data/worlpop - india population/worlpop_15_Dist2001.csv", na="NA") # Enter Output csv file name and path

