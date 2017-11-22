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

Zone<-readOGR("F:/Original SHP/29 admin boundary","29_Admin_Boundary") # To read shapesile (zone)
Lumin <- raster("F:/Raster Data/worlpop - india population/popmap15.tif") # To read Raster data
out <- extract(Lumin, Zone, fun = sum, na.rm = T, small = T, df = T)
#out[out == 0] <- NA
z <- Zone@data
M <- cbind(z,out)
write.csv(M,"F:/Raster Data/worlpop - india population/worlpop_15_29municipal boundary.csv", na="NA") # Enter Output csv file name and path

