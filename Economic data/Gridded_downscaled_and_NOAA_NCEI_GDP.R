############################################## load required libraries and packages 
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

################################################
memory.limit(size = 100000)

Zone<-readOGR("F:/#Economic Data/original shp","2011_Dist_Reproject") # To read shapesile (zone)
Lumin <- raster("F:/#Economic Data/Raster/Ascii/Tiff_2_Final/Mer1995sum_data_more_than_zero.tif") # To read Raster data
ptm <- proc.time()
out <- extract(Lumin, Zone, fun = sum, na.rm = T, small = T, df = T)
out[out == 0] <- NA
z <- Zone@data
M <- cbind(z,out)
write.csv(M,"F:/#Economic Data/Output2/temp3.csv", na="NA") # Enter Output csv file name and path
