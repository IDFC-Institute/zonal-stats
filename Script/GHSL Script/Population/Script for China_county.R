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

Zone<-readOGR("F:/Raster Data/GHSL settlement/world 1 km original/chaina","chaina_country_reproject") # To read shapesile (zone)
Lumin <- raster("F:/HDC LDC EBA wise population/world pop/China_2015_population.tif") # To read Raster data
out <- extract(Lumin, Zone, fun = sum, na.rm = T, small = T, df = T)

z <- Zone@data
M <- cbind(z,out)
write.csv(M,"F:/Raster Data/GHSL settlement/world 1 km original/chaina/zs output/2015_China_country_population.csv", na="NA") # Enter Output csv file name and path
