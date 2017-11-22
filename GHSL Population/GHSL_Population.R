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

Zone<-readOGR("F:/Original SHP/563_town_boundary","AllIndiaClass1_563_TownBoundaries2014_15_reproject") # To read shapesile (zone)
Lumin <- raster("F:/Raster Data/GHSL population/India 250 m original/India_2015_pop/India_2015_pop_250m.tif") # To read Raster data
out <- extract(Lumin, Zone, fun = sum, na.rm = T, small = T, df = T)
#out[out == 0] <- NA
z <- Zone@data
M <- cbind(z,out)
write.csv(M,"F:/Raster Data/GHSL population/India 250 m original/zs/zonal_stat_GHSL_250m_pop_2015_town_Boundary.csv", na="NA") # Enter Output csv file name and path


