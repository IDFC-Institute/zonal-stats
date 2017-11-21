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

memory.limit(size=100000)

Zone<-readOGR("D:/world pop and settlement 2015 ZS/world shp","All_country_world_mollweide") # To read shapesile (zone)

Lumin <- raster("D:/world pop and settlement 2015 ZS/raster/2015_pop_1km.tif") # To read Raster data

out <- extract(Lumin, Zone, fun = sum, na.rm = T, small = T, df = T)

out[out == 0] <- NA

z <- Zone@data


M <- cbind(z,out)


write.csv(M,"D:/world pop and settlement 2015 ZS/zs output/world_countrywise_population_2015.csv", na="NA") # Enter Output csv file name and path
