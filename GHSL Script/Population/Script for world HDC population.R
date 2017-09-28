library(rgdal) 
library (raster)
library (tiff)
library(rgdal)
library(raster)
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
require(rgeos)
require(PBSmapping)


#Export raster c3

memory.limit(size=100000)

Lumin1 <- raster("D:/Raja/#R_Script/GHSL_R_Script/1990/Raster_settlement/GHS_SMOD_POP1990_GLOBE_R2016A_54009_1k_v1_0.tif") # To read Raster data

R <- Lumin1

R[R<3] <-NA

writeRaster(R, filename="D:/Raja/#R_Script/GHSL_R_Script/1990/world_settlement_raster_HDC_c3_1990.tif", format="GTiff", overwrite=TRUE)


#Raster to vector

Lumin2 <- raster("D:/Raja/#R_Script/GHSL_R_Script/1990/world_settlement_raster_HDC_c3_1990.tif")

pp <- rasterToPolygons(Lumin2,dissolve=TRUE) #Raster to vector function

writeOGR(pp, dsn = 'D:/Raja/#R_Script/GHSL_R_Script/1990', layer ='world_settlement_raster_to_vector_c3_1990', driver = 'ESRI Shapefile',check_exists=T, overwrite_layer=T) # To write vector data


###### Change Projection for raster to vector HDC shapefile

##HDc2000_zone <- readOGR("D:/Raja/#R_Script/GHSL_R_Script/1990","world_settlement_raster_to_vector_c3_1990")

##projection_mercator:change projection code

##writeOGR(projection_mercator, dsn = 'D:/Raja/#R_Script/GHSL_R_Script/1990', layer ='world_settlement_raster_to_vector_c3_1990_mercator', driver = 'ESRI Shapefile',check_exists=T, overwrite_layer=T)



#clip hdc by country
Country_zone <- readOGR("D:/Raja/#R_Script/GHSL_R_Script/1990/SHP","All_country_world_mercator")

HDc2000_zone <- readOGR("D:/Raja/#R_Script/GHSL_R_Script/1990","world_settlement_raster_to_vector_c3_1990_reproject_mercator")

crop_merge_hdc2000 <- crop (Country_zone,HDc2000_zone)

writeOGR(crop_merge_hdc2000, dsn = 'D:/Raja/#R_Script/GHSL_R_Script/1990', layer ='clip_countrywise_hdc_shapefile_1990_reproject_mercator', driver = 'ESRI Shapefile',check_exists=T, overwrite_layer=T)


#add column area (in sqkm) in HDC
Zone2<-readOGR("D:/Raja/#R_Script/GHSL_R_Script/1990","clip_countrywise_hdc_shapefile_1990_reproject_mercator") # To read shapesile (zone)

Zone2@data$area_km2_1 <- gArea(Zone2, byid = TRUE) / 1000000

writeOGR(Zone2, dsn = 'D:/Raja/#R_Script/GHSL_R_Script/1990', layer ='clip_countrywise_hdc_shapefile_1990_cal_area_reproject_mercator', driver = 'ESRI Shapefile',check_exists=T, overwrite_layer=T)


#zonal stat HDC Area
Zone<-readOGR("D:/Raja/#R_Script/GHSL_R_Script/1990","clip_countrywise_hdc_shapefile_1990_cal_area_reproject_mercator") # To read shapesile (zone)

Lumin <- raster("D:/Raja/#R_Script/GHSL_R_Script/1990/Raster_Population/1990_pop_1km.tif") # To read Raster data

out <- extract(Lumin, Zone, fun = sum, na.rm = T, small = T, df = T)

#out[out == 0] <- NA

z <- Zone@data

M <- cbind(z,out)

write.csv(M,"D:/Raja/#R_Script/GHSL_R_Script/1990/world_countrywise_HDc_area_population_1990_reproject_mercator_final.csv", na="NA") # Enter Output csv file name and path


#add column area (in sqkm) in country
Zone3<-readOGR("D:/Raja/#R_Script/GHSL_R_Script/1990/SHP","All_country_world_mercator_r_area_calculation") # To read shapesile (zone)

Zone3@data$area_km2_1 <- gArea(Zone3, byid = TRUE) / 1000000

writeOGR(Zone3, dsn = 'D:/Raja/#R_Script/GHSL_R_Script/1990/SHP', layer ='All_country_world_mercator_r_area_calculation_cal_area', driver = 'ESRI Shapefile',check_exists=T, overwrite_layer=T)


#zonal stat country
Zone4<-readOGR("D:/Raja/#R_Script/GHSL_R_Script/1990/SHP","All_country_world_mercator_r_area_calculation_cal_area") # To read shapesile (zone)

Lumin4 <- raster("D:/Raja/#R_Script/GHSL_R_Script/1990/Raster_Population/1990_pop_1km.tif") # To read Raster data

out <- extract(Lumin4, Zone4, fun = sum, na.rm = T, small = T, df = T)

#out[out == 0] <- NA

z <- Zone4@data

M <- cbind(z,out)

write.csv(M,"D:/Raja/#R_Script/GHSL_R_Script/1990/world_country_area_population_1990_mercator_projection.csv", na="NA") # Enter Output csv file name and path

