library(rgdal) # To import raster data
library(maptools) # To plot the data
library(proj4) # To reproject rastery
library(xtable) # To export data to html tables
library (raster) # Required for rgdal
library (rgeos) # Required for maptools
library (spatstat) # Analysing spatial point patterns
library (tiff) # Read TIFF images and required for rgdal
library (sp) #Required for maptools
library (data.table) # Modifying columns
library (modeest) #To calculate mode value for the zone
library (foreign) # Required for maptools

memory.limit(size=100000)

Lumin1 <- raster("D:/IDFC work/raja/world HDC area population zs/2000/Raster_Settlement/GHS_SMOD_POP2000_GLOBE_R2016A_54009_1k_v1_0.tif") # To read Raster data

R <- Lumin1

R[R<3] <-NA

writeRaster(R, filename="D:/IDFC work/raja/world HDC area population zs/2000/world_settlement_raster_HDC_c3_2000.tif", format="GTiff", overwrite=TRUE)

#Raster to vector

Lumin2 <- raster("D:/IDFC work/raja/world HDC area population zs/2000/world_settlement_raster_HDC_c3_2000.tif")

pp <- rasterToPolygons(Lumin2,dissolve=TRUE) #Raster to vector function

writeOGR(pp, dsn = 'D:/IDFC work/raja/world HDC area population zs/2000', layer ='world_settlement_raster_to_vector_c3_2000', driver = 'ESRI Shapefile',check_exists=T, overwrite_layer=T) # To write vector data

#clip hdc by country
Country_zone <- readOGR("D:/IDFC work/raja/world HDC area population zs/2000/shp","All_country_world_mercator_r_area_calculation_cal_area")

HDc2000_zone <- readOGR("D:/IDFC work/raja/world HDC area population zs/2000","world_settlement_raster_to_vector_c3_2000")

crop_merge_hdc2000 <- crop (Country_zone,HDc2000_zone)

writeOGR(crop_merge_hdc2000, dsn = 'D:/IDFC work/raja/world HDC area population zs/2000', layer ='clip_countrywise_hdc_shapefile_2000', driver = 'ESRI Shapefile',check_exists=T, overwrite_layer=T)




#add column area (in sqkm) in HDC
Zone2<-readOGR("D:/IDFC work/raja/world HDC area population zs/2000","clip_countrywise_hdc_shapefile_2000") # To read shapesile (zone)

Zone2@data$area_km2_1 <- gArea(Zone2, byid = TRUE) / 1000000

writeOGR(Zone2, dsn = 'D:/IDFC work/raja/world HDC area population zs/2000', layer ='clip_countrywise_hdc_shapefile_2000_cal_area', driver = 'ESRI Shapefile',check_exists=T, overwrite_layer=T)


#zonal stat HDC Area
Zone<-readOGR("D:/IDFC work/raja/world HDC area population zs/2000","clip_countrywise_hdc_shapefile_2000_cal_area") # To read shapesile (zone)

Lumin <- raster("D:/IDFC work/raja/world HDC area population zs/2000/Raster_Population/2000_pop_1km.tif") # To read Raster data

out <- extract(Lumin, Zone, fun = sum, na.rm = T, small = T, df = T)

#out[out == 0] <- NA

z <- Zone@data

M <- cbind(z,out)

write.csv(M,"D:/IDFC work/raja/world HDC area population zs/2000/world_countrywise_HDc_area_population_2000_mercator_projection.csv", na="NA") # Enter Output csv file name and path


#add column area (in sqkm) in country
Zone3<-readOGR("D:/IDFC work/raja/world HDC area population zs/2000/shp","All_country_world_mercator_r_area_calculation") # To read shapesile (zone)

Zone3@data$area_km2_1 <- gArea(Zone3, byid = TRUE) / 1000000

writeOGR(Zone3, dsn = 'D:/IDFC work/raja/world HDC area population zs/2000/shp', layer ='All_country_world_mercator_r_area_calculation_cal_area', driver = 'ESRI Shapefile',check_exists=T, overwrite_layer=T)


#zonal stat country
Zone4<-readOGR("D:/IDFC work/raja/world HDC area population zs/2000/shp","All_country_world_mercator_r_area_calculation_cal_area") # To read shapesile (zone)

Lumin4 <- raster("D:/IDFC work/raja/world HDC area population zs/2000/Raster_Population/2000_pop_1km.tif") # To read Raster data

out <- extract(Lumin4, Zone4, fun = sum, na.rm = T, small = T, df = T)

#out[out == 0] <- NA

z <- Zone4@data

M <- cbind(z,out)

write.csv(M,"D:/IDFC work/raja/world HDC area population zs/2000/world_countr_area_population_2000_mercator_projection.csv", na="NA") # Enter Output csv file name and path

