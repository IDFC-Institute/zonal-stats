![uxo_idfc_logo](https://user-images.githubusercontent.com/23652706/33058226-92f00448-ceb4-11e7-9997-7e0940f5ab1a.jpg)

**Zonal statistics for HDC Population**

Zonal statistics refers to the calculation of statistics on values of a raster within the zones of another dataset

This project is a collaborative work of UXO India and IDFC

In the following example the population of HDC [High Density Cluster] is calculated

**Required Packages**

library(rgdal) # To import raster data<br/> 
library(maptools) # To plot the data<br/> 
library(proj4)  # To reproject rastery<br/> 
library(xtable) # To export data to html tables<br/> 
library (raster) # Required for rgdal<br/> 
library (rgeos) # Required for maptools<br/> 
library (spatstat) # Analysing spatial point patterns<br/> 
library (tiff) # Read TIFF images and required for rgdal<br/> 
library (sp) #Required for maptools<br/> 
library (data.table) # Modifying columns<br/> 
library (modeest) #To calculate mode value for the zone<br/> 
library (foreign) # Required for maptools<br/> 

**Memory Limiting**

memory.limit(size=100000)<br/> 
[1] 1e+05<br/> 

**Reading the settlement raster layer**

Lumin1 <- raster("D:/K/world HDC area population zs/2000/Raster_Settlement/GHS_SMOD_POP2000_GLOBE_R2016A_54009_1k_v1_0.tif") #To read Raster data<br/> 
plot(Lumin1)<br/> 

![raster pop data](https://user-images.githubusercontent.com/23652706/33063048-49ed900e-cec7-11e7-8c09-5d3e794ad7d3.jpg)

**Assigning the raster to a variable R**

R <- Lumin1<br/> 

**Selecting the HDC (High Density crystal) layer from the settlement layer**

R[R<3] <-NA<br/> 

**Writing the HDC layer to a new raster**

writeRaster(R, filename="D:/K/output/world_settlement_raster_HDC_c3_2000.tif", format="GTiff", overwrite=TRUE)<br/> 

**Read the HDC raster**

Lumin2 <- raster("D:/K/output/world_settlement_raster_HDC_c3_2000.tif") # To read Raster data<br/> 
plot(Lumin2,col=grey(1:100/100))<br/> 

![hdc_raster](https://user-images.githubusercontent.com/23652706/33063196-c8902188-cec7-11e7-8c1c-fbd9ac508896.jpg)

**Convert Raster to Polygons**

pp <- rasterToPolygons(Lumin2,dissolve=TRUE) #Raster to vector function<br/> 
plot(pp)<br/> 

![hdc_raster_to_polygon](https://user-images.githubusercontent.com/23652706/33063452-83d3d0e8-cec8-11e7-8780-1c71af8d4872.jpg)

**Write HDC polygons to shapefile**

writeOGR(pp, dsn = 'D:/K/output', layer ='world_settlement_raster_to_vector_c3_2000', driver = 'ESRI Shapefile',check_exists=T, overwrite_layer=T) # To write vector data<br/> 

**Read the HDC settlement in vector format**

map <- readOGR("D:/K/output", layer="world_settlement_raster_to_vector_c3_2000")<br/> 

**Reproject the HDC shapefile to world mercator**

map <- spTransform(map, CRS("+init=epsg:3395"))<br/>  
plot(map)<br/> 

![hdc_vector_sp_transform](https://user-images.githubusercontent.com/23652706/33063601-f01c25d4-cec8-11e7-859d-d7ddfb4354aa.jpg)

**Write the repojected HDC shapefile to a new shapefile**

writeOGR(map, dsn = 'D:/K/output', layer ='world_settlement_raster_to_vector_c3_2000_world_meracator', driver = 'ESRI Shapefile',check_exists=T, overwrite_layer=T)<br/> 

**Read the all countries data**

Country_zone <- readOGR("D:/K/world HDC area population zs/2000/shp","All_country_world_mercator_r_area_calculation")
plot(Country_zone)<br/> 

![country_data](https://user-images.githubusercontent.com/23652706/33063799-8ed10abe-cec9-11e7-9ff5-e4614164d902.jpg)

**Read the reprojected HDC vector layer for merging fumction**

HDc2000_zone <- readOGR("D:/K/output","world_settlement_raster_to_vector_c3_2000_transfer_meracator_new")<br/> 

**Merging the all country shapefile with HDC settlement layer**

crop_merge_hdc2000 <- crop (Country_zone,HDc2000_zone)<br/> 
plot(crop_merge_hdc2000)<br/> 
![merge_hdc_country](https://user-images.githubusercontent.com/23652706/33063952-28fb7de0-ceca-11e7-8d0f-562a42eee35e.jpg)

**Write the merged data to shapefile**

writeOGR(crop_merge_hdc2000, dsn = 'D:/K/output', layer ='clip_countrywise_hdc_shapefile_2000_world_mercator_new', driver = 'ESRI Shapefile',check_exists=T, overwrite_layer=T)<br/> 

**Reading the merged data shapefile of HDC and country file**

Zone2<-readOGR("D:/K/output","clip_countrywise_hdc_shapefile_2000_world_mercator_new") # To read shapesile (zone)<br/> 

**Calculating area of zones and add that to the attribute table**

Zone2@data$area_km2_1 <- gArea(Zone2, byid = TRUE) / 1000000<br/> 

**write the edited shapefile to a new vector layer**

writeOGR(Zone2, dsn = 'D:/K/output', layer ='clip_countrywise_hdc_shapefile_2000_cal_area_world_mercator', driver = 'ESRI Shapefile',check_exists=T, overwrite_layer=T)<br/> 

**Read the edited shapefile**

Zone<-readOGR("D:/K/output","clip_countrywise_hdc_shapefile_2000_cal_area_world_mercator")<br/> 

**Read the population data**

Lumin <- raster("D:/K/world HDC area population zs/2000/Raster_Population/2000_pop_1km.tif") # To read Raster data
plot(Lumin,col=grey(1:100/100))<br/> 

![hdc_pop_raster_final](https://user-images.githubusercontent.com/23652706/33064180-d7063e16-ceca-11e7-91a8-e47da5471c13.jpg)

**Extract the values from the population data from the merged file, sum them and add it to the attribute table**
out <- extract(Lumin, Zone, fun = sum, na.rm = T, small = T, df = T)<br/> 

**Get the attribute data of merged shapefile**
z <- Zone@data<br/> 

**Combine the extract values(out) to z**
M <- cbind(z,out)<br/> 

**Wtite the bind data to csv file**

write.csv(M,"D:/k/output/world_countrywise_HDc_area_population_2000_world_mercator_final.csv", na="NA") # Enter Output csv file name and path<br/> 
