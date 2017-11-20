# Zonal Statistics for GDP  (Global GDP Scenarios)
 ![pic3](https://user-images.githubusercontent.com/23652706/33016652-5c672b8a-ce15-11e7-9952-001f1180e958.JPG)
 
 
 
Zonal statistics refers to the calculation of statistics on values of a raster within the zones of another dataset.  
This project is a collaborative work of UXO India and IDFC. 
In the following script sum of GDP for each town in India is calculated. 
Including Packages

library(rgdal) # To import raster data

library(maptools) # To plot the data

library(proj4) # T reproject the shapefile

require(raster) # Required for rgdal

library(xtable) # To export data to html tables

require(spatstat) # To analyse spatial point pattern

require(tiff) # Required for rgdal

require(sp) # Required for maptools

require(doBy) # To calculate groupwise statistics

require(data.table) # To modify columns

require(modeest) # To calculate mode value for the zone

require(foreign) # Required for maptools

require(rgeos) # Required for maptools
Read the GDP data of India in csv format

MyData <- read.csv(file="D:/IDFC work/raja/GDP/Dataset/SSP/CSV/gdp_ssp1.csv", 

header=TRUE, sep=",")

Read spatial coordinates from csv file to create a Spatial object

coordinates(MyData)<-~px+py

Give projection system to the csv file

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

Assign the projection system to the csv file

proj4string(MyData) <- crs.geo

Create an empty raster r
r <- raster()
Give the extent of raster as the extent of GDP
extent(r) <- extent(MyData)
extent(r)

 class       : Extent 
 xmin        : -179.9599 
 xmax        : 179.9352 
 ymin        : -59.4568 
 ymax        : 83.55961

Give the pixel size as 0.5
res(r)=0.50
r
 class       : RasterLayer 

 dimensions  : 286, 720, 205920  (nrow, ncol, ncell)

 resolution  : 0.5, 0.5  (x, y)

 extent      : -179.9599, 180.0401, -59.44039, 83.55961  (xmin, xmax, ymin, ymax)

 coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0

Creating a raster layer by summing up the GDP values in the pixel

A <- rasterize(MyData, r, fun = sum,  'g1_1980')

B <- rasterize(MyData, r, fun = sum, 'g1_1990')

C <- rasterize(MyData, r, fun = sum, 'g1_2000')

D <- rasterize(MyData, r, fun = sum, 'g1_2010')

E <- rasterize(MyData, r, fun = sum, 'g1_2020')

F <- rasterize(MyData, r, fun = sum, 'g1_2030')

G <- rasterize(MyData, r, fun = sum,  'g1_2040')

H <- rasterize(MyData, r, fun = sum, 'g1_2050')

I <- rasterize(MyData, r, fun = sum, 'g1_2060')

J <- rasterize(MyData, r, fun = sum, 'g1_2070')

K <- rasterize(MyData, r, fun = sum, 'g1_2080')

L <- rasterize(MyData, r, fun = sum, 'g1_2090')

M <- rasterize(MyData, r, fun = sum, 'g1_2100')

Read the town boundaries shapefile of India

Zone1<-readOGR("D:/IDFC work/raja/GDP/SHP","AllIndiaClass1_563_TownBoundaries2014_15_reproject") 
 To read shapesile (zone)

 OGR data source with driver: ESRI Shapefile 

 Source: "D:/IDFC work/raja/GDP/SHP", layer: "AllIndiaClass1_563_TownBoundaries2014_15_reproject"

 with 561 features

 It has 10 fields

plot(Zone1)

![india_shp](https://user-images.githubusercontent.com/23652706/33017275-bfdbf04a-ce17-11e7-8709-27f90d846a9d.jpg)
  

Calculate sum of GDP for towns of India

out1 <- extract(A, Zone1, fun = sum, na.rm = T, small = T, df = T)

out2 <- extract(B, Zone1, fun = sum, na.rm = T, small = T, df = T)

out3 <- extract(C, Zone1, fun = sum, na.rm = T, small = T, df = T)

out4 <- extract(D, Zone1, fun = sum, na.rm = T, small = T, df = T)

out5 <- extract(E, Zone1, fun = sum, na.rm = T, small = T, df = T)

out6 <- extract(F, Zone1, fun = sum, na.rm = T, small = T, df = T)

out7 <- extract(G, Zone1, fun = sum, na.rm = T, small = T, df = T)

out8 <- extract(H, Zone1, fun = sum, na.rm = T, small = T, df = T)

out9 <- extract(I, Zone1, fun = sum, na.rm = T, small = T, df = T)

out10 <- extract(J, Zone1, fun = sum, na.rm = T, small = T, df = T) 

out11 <- extract(K, Zone1, fun = sum, na.rm = T, small = T, df = T)

out12 <- extract(L, Zone1, fun = sum, na.rm = T, small = T, df = T)

out13 <- extract(M, Zone1, fun = sum, na.rm = T , small = T, df = T)

Write the output to csv file

z1 <- Zone1@data # Assign the attributes of shapefile to z1

output <- cbind(z1,out1,out2,out3,out4,out5,out6,out7,out8,out9,out10,out11,out12,out13) 

 Join the outputs to the attributes of shapefile
write.csv(output,"D:/IDFC work/raja/GDP/Output/output.csv", na="NA") # Enter Output csv file name and path
