**Zonal Statistics for GDP**
_(Global GDP Scenarios)_ 

![pic3](https://user-images.githubusercontent.com/23652706/33055234-4ec9d268-cea4-11e7-8757-aa48c8b4893f.JPG)
 
Zonal statistics refers to the calculation of statistics on values of a raster within the zones of another dataset.  
This project is a collaborative work of UXO India and IDFC. 

In the following script sum of GDP for each town in India is calculated. 

**Required Packages**

library(rgdal) # To import raster data<br/>
library(maptools) # To plot the data<br/>
library(proj4) # To reproject the shapefile<br/>
library(raster) # Required for rgdal<br/>
library(xtable) # To export data to html tables<br/>
library(spatstat) # To analyse spatial point pattern<br/>
library(tiff) # Required for rgdal<br/>
library(sp) # Required for maptools<br/>
library(doBy) # To calculate groupwise statistics<br/>
library(data.table) # To modify columns<br/>
library(modeest) # To calculate mode value for the zone<br/>
library(foreign) # Required for maptools<br/>
library(rgeos) # Required for maptools<br/>



**Read the GDP data of India in csv format**

MyData <- read.csv(file="D:/IDFC work/raja/GDP/Dataset/SSP/CSV/gdp_ssp1.csv", 

header=TRUE, sep=",")



**Read spatial coordinates from csv file to create a Spatial object**

coordinates(MyData)<-~px+py



**Give projection system to the variable**

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")



**Assign the projection system to the shapefile**

proj4string(MyData) <- crs.geo



**Create an empty raster r**

r <- raster()



**Give the extent of raster dataset**

extent(r) <- extent(MyData)

extent(r)

 class       : Extent 
 
 xmin        : -179.9599 
 
 xmax        : 179.9352
 
 ymin        : -59.4568 
 
 ymax        : 83.55961



**Assign the pixel size as 0.5**

res(r)=0.50

r

class       : RasterLayer 
 
dimensions  : 286, 720, 205920  (nrow, ncol, ncell)
 
resolution  : 0.5, 0.5  (x, y)
 
extent      : -179.9599, 180.0401, -59.44039, 83.55961  (xmin, xmax, ymin, ymax)
 
coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0



**Creating a raster layer by summing up the GDP values in the pixel**

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



**Read the town boundaries shapefile of India**

Zone1<-readOGR("D:/IDFC work/raja/GDP/SHP","AllIndiaClass1_563_TownBoundaries2014_15_reproject") # To read shapesile (zone)

OGR data source with driver: ESRI Shapefile 
 
Source: "D:/IDFC work/raja/GDP/SHP", layer: "AllIndiaClass1_563_TownBoundaries2014_15_reproject"
 
with 561 features
 
It has 10 fields
 
plot(Zone1)

![india_shp](https://user-images.githubusercontent.com/23652706/33055147-daef0368-cea3-11e7-8fba-0408a5f33d56.jpg)


**Calculate sum of GDP for towns of India**

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



**Write the output to csv file**

z1 <- Zone1@data # Assign the attributes of shapefile to z1

output <- cbind(z1,out1,out2,out3,out4,out5,out6,out7,out8,out9,out10,out11,out12,out13) # Join the outputs to the attributes of shapefile

write.csv(output,"D:/IDFC work/raja/GDP/Output/output.csv", na="NA") # Enter Output csv file name and path


