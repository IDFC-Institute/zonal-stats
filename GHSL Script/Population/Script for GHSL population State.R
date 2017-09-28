
#library(rgdal)
#library(maptools)
#library(lattice)
#library(proj4)
#require(raster)
#library(geoR)
#library(xtable)
#library(fields)
#require(gstat)
#require(spatstat)
#require(tiff)
#require(sp)
#require(doBy)
#require(data.table)
#require(modeest)
#require(foreign)

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
memory.size(100000)

Zone<-readOGR("D:/Raja/GHSL_R_Script/Part_2","State") # To read shapesile (zone)

Lumin <- raster("D:/#IDFC# classification/#DMIC/GHSL_population_calculation/data/GHS_POP_GPW41975_GLOBE_R2015A_54009_250_v1_0/GHS_POP_GPW41975_GLOBE_R2015A_54009_250_v1_0/GHS_POP_GPW41975_GLOBE_R2015A_54009_250_v1_0.tif") # To read Raster data

Zonal_Stat <- function(x,y) # Function declaration and signature
{
A<-extract(x,y) # Extract raster data zone-wise
R<-array(0,dim=c(length(A),3)) # Create empty array
for (i in 1:length(A))  # Create for loop to find the zonal statistics
{
  temp=A[[i]]  #Get zone-wise data in temperary memory
  R[i,1]=length(temp) # Find number of pixel in that zone
  R[i,2]=mean(temp) # Find mean of that zone
  R[i,3]=sum(temp) # Find sum of that zone
  rm(temp) # remove temparary variable
}
colnames(R) <- c("Count","Mean","Sum") # Change column header
z<-cbind(y,R) # Bind data with shape file
return(z) 
}
M<-Zonal_Stat(Lumin,Zone)
write.csv(M,"D:/Raja/GHSL_R_Script/2015_ZS_pop.csv", na="NA") # Enter Output csv file name and path
writeOGR(M, dsn = 'D:/Raja/GHSL_R_Script', layer ='2015_ZS_pop', driver = 'ESRI Shapefile',check_exists=T, overwrite_layer=T) 
