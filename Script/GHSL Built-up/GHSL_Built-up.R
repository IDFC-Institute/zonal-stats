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
library(gstat)
memory.limit(size = 100000)
Zone<-readOGR("D:/K/New folder/R Markdown/Input/SHP","State") # To read shapesile (zone)
Lumin <- raster("D:/K/New folder/R Markdown/Input/GHSL builtup/T3_2014/India_GHSL_builtup_t3_reclass.tif")
Zonal_Stat <- function(x,y) # Function declaration and signature
{
  k = x@crs
  y = spTransform(y,k)
  A<-extract(x,y) # Extract raster data zone-wise
  R<-array(0,dim=c(length(A),3)) # Create empty array
  for (i in 1:length(A))  # Create for loop to find the zonal statistics
  {
    temp=A[[i]]  #Get zone-wise data in temperary memory
    #R[i,1]=length(temp) # Find number of pixel in that zone
    #R[i,2]=mean(temp) # Find mean of that zone
    R[i,3]=sum(temp) # Find sum of that zone
    rm(temp) # remove temparary variable
  }
  colnames(R) <- c("Count","Mean","Sum") # Change column header
  z<-cbind(y,R) # Bind data with shape file
  return(z) 
}
M<-Zonal_Stat(Lumin,Zone)
write.csv(M,"D:/K/New folder/R Markdown/Input/GHSL builtup/T3_2014/2015_example3.csv", na="NA")