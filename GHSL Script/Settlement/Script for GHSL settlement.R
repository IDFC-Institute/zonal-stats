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
require(Matrix)
require(stringi)
require(eeptools)

################################################
memory.limit(size = 100000)

Zone<-readOGR("F:/Raster Data/GHSL settlement/world 1 km original/chaina","chaina_reproject") 

Lumin <- raster("F:/Raster Data/GHSL settlement/world 1 km original/2015/GHS_SMOD_POP2015_GLOBE_R2016A_54009_1k_v1_0/GHS_SMOD_POP2015_GLOBE_R2016A_54009_1k_v1_0.tif") 
values(Lumin)
#ptm <- proc.time()
Zonal_Stat <- function(x,y) # Function declaration and signature
{
A<-extract(x,y) # Extract raster data zone-wise
R<-array(0,dim=c(length(A),6)) # Create empty array
for (i in 1:length(A))  # Create for loop to find the zonal statistics
{
  temp=A[[i]]  #Get zone-wise data in temperary memory
  R[i,1]=length(temp) # Find number of pixel in that zone
  SRR1<-length(temp[temp==1])
  SRR2<-length(temp[temp==2])
  SRR3<-length(temp[temp==3])
  R[i,2]= SRR1
  R[i,3]= SRR2 
  R[i,4]= SRR3 
  R[i,5]=min(temp) # Find minimum of that zone
  R[i,6]=max(temp) # Find maximum of that zone
  
  rm(temp) # remove temparary variable
}
colnames(R) <- c("Count","BAS","LDC","HDC","Min","Max") # Change column header
z<-cbind(y,R) # Bind data with shape file
return(z) 
}
M<-Zonal_Stat(Lumin,Zone)
#proc.time() - ptm
write.csv(M,"F:/Raster Data/GHSL settlement/world 1 km original/chaina/zs output/chaina_ghs_settlement_settlement2015.csv", na="NA") # Enter Output csv file name and path


