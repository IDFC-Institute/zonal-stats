![uxo_idfc_logo](https://user-images.githubusercontent.com/23652706/33058226-92f00448-ceb4-11e7-9997-7e0940f5ab1a.jpg)

**Zonal Statistics for Built-up**

Zonal statistics refers to the calculation of statistics on values of a raster within the zones of another dataset

This project is a collaborative work of UXO India and IDFC

In the following example the zonal statistics [Number of pixels, mean and sum] for Builtup of each state in India is calculated

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

**Setting the memory limit [To accommodate large data]**

memory.limit(size = 100000)<br/>
[1] 1e+05<br/>

**To read shapefile and assign to a variable [zone]**

Zone<-readOGR("D:/K/New folder/R Markdown/Input/SHP","State") #OGR data source with driver: ESRI Shapefile<br/> 
plot(Zone)<br/>

![india_state](https://user-images.githubusercontent.com/23652706/33058132-24546b5a-ceb4-11e7-89e0-a096e934606e.jpg)

**To read Raster data and assign to a variable [Lumin]**

Lumin <- raster("D:/K/New folder/R Markdown/Input/GHSL builtup/T3_2014/India_GHSL_builtup_t3_reclass.tif")<br/>
plot(Lumin)<br/>

![ghsl_builtup_raster](https://user-images.githubusercontent.com/23652706/33058158-41c252ce-ceb4-11e7-909a-88bd63cdf6b4.jpg)

**Define function for zonal statistics**

Zonal_Stat <- function(x,y) # Function declaration and signature<br/>
{<br/>
k = x@crs  # Assign the projection of raster to variable k<br/> 
y = spTransform(y,k)  # Reproject the shapefile to raster projection<br/> 
A<-extract(x,y) # Extract raster data zone-wise<br/> 
R<-array(0,dim=c(length(A),3)) # Create empty array<br/> 
for (i in 1:length(A))  # Create for loop to find the zonal statistics<br/> 
{<br/>
  temp=A[[i]] #Get zone-wise data in temperary memory<br/>
  R[i,1]=length(temp) # Find number of pixel in that zone<br/>
  R[i,2]=mean(temp) # Find mean of that zone<br/>
  R[i,3]=sum(temp) # Find sum of that zone<br/>
  rm(temp) # remove temparary variable<br/>
}<br/>
colnames(R) <- c("Count","Mean","Sum") # Change column header<br/>
z<-cbind(y,R) # Bind data with shape file<br/>
return(z)<br/>
}<br/>

**Call the zonal statistics function**

M<-Zonal_Stat(Lumin,Zone)<br/>

**Write the output to csv file<br/>**
write.csv(M,"D:/K/New folder/R Markdown/Input/GHSL builtup/T3_2014/2015_example3.csv", na="NA")
