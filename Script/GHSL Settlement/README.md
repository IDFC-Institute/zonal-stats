![uxo_idfc_logo](https://user-images.githubusercontent.com/23652706/33058226-92f00448-ceb4-11e7-9997-7e0940f5ab1a.jpg)

**Zonal statistics for GHSL Settlement**

Zonal statistics refers to the calculation of statistics on values of a raster within the zones of another dataset

This project is a collaborative work of UXO India and IDFC

In the following example the settlement of each state in India is calculated

**Required Packages**

library(rgdal) # To import raster data
library(maptools) # To plot the data
library(proj4) # To reproject raster
library(xtable) # To export data to html tables
library (raster) # Required for rgdal
library (rgeos) # Required for maptools
library (spatstat) # Analysing spatial point patterns 
library (tiff) #Read TIFF images and required for rgdal 
library (sp)#Required for maptools 
library (data.table) # Modifying columns
library (modeest) #To calculate mode value for the zone 
library (foreign) # Required for maptools

**Setting the memory limit**

memory.limit(size = 100000)<br/>
[1] 1e+05<br/>

**To read shapefile and assign to a variable zone**

Zone<-readOGR("D:/K/New folder/R Markdown/Input/SHP","State") # To read shapesile (zon e)<br/>
plot(Zone)<br/>

![india_state](https://user-images.githubusercontent.com/23652706/33067450-3037e792-ced4-11e7-8518-9f29a5426c54.jpg)

**To read Raster data and assign to a variable Lumin and user can change the data as per requirement. This documentation is done by taking 2015 data as an example**

Lumin <- raster("D:/K/New folder/R Markdown/Input/GHSL settlement/GHS_SMOD_POP201 5_GLOBE_R2016A_54009_1k_v1_0.tif")<br/>
plot(Lumin)<br/>

![ghsl_settlement](https://user-images.githubusercontent.com/23652706/33067573-8e5ee5a0-ced4-11e7-8951-2e202a0457e0.jpg)

**Define function for zonal statistics**

Zonal_Stat <- function(x,y) # Function declaration and signature<br/>
{<br/>
k = x@crs #Assigning the raster projection to K<br/>
y = spTransform(y,k) #Reproject the vector to raster projection A<-extract(x,y) # Extract raster data zone-wise<br/> R<-array(0,dim=c(length(A),6)) # Create empty array<br/>
for (i in 1:length(A)) # Create for loop to find the zonal statistics<br/>
{<br/>
temp=A[[i]] #Get zone-wise data in temperary memory<br/> 
R[i,1]=length(temp) # Find number of pixel in that zone<br/>
SRR1<-length(temp[temp==1]) #Assigning the length of class 1 of raster layer to SRR1<br/> 
SRR2<-length(temp[temp==2]) #Assigning the length of class 2 of raster layer to SRR2<br/>
SRR3<-length(temp[temp==3]) #Assigning the length of class 3 of raster layer to SRR3<br/> 
R[i,2]= SRR1#Assign the SRR1 to column 2 of array R<br/>
R[i,3]= SRR2 #Assign the SRR2 to column 3 of array<br/>
R R[i,4]= SRR3 #Assign the SRR3 to column 4 of array<br/> 
R R[i,5]=min(temp) # Find minimum of that zone<br/> 
R[i,6]=max(temp) # Find maximum of that zone<br/>
rm(temp) # remove temparary variable<br/>
}<br/>
colnames(R) <- c("Count","BAS","LDC","HDC","Min","Max") # Change column header z<-cbind(y,R) # Bind data with shape file<br/>
return(z)<br/>
}<br/>

**Call the zonal statistics function**

M<-Zonal_Stat(Lumin,Zone)<br/>
M<br/>
class	: SpatialPolygonsDataFrame<br/>					
features	: 36<br/>						
extent	: 6365419, 9372665, 834596.8, 4460278 (xmin, xmax, ymin, ymax)<br/>	
coord. ref. : +proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs<br/>	
variables	: 10<br/>						
names	:		Name, St_Csus_Cd,		Name_T,	Area,  Count,  BAS,	LDC,<br/>
HDC, Min, Max<br/>						
min values	: Andaman & Nicobar Island,	0,	State,	109.3623,   105,   2,	4, 0,	0,	2<br/>								
max values	:	West Bengal,	35, Union Territory, 342592.8684, 343863, 67215, 15404, 10709,	0,	3<br/>					

**Write the output to csv file**
write.csv(M,"D:/K/New folder/R Markdown/Input/GHSL settlement/2015_ZS_Built_up.csv", n a="NA")<br/>
