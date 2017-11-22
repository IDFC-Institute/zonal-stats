![uxo_idfc_logo](https://user-images.githubusercontent.com/23652706/33058226-92f00448-ceb4-11e7-9997-7e0940f5ab1a.jpg)

**Zonal Statistics for DMSP OLS Inter-Calibrated**
 
Zonal statistics refers to the calculation of statistics on values of a raster within the zones of another dataset

This project is a collaborative work of UXO India and IDFC

In the following example the statistics i.e mean, median, mode, standard deviation, minimum, maximum values are calculated for DMSP OLS Inter-Calibrated data

**load required libraries and packages**

library(rgdal) # To import raster data<br>
library(maptools) # To plot the data<br>
library(proj4)  # To reproject raster<br>
library(xtable) # To export data to html tables<br>
library (raster) # Required for rgdal<br>
library (rgeos) # Required for maptools<br>
library (spatstat) # Analysing spatial point patterns<br>
library (tiff) # Read TIFF images and required for rgdal<br>
library (sp) #Required for maptools<br>
library (data.table) # Modifying columns<br>
library (modeest) #To calculate mode value for the zone<br>
library (foreign) # Required for maptools<br>

**Setting working directory**

setwd("D:/IDFC work/Bulk Zonal Stat Calculation/INPUT/R_Script_Directory") # To set directory<br>

**Starting time time calculation**

ptm <- proc.time()<br>

**Defining function for zonal statistics**

Zonal_Stat_NTL <- function(x,y) # Define function<br>
{<br>
  A<-extract(x,y) # Extract raster data zone-wise<br>
  
  R<-array(0,dim=c(length(A),11)) # Create an empty array<br>
  for (i in 1:length(A))  # Create for loop <br>
  {<br>
    temp=A[[i]]  #Get temperary memory<br>
    NTLLumin1<-names(NTLLumin)<br>
    NTLLumin2<- gsub(".tif","",NTLLumin1)<br>
    B<-mlv(temp, method = "mfv") # Find mode of that zone<br>
    B<br>
    R[i,1]=NTLLumin2     # To mention Luminosity Data_Source & year<br>
    SR1<-length(temp[temp>0])<br>
    SR2<-length(temp) <br>
    SR3<-SR1/SR2<br>
    SR<-paste(round(SR3*100,digits=2),"%",sep="")<br>
    R[i,2]= SR2 # Find count for that zone<br>
    R[i,3]= SR1 # Find Lit up pixel count for that zone<br>
    R[i,4]= SR # Percentage area cover by light<br>
    R[i,5]= mean(temp) # Find mean of that zone<br>
    R[i,6]= min(temp) # Find minimum of that zone<br>
    R[i,7]= max(temp) # Find maximum of that zone<br>
    R[i,8]= median(temp) # Find median of that zone<br>
    R[i,9]= sd(temp) # Find Std_Dev of that zone<br>
    R[i,10]= sum(temp) # Find sum of that zone<br>
    R[i,11]=B$M<br>
        rm(temp)<br>
  }<br>
  
  BQ=paste(substr(names(x),(stri_length(names(x))-4),(stri_length(names(x)))), y[[6]], sep="_")<br>
  
  CQ=paste(substr(names(x),(stri_length(names(x))-3),(stri_length(names(x)))))<br>
  colnames(R) <- c("NTL_Data_Source","NTL_Count","NTL_Lit_up_Pixel_count","NTL_Percentage_light_cover_Area","NTL_Mean","NTL_Min","NTL_Max","NTL_Median","NTL_Std_Dev","NTL_Sum","NTL_Mode") # Change column header<br>
  
  z<-cbind(y[[1]],y[[2]],y[[3]],y[[4]],y[[5]],y[[6]],CQ,R,BQ) # Bind data with shape file<br>
  return(z) <br>
}<br>
Main function to read files and call the zonal statistics function<br>
NTLfileR <- list.files(getwd(), pattern="NTL.*.tif$", full.names=FALSE) # Read list of Raster<br>
for(m in 1:length(NTLfileR))                                             # for loop to read raster<br>
{<br>
  NTLLumin <-raster(NTLfileR[m])    #Move raster to variable Lumin<br>
  Sfile<- list.files(getwd(),pattern=".*.shp$", full.name=FALSE)    #Read list of Zone<br>
  x = NTLLumin@crs                #Assign the projection of raster to a variable<br>
  for(q in 1:length(Sfile))       # Loop for reading the shapefiles<br>
  { <br>
    Zone <-shapefile(Sfile[q])    # Assigning each shapefile to a variable zone<br>
    Zone <- spTransform(Zone,x)   # reproject shapefile to raster projection sysytem<br>
    M<-Zonal_Stat_NTL(NTLLumin,Zone) # call the zonal statistics function<br>
    if (q<2) { temp_shape_NTL<-M }   else    { temp_shape_NTL<-rbind(temp_shape_NTL,M) }# Write the output to temp_shape_NTL<br>
  }<br>
  if (m<2) { temp_raster_NTL<-temp_shape_NTL }  else  { temp_raster_NTL<-rbind(temp_raster_NTL,temp_shape_NTL) } #Write the output to temp_raster_NTL<br>
  rm(temp_shape_NTL) # remove the data<br>
}<br>

NTL<-temp_raster_NTL<br>

proc.time() – ptm #Displaying of time taken.<br>

**user  system elapsed**

10.52    0.04   10.62<br>

colnames(NTL) <- c( "city name", "boundary type", "state", "X","y", "z","year","NTL_Data_Source","NTL_Count","NTL_Lit_up_Pixel_count","NTL_Percentage_light_cover_Area","NTL_Mean","NTL_Min","NTL_Max","NTL_Median","NTL_Std_Dev","NTL_Sum","NTL_Mode","BQ" ) # Change column header<br>

write.csv(NTL,"D:/IDFC work/Bulk Zonal Stat Calculation/INPUT/R_Script_Directory/NTL_Admin_ZS.csv", na="NA") # Enter Output csv file name and path
