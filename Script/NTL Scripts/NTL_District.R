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


setwd("C:/Users/Admin/Desktop/Intercalibrated_NTL") # To set directory

# Non Radiance Zonal Stat Calculation Function######################################################################################
ptm <- proc.time()
Zonal_Stat_NTL <- function(x,y) # Define function
{
  A<-extract(x,y) # Extract raster data zone-wise
  
  R<-array(0,dim=c(length(A),11)) # Create an empty array
  for (i in 1:length(A))  # Create for loop 
  {
    temp=A[[i]]  #Get temperary memory
    NTLLumin1<-names(NTLLumin)
    NTLLumin2<- gsub(".tif","",NTLLumin1)
    B<-mlv(temp, method = "mfv") # Find mode of that zone
    
    R[i,1]=NTLLumin2     # To mention Luminosity Data_Source & year
    SR1<-length(temp[temp>0])
    SR2<-length(temp) 
    SR3<-SR1/SR2
    SR<-paste(round(SR3*100,digits=2),"%",sep="")
    R[i,2]= SR2 # Find count for that zone
    R[i,3]= SR1 # Find Lit up pixel count for that zone
    R[i,4]= SR # Percentage area cover by light
    R[i,5]= mean(temp) # Find mean of that zone
    R[i,6]= min(temp) # Find minimum of that zone
    R[i,7]= max(temp) # Find maximum of that zone
    R[i,8]= median(temp) # Find median of that zone
    R[i,9]= sd(temp) # Find Std_Dev of that zone
    R[i,10]= sum(temp) # Find sum of that zone
    R[i,11]=B$M
    
    rm(temp)
  }
  
  BQ=paste(substr(names(x),(stri_length(names(x))-4),(stri_length(names(x)))), y[[6]], sep="_")
  CQ=paste(substr(names(x),(stri_length(names(x))-3),(stri_length(names(x)))))
  colnames(R) <- c("NTL_Data_Source","NTL_Count","NTL_Lit_up_Pixel_count","NTL_Percentage_light_cover_Area","NTL_Mean","NTL_Min","NTL_Max","NTL_Median","NTL_Std_Dev","NTL_Sum","NTL_Mode") # Change column header
  
  z<-cbind(y[[1]],y[[2]],y[[3]],y[[4]],y[[5]],y[[6]],CQ,R,BQ) # Bind data with shape file
  return(z) 
}

# To Calculate Zonal Stat For Non Radiance Data ######################################################################################################################

NTLfileR <- list.files(getwd(), pattern="NTL.*.tif$", full.names=FALSE) # Read list of Raster
for(m in 1:length(NTLfileR))                                             # for loop to read raster
{
  NTLLumin <-raster(NTLfileR[m])                                            #Move raster to variable Lumin
  Sfile<- list.files(getwd(),pattern=".*.shp$", full.name=FALSE)    #Read list of Zone
  for(q in 1:length(Sfile))  
  { 
    Zone <-shapefile(Sfile[q])
    M<-Zonal_Stat_NTL (NTLLumin,Zone) 
    if (q<2) { temp_shape_NTL<-M }   else    { temp_shape_NTL<-rbind(temp_shape_NTL,M) }
  }
  if (m<2) { temp_raster_NTL<-temp_shape_NTL }  else  { temp_raster_NTL<-rbind(temp_raster_NTL,temp_shape_NTL) }
  rm(temp_shape_NTL)
}  
############################################################################################################################################################
NTL<-temp_raster_NTL
proc.time() - ptm
colnames(NTL) <- c("State_Name","District_Name","Census_CD_2011","New_Census_CD","State_Cen_CD","Area","Year","NTL_Data_Source","NTL_Count","NTL_Lit_Up_Pixcel_Count","Lit_up_pixel_Percent","NTL_Mean","NTL_Min","NTL_Max","NTL_Median","NTL_Std_Dev","NTL_Sum","NTL_Mode","Sys_Genrated_UID") # Change column header

write.csv(NTL,"C:/Users/Admin/Desktop/Intercalibrated_NTL/NTL_Dist_ZS.csv", na="NA") # Enter Output csv file name and path 


