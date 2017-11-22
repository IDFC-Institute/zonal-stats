![uxo_idfc_logo](https://user-images.githubusercontent.com/23652706/33058226-92f00448-ceb4-11e7-9997-7e0940f5ab1a.jpg)

**Zonal Statistics for Nighttime-Light data**

Zonal statistics refers to the calculation of statistics on values of a raster within the zones of another dataset<br/>

This project is a collaborative work of UXO India and IDFC<br/>

In the following example the zonal statistics i.e., sum, mean, median, mode, minimum value, maximum value, standard deviation for DMSP OLS Original, Deblurred, Radiance and VIIRS datasets are calculated<br/>

**Required Packages**

library(rgdal) # To import raster data<br/>
library(maptools) # To plot the data<br/>
library(proj4)  # To reproject raster<br/>
library(xtable) # To export data to html tables<br/>
library (raster) # Required for rgdal<br/>
library (rgeos) # Required for maptools<br/>
library (spatstat) # Analysing spatial point patterns<br/>
library (tiff) # Read TIFF images and required for rgdal<br/>
library (sp) #Required for maptools<br/>
library (data.table) # Modifying columns<br/>
library (modeest) #To calculate mode value for the zone<br/>
library (foreign) # Required for maptools<br/>

**Set memory**

memory.size(100000)<br/>
[1] 1e+05<br/>

**Set working directory i.e giving the path for input files**

setwd("D:/IDFC work/Bulk Zonal Stat Calculation/INPUT/R_Script_Directory") # To set directory<br/>

**DMSP OLS original - Zonal Stat Calculation Function**

Zonal_Stat_NR <- function(x,y) # Define function<br/>

{<br/>
  A<-extract(x,y) # Extract raster data zone-wise<br/>
  R<-array(0,dim=c(length(A),48)) # Create an empty array<br/>
  for (i in 1:length(A))  # Create for loop<br/>
  {<br/>
    temp=A[[i]]  #Get temperary memory<br/>
    NRLumin1<-names(NRLumin)<br/>
    NRLumin2<- gsub(".tif","",NRLumin1) #gsub() function replaces all matches of a string<br/>
    B<-mlv(temp, method = "mfv") # Find mode of that zone<br/>
    R[i,1]="https://ngdc.noaa.gov/eog/dmsp/downloadV4composites.html#AVSLCFC"<br/>
    R[i,2]=NRLumin2     # To mention Luminosity Data_Source & year<br/>
    SR1<-length(temp[temp>0])<br/>
    SR2<-length(temp)  # Get the length of temp<br/>
    SR3<-SR1/SR2<br/>
    SR<-paste(round(SR3*100,digits=2),"%",sep="")#Rounds the values in its first argument to the specified number of decimal     places<br/>
    R[i,3]= SR2 # Find count for that zone<br/>
    R[i,4]= SR1 # Find Lit up_Pixel count for that zone<br/>
    R[i,5]= SR # Percentage area cover by light<br/>
    R[i,6]= mean(temp) # Find mean of that zone<br/>
    R[i,7]= min(temp) # Find minimum of that zone<br/>
    R[i,8]= max(temp) # Find maximum of that zone<br/>
    R[i,9]= median(temp) # Find median of that zone<br/>
    R[i,10]= sd(temp) # Find Std_Dev of that zone<br/>
    R[i,11]= sum(temp) # Find sum of that zone<br/>
    R[i,12]=B$M # Add data from column M of data B to 12th column of R<br/>
    R[i,13:48]=""<br/>
    rm(temp) # Delete the data from temp<br/>
  }<br/>
  
BQ=paste(substr(names(x),(stri_length(names(x))-4),(stri_length(names(x)))), y[[6]], sep="_")<br/>
  CQ=paste(substr(names(x),(stri_length(names(x))-3),(stri_length(names(x)))))<br/>
  colnames(R) <-<br/> 
  c("NR_Link","NR_Data_Source","NR_Count","NR_Lit_up_Pixel_count","NR_Percentage_light_cover_Area","NR_Mean","NR_Min","NR_Max","NR_Median","NR_Std_Dev","NR_Sum","NR_Mode","NRD_Link","NRD_Data_Source","NRD_Count","NRD_Lit_up_Pixel_count","NRD_Percentage_light_cover_Area","NRD_Mean","NRD_Min","NRD_Max","NRD_Median","NRD_Std_Dev","NRD_Sum","NRD_Mode","Rad_Link","Rad_Data_Source","Rad_Count","Rad_Lit_up_Pixel_count","Rad_Percentage_light_cover_Area","Rad_Mean","Rad_Min","Rad_Max","Rad_Median","Rad_Std_Dev","Rad_Sum","Rad_Mode","VIIRS_Link","VIIRS_Data_Source","VIIRS_Count","VIIRS_Lit_up_Pixel_count","VIIRS_Percentage_light_cover_Area","VIIRS_Mean","VIIRS_Min","VIIRS_Max","VIIRS_Median","VIIRS_Std_Dev","VIIRS_Sum","VIIRS_Mode") # Change column header
   z<-cbind(y[[1]],y[[2]],y[[3]],y[[4]],y[[5]],y[[6]],CQ,R,BQ) # Bind data with shape file<br/>
  return(z)<br/>
}<br/>

**DMSP OLS Deblurred - Zonal Statistics Calculation Function**

Zonal_Stat_NRD <- function(xA,yA) # Define function<br/>

{
  AA<-extract(xA,yA) # Extract raster data zone-wise<br/>
  RA<-array(0,dim=c(length(AA),48)) # Create an empty array<br/>
  for (j in 1:length(AA))  # Create for loop<br/>
  {<br/>
    temp=AA[[j]]  #Get temperary memory<br/>
    NRDLumin1<-names(NRDLumin)<br/>
    NRDLumin2<- gsub(".tif","",NRDLumin1)<br/>
    BA<-mlv(temp, method = "mfv") # Find mode of that zone<br/>
    RA[j,1:12]=""<br/>
    RA[j,13]="https://ngdc.noaa.gov/eog/dmsp/downloadV4composites.html#AXP"<br/>
    RA[j,14]=NRDLumin2     # To mention Luminosity Data_Source & year<br/>
    SA1<-length(temp[temp>0])<br/>
    SA2<-length(temp)<br/>
    SA3<-SA1/SA2<br/>
    SA<-paste(round(SA3*100,digits=2),"%",sep="")<br/>
    RA[j,15]= SA2 # Find count for that zone<br/>
    RA[j,16]= SA1 # Find Lit up_Pixel count for that zone<br/>
    RA[j,17]= SA # Percentage area cover by light<br/>
    RA[j,18]=mean(temp) # Find mean of that zone<br/>
    RA[j,19]=min(temp) # Find minimum of that zone<br/>
    RA[j,20]=max(temp) # Find maximum of that zone<br/>
    RA[j,21]=median(temp) # Find median of that zone<br/>
    RA[j,22]=sd(temp) # Find Std_Dev of that zone<br/>
    RA[j,23]=sum(temp) # Find sum of that zone<br/>
    RA[j,24]=BA$M # Find mode of that zone<br/>
    RA[j,25:48]=""<br/>
    rm(temp)<br/>
  }<br/>
  
  BQ=paste(substr(names(xA),(stri_length(names(xA))-4),(stri_length(names(xA)))), yA[[6]], sep="_")<br/>
  CQ=paste(substr(names(xA),(stri_length(names(xA))-3),(stri_length(names(xA)))))<br/>
  colnames(RA) <-<br/>
  c("NR_Link","NR_Data_Source","NR_Count","NR_Lit_up_Pixel_count","NR_Percentage_light_cover_Area","NR_Mean","NR_Min","NR_Max","NR_Median","NR_Std_Dev","NR_Sum","NR_Mode","NRD_Link","NRD_Data_Source","NRD_Count","NRD_Lit_up_Pixel_count","NRD_Percentage_light_cover_Area","NRD_Mean","NRD_Min","NRD_Max","NRD_Median","NRD_Std_Dev","NRD_Sum","NRD_Mode","Rad_Link","Rad_Data_Source","Rad_Count","Rad_Lit_up_Pixel_count","Rad_Percentage_light_cover_Area","Rad_Mean","Rad_Min","Rad_Max","Rad_Median","Rad_Std_Dev","Rad_Sum","Rad_Mode","VIIRS_Link","VIIRS_Data_Source","VIIRS_Count","VIIRS_Lit_up_Pixel_count","VIIRS_Percentage_light_cover_Area","VIIRS_Mean","VIIRS_Min","VIIRS_Max","VIIRS_Median","VIIRS_Std_Dev","VIIRS_Sum","VIIRS_Mode") # Change column header
  zA<-cbind(yA[[1]],yA[[2]],yA[[3]],yA[[4]],yA[[5]],yA[[6]],CQ,RA,BQ) # Bind data with shape file
  return(zA)<br/> 
}<br/>

**DMSP OLS Radiance - Zonal Statistics Calculation Function**

Zonal_Stat_Rad <- function(xR,yR) # Define function<br/>

{<br/>
  AAA<-extract(xR,yR) # Extract raster data zone-wise<br/>
  RR<-array(0,dim=c(length(AAA),48)) # Create an empty array<br/>
  for (k in 1:length(AAA))  # Create for loop<br/>
  {<br/>
    temp=AAA[[k]]  #Get temperary memory<br/>
    RLumin1<-names(RLumin)<br/>
    RLumin2<- gsub(".tif","",RLumin1)<br/>
    BR<-mlv(temp, method = "mfv") # Find mode of that zone<br/>
    RR[k,1:24]=""<br/>
    RR[k,25]="https://ngdc.noaa.gov/eog/dmsp/download_radcal.html"<br/>
    RR[k,26]=RLumin2     # To mention Luminosity Data_Source & year<br/>
    SRR1<-length(temp[temp>0])<br/>
    SRR2<-length(temp)  # Get the size of temp<br/>
    SRR3<-SRR1/SRR2<br/>
    SRR<-paste(round(SRR3*100,digits=2),"%",sep="") # Round off the value<br/>
    RR[k,27]= SRR2 # Find count for that zone<br/>
    RR[k,28]= SRR1 # Find Lit up Pixel count for that zone<br/>
    RR[k,29]= SRR # Percentage area cover by light<br/>
    RR[k,30]=mean(temp) # Find mean of that zone<br/>
    RR[k,31]=min(temp) # Find minimum of that zone<br/>
    RR[k,32]=max(temp) # Find maximum of that zone<br/>
    RR[k,33]=median(temp) # Find median of that zone<br/>
    RR[k,34]=sd(temp) # Find Std_Dev of that zone<br/>
    RR[k,35]=sum(temp) # Find sum of that zone<br/>
    RR[k,36]=BR$M# Find mode of that zone<br/>
    RR[k,37:48]=""<br/>
    rm(temp)<br/> 
  }<br/>
  
  BQ=paste(substr(names(xR),(stri_length(names(xR))-4),(stri_length(names(xR)))), yR[[6]], sep="_")<br/>
  CQ=paste(substr(names(xR),(stri_length(names(xR))-3),(stri_length(names(xR)))))<br/>
  colnames(RR) <-<br/>
  c("NR_Link","NR_Data_Source","NR_Count","NR_Lit_up_Pixel_count","NR_Percentage_light_cover_Area","NR_Mean","NR_Min","NR_Max","NR_Median","NR_Std_Dev","NR_Sum","NR_Mode","NRD_Link","NRD_Data_Source","NRD_Count","NRD_Lit_up_Pixel_count","NRD_Percentage_light_cover_Area","NRD_Mean","NRD_Min","NRD_Max","NRD_Median","NRD_Std_Dev","NRD_Sum","NRD_Mode","Rad_Link","Rad_Data_Source","Rad_Count","Rad_Lit_up_Pixel_count","Rad_Percentage_light_cover_Area","Rad_Mean","Rad_Min","Rad_Max","Rad_Median","Rad_Std_Dev","Rad_Sum","Rad_Mode","VIIRS_Link","VIIRS_Data_Source","VIIRS_Count","VIIRS_Lit_up_Pixel_count","VIIRS_Percentage_light_cover_Area","VIIRS_Mean","VIIRS_Min","VIIRS_Max","VIIRS_Median","VIIRS_Std_Dev","VIIRS_Sum","VIIRS_Mode") # Change column header
  zR<-cbind(yR[[1]],yR[[2]],yR[[3]],yR[[4]],yR[[5]],yR[[6]],CQ,RR,BQ) # Bind data with shape file
  return(zR)<br/>
  }<br/>

**VIIRS - Zonal Statistics Calculation Function**

Zonal_Stat_VIIRS <- function(xB,yB) # Define function<br/>

{<br/>
  AB<-extract(xB,yB) # Extract raster data zone-wise<br/>  
  RB<-array(0,dim=c(length(AB),48)) # Create an empty array<br/>
  for (l in 1:length(AB))  # Create for loop<br/>
  {<br/>
    temp=AB[[l]]  #Get temperary memory<br/>
    VLumin1<-names(VLumin)<br/>
    VLumin2<- gsub(".tif","",VLumin1)<br/>
    BB<-mlv(temp, method = "mfv") # Find mode of that zone<br/>
    RB[l,1:36]=""<br/>
    RB[l,37]= "https://ngdc.noaa.gov/eog/viirs/download_monthly.html"<br/>
    RB[l,38]=VLumin2 # To mention Luminosity Data_Source & year<br/>
    SRRR1<-length(temp[temp>0])<br/>
    SRRR2<-length(temp)<br/>
    SRRR3<-SRRR1/SRRR2<br/>
    SRRR<-paste(round(SRRR3*100,digits=2),"%",sep="")<br/>
    RB[l,39]= SRRR2 # Find count for that zone<br/>
    RB[l,40]= SRRR1 # Find Lit up_Pixel count for that zone<br/>
    RB[l,41]= SRRR # Percentage area cover by light<br/>
    RB[l,42]=mean(temp) # Find mean of that zone<br/>
    RB[l,43]=min(temp) # Find minimum of that zone<br/>
    RB[l,44]=max(temp) # Find maximum of that zone<br/>
    RB[l,45]=median(temp) # Find median of that zone<br/>
    RB[l,46]=sd(temp) # Find Std_Dev of that zone<br/>
    RB[l,47]=sum(temp) # Find sum of that zone<br/>
    RB[l,48]=BB$M      # Find mode of that zone<br/>
    rm(temp)<br/> 
  }<br/>
  
  BQ=paste(substr(names(xB),(stri_length(names(xB))-4),(stri_length(names(xB)))), yB[[6]], sep="_")<br/>
  CQ=paste(substr(names(xB),(stri_length(names(xB))-3),(stri_length(names(xB)))))<br/>
  colnames(RB) <-<br/> 
  c("NR_Link","NR_Data_Source","NR_Count","NR_Lit_up_Pixel_count","NR_Percentage_light_cover_Area","NR_Mean","NR_Min","NR_Max","NR_Median","NR_Std_Dev","NR_Sum","NR_Mode","NRD_Link","NRD_Data_Source","NRD_Count","NRD_Lit_up_Pixel_count","NRD_Percentage_light_cover_Area","NRD_Mean","NRD_Min","NRD_Max","NRD_Median","NRD_Std_Dev","NRD_Sum","NRD_Mode","Rad_Link","Rad_Data_Source","Rad_Count","Rad_Lit_up_Pixel_count","Rad_Percentage_light_cover_Area","Rad_Mean","Rad_Min","Rad_Max","Rad_Median","Rad_Std_Dev","Rad_Sum","Rad_Mode","VIIRS_Link","VIIRS_Data_Source","VIIRS_Count","VIIRS_Lit_up_Pixel_count","VIIRS_Percentage_light_cover_Area","VIIRS_Mean","VIIRS_Min","VIIRS_Max","VIIRS_Median","VIIRS_Std_Dev","VIIRS_Sum","VIIRS_Mode") # Change column header
  zB<-cbind(yB[[1]],yB[[2]],yB[[3]],yB[[4]],yB[[5]],yB[[6]],CQ,RB,BQ) # Bind data with shape file
  return(zB)<br/>
  }<br/>

**call function for DMSP OLS Original**

ptm <- proc.time()<br/>
NRfileR <- list.files(getwd(), pattern="NR.*.tif$", full.names=FALSE) # Read list of Raster<br/> 
for(m in 1:length(NRfileR)) # Flow control for all the non radiance data<br/>
{<br/>
  NRLumin <-raster(NRfileR[m]) # Read the raster data<br/> 
  x = NRLumin@crs # Read the projection of the raster<br/>
  Sfile<- list.files(getwd(),pattern=".*.shp$", full.name=FALSE) # Get the shapefiles list in the working directory<br/>
  for(q in 1:length(Sfile))  # Flow control for all the non radiance data<br/>
  {<br/>    
    Zone <-shapefile(Sfile[q]) # Assign the shapefile to variable<br/>
    Zone <- spTransform(Zone,x) # Reproject the vector to the raster projection<br/>
    M<-Zonal_Stat_NR (NRLumin,Zone) #Call the function<br/>
    if (q<2) { temp_shape_NR<-M }   else    { temp_shape_NR<-rbind(temp_shape_NR,M) } # Bind the zonal statistics to<br/> temp_shape_NR<br/>
  }<br/>
  if (m<2) { temp_raster_NR<-temp_shape_NR }  else  { temp_raster_NR<-rbind(temp_raster_NR,temp_shape_NR) } # Bind the zonal statistics to temp_raster_NR<br/>
  rm(temp_shape_NR) # Removes the files in temp_shape_NR<br/>
}<br/>

proc.time() - ptm<br/>
user  system elapsed<br/> 
387.72    2.64  391.08<br/>

**call function for DMSP OLS Deblurred**

ptm <- proc.time()<br/>
NRDfileR <- list.files(getwd(), pattern="DBR.*.tif$", full.names=FALSE) # Read list of Raster<br/>
for(n in 1:length(NRDfileR)) #for loop to read raster<br/>
{<br/>
  NRDLumin <-raster(NRDfileR[n]) #Move raster to variable Lumin<br/>
  y = NRDLumin@crs<br/>
  Sfile<- list.files(getwd(),pattern=".*.shp$", full.name=FALSE)    #Read list of Zone<br/>
  for(r in 1:length(Sfile))<br/>  
  {<br/>
    Zone <-shapefile(Sfile[r])<br/>
    Zone <- spTransform(Zone,y)<br/>
    N<-Zonal_Stat_NRD (NRDLumin,Zone)<br/> 
    if (r<2) { temp_shape_NRD<-N }   else    { temp_shape_NRD<-rbind(temp_shape_NRD,N) }<br/>
  }<br/>
  if (n<2) { temp_raster_NRD<-temp_shape_NRD }  else  { temp_raster_NRD<-rbind(temp_raster_NRD,temp_shape_NRD) }<br/>
  rm(temp_shape_NRD)<br/>
}<br/> 

proc.time() - ptm<br/>
user  system elapsed<br/> 
703.93  103.55  808.82<br/>

**call function for DMSP OLS Radiance**

ptm <- proc.time()<br/>
RfileR <- list.files(getwd(), pattern="RAD.*.tif$", full.names=FALSE) # Read list of Raster<br/>
for(o in 1:length(RfileR))  # for loop to read raster<br/>
{<br/>
  RLumin <-raster(RfileR[o]) #Move raster to variable Lumin<br/>
  f = RLumin@crs  #Get the projection of raster<br/>
  Sfile<- list.files(getwd(),pattern=".*.shp$", full.name=FALSE)    #Read list of Zone<br/>
  for(s in 1:length(Sfile))  # Loop to read the files<br/>
  {<br/> 
    Zone <-shapefile(Sfile[s]) #Assign the shapefile to a variable[zone]<br/>
    Zone <- spTransform(Zone,f) # Reproject the shapefile to raster projection system<br/>
    O<-Zonal_Stat_Rad (RLumin,Zone)  # Call the function<br/>
    if (s<2) { temp_shape_R<-O }   else    { temp_shape_R<-rbind(temp_shape_R,O) } #Write the zonal statistics output to temp_shape_V<br/>

  }<br/>
  if (o<2) { temp_raster_R<-temp_shape_R }  else  { temp_raster_R<-rbind(temp_raster_R,temp_shape_R) } #Write the zonal statistics output to temp_raster_V<br/>
  rm(temp_shape_R)<br/> 
}<br/>  

proc.time() - ptm<br/>
 user  system elapsed<br/>
92.77    1.27   94.34<br/>

**call function for VIIRS Data**

ptm <- proc.time()<br/>
VfileR <- list.files(getwd(), pattern="NPP.*.tif$", full.names=FALSE) # Read list of Raster<br/>
for(p in 1:length(VfileR))                                             # for loop to read raster<br/>
{<br/>
  VLumin <-raster(VfileR[p])                                            #Move raster to variable Lumin<br/>
  g = VLumin@crs<br/>
  Sfile<- list.files(getwd(),pattern=".*.shp$", full.name=FALSE)    #Read list of Zone<br/>
  for(t in 1:length(Sfile))  #Loop for reading the files<br/>
  {<br/> 
    Zone <-shapefile(Sfile[t])  #Assign the shapefile to the variable<br/>
    Zone <- spTransform(Zone,g) #Reproject the vector to raster projection system<br/>
    P<-Zonal_Stat_VIIRS (VLumin,Zone) #Calling the function<br/>
    if (t<2) { temp_shape_V<-P }   else    { temp_shape_V<-rbind(temp_shape_V,P) } #Write the zonal statistics output to temp_shape_V<br/>
  }<br/>
  if (p<2) { temp_raster_V<-temp_shape_V }  else  { temp_raster_V<-rbind(temp_raster_V,temp_shape_V) } Write the zonal statistics output to temp_raster_V<br/>
  rm(temp_shape_V)<br/>
}<br/>

proc.time() - ptm<br/>
user  system elapsed<br/> 
97.09    1.40   98.68<br/>

ptm <- proc.time()<br/>

NR<-temp_raster_NR # Assign the output to the variable<br/>
NRD<-temp_raster_NRD # Assign the output to the variable<br/>
Rad<-temp_raster_R # Assign the output to the variable<br/>
VRS<-temp_raster_V # Assign the output to the variable<br/>

AS<-merge(NR, NRD,by="BQ" , all = TRUE ) # Merge Non Rad & Non Rad Deblurr Data & save to variable "AS"<br/>

AS["NRD_Link.x"]<-AS["NRD_Link.y"] # Assign the NRD_Link.y as NRD_Link.x<br/>
AS["NRD_Data_Source.x"]<-AS["NRD_Data_Source.y"]<br/>
AS["NRD_Count.x"]<-AS["NRD_Count.y"]<br/>
AS["NRD_Lit_up_Pixel_count.x"]<-AS["NRD_Lit_up_Pixel_count.y"]<br/>
AS["NRD_Percentage_light_cover_Area.x"]<-AS["NRD_Percentage_light_cover_Area.y"]<br/>
AS["NRD_Mean.x"]<-AS["NRD_Mean.y"]<br/>
AS["NRD_Min.x"]<-AS["NRD_Min.y"]<br/>
AS["NRD_Max.x"]<-AS["NRD_Max.y"]<br/>
AS["NRD_Median.x"]<-AS["NRD_Median.y"]<br/>
AS["NRD_Std_Dev.x"]<-AS["NRD_Std_Dev.y"]<br/>
AS["NRD_Sum.x"]<-AS["NRD_Sum.y"]<br/>
AS["NRD_Mode.x"]<-AS["NRD_Mode.y"]<br/>

ASK<-AS[1:56] #Assign 1 to 56 columns of AS to ASK<br/>

CS<-merge(ASK, Rad,by="BQ" , all = TRUE )  # Merge "AS" variable with Radiance data<br/>
colnames(CS) <-<br/> c("BQ","Census_Code","State_Name","State_Census_Cd","District_Name","District_Census_ID","Area","Year","NR_Link","NR_Data_Source","NR_Count","NR_Lit_up_pixel_count","NR_Light_pixcel_percentage","NR_Mean","NR_Min","NR_Max","NR_Median","NR_Std_Dev","NR_Sum","NR_Mode","NRD_Link","NRD_Data_Source","NRD_Count","NRD_Lit_up_pixel_count","NRD_Light_pixcel_percentage","NRD_Mean","NRD_Min","NRD_Max","NRD_Median","NRD_Std_Dev","NRD_Sum","NRD_Mode","Rad_Link","Rad_Data_Source","Rad_Count","Rad_Lit_up_pixel_count","Rad_Light_pixcel_percentage","Rad_Mean","Rad_Min","Rad_Max","Rad_Median","Rad_Std_Dev","Rad_Sum","Rad_Mode","VIIRS_Link","VIIRS_Data_Source","VIIRS_Count","VIIRS_Lit_up_pixel_count","VIIRS_Light_pixcel_percentage","VIIRS_Mean","VIIRS_Min","VIIRS_Max","VIIRS_Median","VIIRS_Std_Dev","VIIRS_Sum","VIIRS_Mode","Census_Code1","State_Name1","State_Census_Cd1","District_Name1","District_Census_ID1","Area1","Year1","NR_Link1","NR_Data_Source1","NR_Count1","NR_Lit_up_pixel_count1","NR_Light_pixcel_percentage1","NR_Mean1","NR_Min1","NR_Max1","NR_Median1","NR_Std_Dev1","NR_Sum1","NR_Mode1","NRD_Link1","NRD_Data_Source1","NRD_Count1","NRD_Lit_up_pixel_count1","NRD_Light_pixcel_percentage1","NRD_Mean1","NRD_Min1","NRD_Max1","NRD_Median1","NRD_Std_Dev1","NRD_Sum1","NRD_Mode1","Rad_Link1","Rad_Data_Source1","Rad_Count1","Rad_Lit_up_pixel_count1","Rad_Light_pixcel_percentage1","Rad_Mean1","Rad_Min1","Rad_Max1","Rad_Median1","Rad_Std_Dev1","Rad_Sum1","Rad_Mode1","VIIRS_Link1","VIIRS_Data_Source1","VIIRS_Count1","VIIRS_Lit_up_pixel_count1","VIIRS_Light_pixcel_percentage1","VIIRS_Mean1","VIIRS_Min1","VIIRS_Max1","VIIRS_Median1","VIIRS_Std_Dev1","VIIRS_Sum1","VIIRS_Mode1") # Change column header<br/>

levels(CS$Year)<-unique(c(levels(CS$Year),levels(CS$Year1)))<br/> 
levels(CS$Year)<br/>
  [1] "1992" "1993" "1994" "1995" "1996" "1997" "1998" "1999" "2000" "2001"<br/>
 [11] "2002" "2003" "2004" "2005" "2006" "2007" "2008" "2009" "2010" "2011"<br/>
 [21] "2012" "2013"<br/>

for(d in 1:lengths(CS[1], use.names = FALSE)) # Run the loop from 1 to length(CS)<br/>
{<br/>
  if(is.na(CS[d,"Census_Code"])) # Enter the loop if the CS contained d or Census_Code<br/> 
  {<br/>
    CS[d,"Census_Code"]<-CS[d,"Census_Code1"]  # Assign the Census_Code1 to Census_Code<br/>
    CS[d,"State_Name"]<-CS[d,"State_Name1"]<br/>  
    CS[d,"State_Census_Cd"]<-CS[d,"State_Census_Cd1"]<br/> 
    CS[d,"District_Name"]<-CS[d,"District_Name1"]<br/> 
    CS[d,"District_Census_ID"]<-CS[d,"District_Census_ID1"]<br/> 
    CS[d,"Area"]<-CS[d,"Area1"] ## Rename the codes<br/>
    CS[d,"Year"]<-CS[d,"Year1"] ## Rename the codes<br/>
  }<br/>
}<br/>

CS["Rad_Link"]<-CS["Rad_Link1"] # Assign the Rad_Link 1to Rad_Link<br/>
CS["Rad_Data_Source"]<-CS["Rad_Data_Source1"]<br/>
CS["Rad_Count"]<-CS["Rad_Count1"]<br/>
CS["Rad_Lit_up_pixel_count"]<-CS["Rad_Lit_up_pixel_count1"]<br/> 
CS["Rad_Light_pixcel_percentage"]<-CS["Rad_Light_pixcel_percentage1"]<br/> 
CS["Rad_Mean"]<-CS["Rad_Mean1"] ## Rename the codes<br/>
CS["Rad_Min"]<-CS["Rad_Min1"]<br/>
CS["Rad_Max"]<-CS["Rad_Max1"]<br/>
CS["Rad_Median"]<-CS["Rad_Median1"]<br/>
CS["Rad_Std_Dev"]<-CS["Rad_Std_Dev1"]<br/>
CS["Rad_Sum"]<-CS["Rad_Sum1"]<br/>
CS["Rad_Mode"]<-CS["Rad_Mode1"]<br/>

CSK<-CS[1:56] #Assign 1 to 56 columns of CS to CSK<br/>

ES<-merge(CSK,VRS,by="BQ" , all = TRUE )  # Merge "AS" variable with Radiance data<br/>
colnames(ES) <-<br/> c("BQ","Census_Code","State_Name","State_Census_Cd","District_Name","District_Census_ID","Area","Year","NR_Link","NR_Data_Source","NR_Count","NR_Lit_up_pixel_count","NR_Light_pixcel_percentage","NR_Mean","NR_Min","NR_Max","NR_Median","NR_Std_Dev","NR_Sum","NR_Mode","NRD_Link","NRD_Data_Source","NRD_Count","NRD_Lit_up_pixel_count","NRD_Light_pixcel_percentage","NRD_Mean","NRD_Min","NRD_Max","NRD_Median","NRD_Std_Dev","NRD_Sum","NRD_Mode","Rad_Link","Rad_Data_Source","Rad_Count","Rad_Lit_up_pixel_count","Rad_Light_pixcel_percentage","Rad_Mean","Rad_Min","Rad_Max","Rad_Median","Rad_Std_Dev","Rad_Sum","Rad_Mode","VIIRS_Link","VIIRS_Data_Source","VIIRS_Count","VIIRS_Lit_up_pixel_count","VIIRS_Light_pixcel_percentage","VIIRS_Mean","VIIRS_Min","VIIRS_Max","VIIRS_Median","VIIRS_Std_Dev","VIIRS_Sum","VIIRS_Mode","Census_Code1","State_Name1","State_Census_Cd1","District_Name1","District_Census_ID1","Area1","Year1","NR_Link1","NR_Data_Source1","NR_Count1","NR_Lit_up_pixel_count1","NR_Light_pixcel_percentage1","NR_Mean1","NR_Min1","NR_Max1","NR_Median1","NR_Std_Dev1","NR_Sum1","NR_Mode1","NRD_Link1","NRD_Data_Source1","NRD_Count1","NRD_Lit_up_pixel_count1","NRD_Light_pixcel_percentage1","NRD_Mean1","NRD_Min1","NRD_Max1","NRD_Median1","NRD_Std_Dev1","NRD_Sum1","NRD_Mode1","Rad_Link1","Rad_Data_Source1","Rad_Count1","Rad_Lit_up_pixel_count1","Rad_Light_pixcel_percentage1","Rad_Mean1","Rad_Min1","Rad_Max1","Rad_Median1","Rad_Std_Dev1","Rad_Sum1","Rad_Mode1","VIIRS_Link1","VIIRS_Data_Source1","VIIRS_Count1","VIIRS_Lit_up_pixel_count1","VIIRS_Light_pixcel_percentage1","VIIRS_Mean1","VIIRS_Min1","VIIRS_Max1","VIIRS_Median1","VIIRS_Std_Dev1","VIIRS_Sum1","VIIRS_Mode1") # Change column header<br/>

levels(ES$Year)<-unique(c(levels(ES$Year),levels(ES$Year1)))<br/> 
levels(ES$Year)<br/>
  [1] "1992" "1993" "1994" "1995" "1996" "1997" "1998" "1999" "2000" "2001"<br/>
 [11] "2002" "2003" "2004" "2005" "2006" "2007" "2008" "2009" "2010" "2011"<br/>
 [21] "2012" "2013" "2014" "2015" "2016"<br/>

for(z in 1:lengths(ES[1], use.names = FALSE))  # Run the loop from 1 to length(ES)<br/>
{<br/>
  if(is.na(ES[z,"Census_Code"]))  # Enter the loop if the ES contained z or Census_Code<br/>
  {<br/>
    ES[z,"Census_Code"]<-ES[z,"Census_Code1"] # Assign the Census_Code1 to Census_Code<br/>
    ES[z,"State_Name"]<-ES[z,"State_Name1"]<br/>
    ES[z,"State_Census_Cd"]<-ES[z,"State_Census_Cd1"]<br/>
    ES[z,"District_Name"]<-ES[z,"District_Name1"]<br/>
    ES[z,"District_Census_ID"]<-ES[z,"District_Census_ID1"]<br/>
    ES[z,"Area"]<-ES[z,"Area1"]<br/>
    ES[z,"Year"]<-ES[z,"Year1"]<br/>
  }<br/>
}<br/>

ES["VIIRS_Link"]<-ES["VIIRS_Link1"] # Assign the VIIRS_Link1 to VIIRS_Link<br/>
ES["VIIRS_Data_Source"]<-ES["VIIRS_Data_Source1"]<br/>
ES["VIIRS_Count"]<-ES["VIIRS_Count1"]<br/>
ES["VIIRS_Lit_up_pixel_count"]<-ES["VIIRS_Lit_up_pixel_count1"]<br/>
ES["VIIRS_Light_pixcel_percentage"]<-ES["VIIRS_Light_pixcel_percentage1"]<br/>
ES["VIIRS_Mean"]<-ES["VIIRS_Mean1"]<br/>
ES["VIIRS_Min"]<-ES["VIIRS_Min1"]<br/>
ES["VIIRS_Max"]<-ES["VIIRS_Max1"]<br/>
ES["VIIRS_Median"]<-ES["VIIRS_Median1"]<br/>
ES["VIIRS_Std_Dev"]<-ES["VIIRS_Std_Dev1"]<br/>
ES["VIIRS_Sum"]<-ES["VIIRS_Sum1"]<br/>
ES["VIIRS_Mode"]<-ES["VIIRS_Mode1"]<br/>

ESK<-ES[1:56] # Assign 1 to 56 columns of ES to ESK<br/>

colnames(ESK) <-<br/> c("System_Gen_UID","Census_Code","State_Name","State_Census_Cd","District_Name","District_Census_ID","Area","Year","NR_Link","NR_Data_Source","NR_Count","NR_Lit_up_pixel_count","NR_Light_pixcel_percentage","NR_Mean","NR_Min","NR_Max","NR_Median","NR_Std_Dev","NR_Sum","NR_Mode","NRD_Link","NRD_Data_Source","NRD_Count","NRD_Lit_up_pixel_count","NRD_Light_pixcel_percentage","NRD_Mean","NRD_Min","NRD_Max","NRD_Median","NRD_Std_Dev","NRD_Sum","NRD_Mode","Rad_Link","Rad_Data_Source","Rad_Count","Rad_Lit_up_pixel_count","Rad_Light_pixcel_percentage","Rad_Mean","Rad_Min","Rad_Max","Rad_Median","Rad_Std_Dev","Rad_Sum","Rad_Mode","VIIRS_Link","VIIRS_Data_Source","VIIRS_Count","VIIRS_Lit_up_pixel_count","VIIRS_Light_pixcel_percentage","VIIRS_Mean","VIIRS_Min","VIIRS_Max","VIIRS_Median","VIIRS_Std_Dev","VIIRS_Sum","VIIRS_Mode") # Change column names<br/>

**Write output to the csv file**

write.csv(ESK,"D:/IDFC work/Bulk Zonal Stat Calculation/INPUT/R_Script_Directory/29_admin_zs.csv", na="NA") # Enter Output csv file name and path<br/>

 
 



