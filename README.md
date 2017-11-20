Zonal Statistics
load required libraries and packages
library(rgdal)
## Loading required package: sp
## rgdal: version: 1.1-10, (SVN revision 622)
##  Geospatial Data Abstraction Library extensions to R successfully loaded
##  Loaded GDAL runtime: GDAL 2.0.1, released 2015/09/15
##  Path to GDAL shared files: C:/Program Files/R/R-3.3.2/library/rgdal/gdal
##  Loaded PROJ.4 runtime: Rel. 4.9.2, 08 September 2015, [PJ_VERSION: 492]
##  Path to PROJ.4 shared files: C:/Program Files/R/R-3.3.2/library/rgdal/proj
##  Linking to sp version: 1.2-3
library(maptools)
## Checking rgeos availability: TRUE
library(lattice)
library(proj4)
## 
## Attaching package: 'proj4'
## The following object is masked from 'package:rgdal':
## 
##     project
require(raster)
## Loading required package: raster
library(geoR)
## --------------------------------------------------------------
##  Analysis of Geostatistical Data
##  For an Introduction to geoR go to http://www.leg.ufpr.br/geoR
##  geoR version 1.7-5.2 (built on 2016-05-02) is now loaded
## --------------------------------------------------------------
library(xtable)
## 
## Attaching package: 'xtable'
## The following object is masked from 'package:maptools':
## 
##     label
library(fields)
## Loading required package: spam
## Loading required package: grid
## Spam version 1.4-0 (2016-08-29) is loaded.
## Type 'help( Spam)' or 'demo( spam)' for a short introduction 
## and overview of this package.
## Help for individual functions is also obtained by adding the
## suffix '.spam' to the function name, e.g. 'help( chol.spam)'.
## 
## Attaching package: 'spam'
## The following object is masked from 'package:xtable':
## 
##     display
## The following objects are masked from 'package:base':
## 
##     backsolve, forwardsolve
## Loading required package: maps
## Warning: package 'maps' was built under R version 3.3.3
require(gstat)
## Loading required package: gstat
require(spatstat)
## Loading required package: spatstat
## Loading required package: nlme
## 
## Attaching package: 'nlme'
## The following object is masked from 'package:raster':
## 
##     getData
## Loading required package: rpart
## 
## spatstat 1.47-0       (nickname: 'Responsible Gambler') 
## For an introduction to spatstat, type 'beginner'
## 
## Note: R version 3.3.2 (2016-10-31) is more than 9 months old; we strongly recommend upgrading to the latest version
## 
## Attaching package: 'spatstat'
## The following object is masked from 'package:gstat':
## 
##     idw
## The following objects are masked from 'package:raster':
## 
##     area, rotate, shift
## The following object is masked from 'package:lattice':
## 
##     panel.histogram
require(tiff)
## Loading required package: tiff
require(sp)
require(doBy)
## Loading required package: doBy
require(data.table)
## Loading required package: data.table
## 
## Attaching package: 'data.table'
## The following object is masked from 'package:spatstat':
## 
##     shift
## The following object is masked from 'package:raster':
## 
##     shift
require(modeest)
## Loading required package: modeest
## 
## This is package 'modeest' written by P. PONCET.
## For a complete list of functions, use 'library(help = "modeest")' or 'help.start()'.
require(foreign)
## Loading required package: foreign
Setting the memory limit
memory.limit(size = 100000)
## [1] 1e+05
To read shapesile and assign to a variable zone
Zone<-readOGR("D:/K/New folder/R Markdown/Input/SHP","State") 
## OGR data source with driver: ESRI Shapefile 
## Source: "D:/K/New folder/R Markdown/Input/SHP", layer: "State"
## with 36 features
## It has 4 fields
plot(Zone)
 
To read Raster data and assign to a variable Lumin and user can change the data as per their requirement
Lumin <- raster("D:/K/New folder/R Markdown/Input/Economic data/gdp90_15mi.tif")
plot(Lumin)
 
Calculate sum of the values of the raster data falling in the zone
out <- extract(Lumin, Zone, fun = sum, na.rm = T, small = T, df = T)
Assign the values 0 to NA
out[out == 0] <- NA
Extract the attributes from zone
z <- Zone@data
Bind the extracted attributes and the output
M <- cbind(z,out)
Write the output to the CSV format
write.csv(M,"D:/K/New folder/R Markdown/Input/Economic data/zonal_stat_GPW_2020_count_Dist_2001.csv", na="NA") # Enter Output csv file name and path
