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
require(rgeos)
library(GISTools)
#################################################################################################################################################
memory.limit(size=100000)
#CSV to Point Shapefile
MyData <- read.csv(file="D:/#IDFC# classification/GDP/GDP dataset/Raster dataset/GDP_global_envi_research/GDP/Raster/ssp2/gdp_ssp2.csv", header=TRUE, sep=",")
coordinates(MyData)<-~px+py
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(mapG) <- crs.geo 


#################################################################################################################################################
#Convert point Shapefile in to Raster

r <- raster()
extent(r) <- extent(MyData)
res(r)=0.50
A <- rasterize(a, r, fun = sum, 'g3_1980')
B <- rasterize(a, r, fun = sum, 'g3_1990')
C <- rasterize(a, r, fun = sum, 'g3_2000')
D <- rasterize(a, r, fun = sum, 'g3_2010')
E <- rasterize(a, r, fun = sum, 'g3_2020')
F <- rasterize(a, r, fun = sum, 'g3_2030')
G <- rasterize(a, r, fun = sum, 'g3_2040')
H <- rasterize(a, r, fun = sum, 'g3_2050')
I <- rasterize(a, r, fun = sum, 'g3_2060')
J <- rasterize(a, r, fun = sum, 'g3_2070')
K <- rasterize(a, r, fun = sum, 'g3_2080')
L <- rasterize(a, r, fun = sum, 'g3_2090')
M <- rasterize(a, r, fun = sum, 'g3_2100')


#################################################################################################################################################
#zonal stat
Zone1<-readOGR("D:/IDFC work/raja/GDP/SHP","AllIndiaClass1_563_TownBoundaries2014_15_reproject") # To read shapesile (zone)

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

z1 <- Zone1@data
output <- cbind(z1,out1,out2,out3,out4,out5,out6,out7,out8)
write.csv(output,"D:/IDFC work/raja/GDP/Output/MER_output.csv", na="NA") # Enter Output csv file name and path

