library(vegan)
library(maptools)
library(RColorBrewer)
library(mapplots)
library(GISTools)
complete = read.csv(file="complete.csv", header=TRUE)
#mapping the capture localities of the NECH
library(maps)
library(mapdata)
library(scales)
par(mfrow=c(1,1))
map("worldHires", xlim=c(-90,-30), ylim=c(-30,12), col="gray95", fill=TRUE)
NECHrange <- readShapePoly("species_22701173")  #layer of data for species range
PEARrange<- readShapePoly("species_22730401") #PEAR layer
MYCSrange <- readShapePoly("species_22733153") #Myrmeciza 
NESU <- readShapePoly("species_22701178") #Neopelma sulphureiventer 
NEPA <- readShapePoly("species_22701183") #Neopelma pallescens
NECHRY <- readShapePoly("species_22724481") #Neopelma chrysolophum
NEAU <- readShapePoly("species_22701188") #Neopelma aurifrons
plot(NECHrange, add=TRUE, col=alpha("darkgreen", 0.6), border=FALSE)  #plot the species range
plot(NESU, add=TRUE, col="dark red" )
plot(NEPA, add=TRUE, col="hot pink" )
plot(NECHRY, add=TRUE, col="yellow")
plot(NEAU, add=TRUE, col="blue")
plot(PEARrange, add=TRUE, col="dark red")
plot(MYCSrange, add=TRUE, col="orange")
# plot points
NECH = complete[ which(complete$Codigo == "N" & complete$Codigo.especie == "NECH"),]
NECHloc <- subset(NECH, !is.na(lon) & !is.na(lat))
dim(NECHloc)
NECHall = complete[ which(complete$Codigo.especie == "NECH"),]
NECHall[,c(1:8, 42:43)]
write.csv(NECHall[,2:5], file ="new_NECH.csv")

points(NECH$lon, NECH$lat, col="black", pch=20, cex=0.95)
box() # restore the box around the map
#library(RgogleMaps)
# Mapping via RgoogleMaps
# Find map center and get map
#center <- rev(sapply(rawdata, mean))
#map <- GetMap(center=center, zoom=11)
# Translate original data
#coords <- LatLon2XY.centered(map, rawdata$lat, rawdata$lon, 11)
#coords <- data.frame(coords)