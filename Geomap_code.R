#Use downloaded shapefiles to create Geomaps
# install packages
# run libraries
#install packages
install.packages("sf")
install.packages("raster")
install.packages("dplyr")
install.packages("spData")
install.packages("tmap")
install.packages("ggplot2")
install.packages("rgdal")
install.packages("png")
install.packages("rgeos")
install.package("rio")

#libraries
library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)    # for static and interactive maps
library(ggplot2) # tidyverse data visualization package
library(rgdal)
library(png)
library(rgeos)
library(rio)

#download shape files to directory
#change the directory location below
shp2<- shapefile("C:/Users/44787/OneDrive - University of Derby/Desktop/RStudio/crimes/shapefiles/Lower_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.shx")


#import your data
regions <- rio::import("C:/Users/44787/OneDrive - University of Derby/Desktop/RStudio/crimes/AssessmentCrimeData.csv")

regions$District = sub("(\\D+\\s).*","\\1",regions$Name)
View(regions)
#make sure that the column name for LSOA codes  match with the name in the shapefiles. In this case the columns name is "LSOA11CD".
shp_main<-subset(shp2, LSOA11CD %in% regions$LSOA)
View(shp_main)
#add variables to the shapefile
shp_main$"LSOA Code"<-regions$LSOA11CD
shp_main$"Region Name"<-regions$Name
shp_main$"Region District"<-regions$District

##############
#adding my variables to shapefiles
shp_main$"log10(ASB Density)"<- log10(regions$Burglary/regions$`Land Area in Hectares`)
shp_main$"log10(Burglary Density)"<- log10(regions$Burglary/regions$`Land Area in Hectares`)
shp_main$"log10(Robbery Density)"<- log10(regions$Robbery/regions$`Land Area in Hectares`)
shp_main$"log10(Vehicle crime Density)"<- log10(regions$`Vehicle Crimes`/regions$`Land Area in Hectares`)
shp_main$"log10(Violent crime Density)"<- log10(regions$`Violent Crimes`/regions$`Land Area in Hectares`)
shp_main$"log10(Shoplifting Density)"<- log10(regions$Shoplifting/regions$`Land Area in Hectares`)
shp_main$"log10(Criminal damage&Arson Density)"<- log10(regions$`Criminal Damage & Arson`/regions$`Land Area in Hectares`)
shp_main$"log10(other theft Density)"<- log10(regions$`Other Theft`/regions$`Land Area in Hectares`)
shp_main$"log10(Drugs Density)"<- log10(regions$Drugs/regions$`Land Area in Hectares`)
shp_main$"log10(Other crimes Density)"<- log10(regions$`Other Crimes`/regions$`Land Area in Hectares`)
shp_main$"log10(Bike theft Density)"<- log10(regions$`Bike Theft`/regions$`Land Area in Hectares`)
shp_main$"log10(Posession of weapons Density)"<- log10(regions$`Possession of Weapons`/regions$`Land Area in Hectares`)
shp_main$"log10(Theft from a person Density)"<- log10(regions$`Public Order`/regions$`Land Area in Hectares`)
##
shp_main$"log10(population Density)"<- log10(regions$`Population`/regions$`Land Area in Hectares`)
shp_main$"log10(population)"<- log10(regions$`Population`)



#MY MAP
###1
tm_shape(shp_main) +
  
  tm_polygons ("log10(ASB Density)") +
  tm_layout (legend.outside = TRUE,legend.outside.position = "right")+
  tm_borders(lwd=1)
#2
tm_shape(shp_main) +
  
  tm_polygons ("log10(Burglary Density)") +
  tm_layout (legend.outside = TRUE,legend.outside.position = "right")+
  tm_borders(lwd=1)

###3
tm_shape(shp_main) +
  
  tm_polygons ("log10(Robbery Density)") +
  tm_layout (legend.outside = TRUE,legend.outside.position = "right")+
  tm_borders(lwd=1)

#4
tm_shape(shp_main) +
  
  tm_polygons ("log10(Vehicle crime Density)") +
  tm_layout (legend.outside = TRUE,legend.outside.position = "right")+
  tm_borders(lwd=1)

###5
tm_shape(shp_main) +
  
  tm_polygons ("log10(Violent Crime Density)") +
  tm_layout (legend.outside = TRUE,legend.outside.position = "right")+
  tm_borders(lwd=1)
##6
tm_shape(shp_main) +
  
  tm_polygons ("log10(Shoplifting Density)") +
  tm_layout (legend.outside = TRUE,legend.outside.position = "right")+
  tm_borders(lwd=1)

######7
tm_shape(shp_main) +

tm_polygons ("log10(Criminal Damage & Arson Density)") +
  tm_layout (legend.outside = TRUE,legend.outside.position = "right")+
  tm_borders(lwd=1)

###8
tm_shape(shp_main) +

tm_polygons ("log10(Othertheft Density)") +
  tm_layout (legend.outside = TRUE,legend.outside.position = "right")+
  tm_borders(lwd=1)
###9
tm_shape(shp_main) +
  
  tm_polygons ("log10(Drugs Density)") +
  tm_layout (legend.outside = TRUE,legend.outside.position = "right")+
  tm_borders(lwd=1)

##10
tm_shape(shp_main) +
  
  tm_polygons ("log10(Other crimes Density)") +
  tm_layout (legend.outside = TRUE,legend.outside.position = "right")+
  tm_borders(lwd=1)
##11

tm_shape(shp_main) +

tm_polygons ("log10(Bike Theft Density)") +
  tm_layout (legend.outside = TRUE,legend.outside.position = "right")+
  tm_borders(lwd=1)

#12
tm_shape(shp_main) +

tm_polygons ("log10(POW Density)") +
  tm_layout (legend.outside = TRUE,legend.outside.position = "right")+
  tm_borders(lwd=1)

#13
tm_shape(shp_main) +

tm_polygons ("log10(Public order Density)") +
  tm_layout (legend.outside = TRUE,legend.outside.position = "right")+
  tm_borders(lwd=1)

###14
tm_shape(shp_main) +
  
  tm_polygons ("log10(theft.from.a.person Density)") +
  tm_layout (legend.outside = TRUE,legend.outside.position = "right")+
  tm_borders(lwd=1)
##### poulation density map
tm_shape(shp_main) +
  
  tm_polygons ("log10(population Density)", palette = "YlGn") +
  tm_layout(legend.outside = TRUE,legend.outside.position = "right") +
  tm_borders(lwd=1)
######population map
tm_shape(shp_main) +
  
  tm_polygons ("Population", palette = "YlGn") +
  tm_layout(legend.outside = TRUE,legend.outside.position = "right") +
  tm_borders(lwd=1)
