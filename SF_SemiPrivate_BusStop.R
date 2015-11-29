##################################################
##     SF Semi-Private Bus Stop                ##
################################################
# SF Commutter Bus Trial 2014-2015           

# 1. Set Workspace
# 2. Load and Setup Data
        # Traffic Incident Data
        # Bus Stop Location
                # Full Stop List
                # Semi-Private Stops
        # Weather Data
        # Distance Matrix
# 3. Set Geographic Boundaries and Panel Data Set
# 4. Model
# 5. Visualizations 

## 1 ## Set Workspace ############################
rm(list = ls()) #clear workspace

# Load Packages
library(sp) #spatial pacakge
library(rgdal)
library(plyr)
library(reshape2)
library(ggmap)
library(lubridate)
library(fields)
library(scales)
library(rgeos)
library(tidyr)
library(dplyr)

## 2 ## Load and Setup Data ######################

## SFPD Traffic Incidents ####
# Source: Open Data SF 
TrafficIncidents <- read.csv("SFPD_Incidents_Accidents.csv", stringsAsFactors = FALSE) #load data

TrafficIncidents$DateTime <- mdy_hm(paste(TrafficIncidents$Date, TrafficIncidents$Time)) # Merge date and time and convert to date & time format

TrafficIncidents$Location <- gsub("\\(|\\)", "", TrafficIncidents$Location)  #remove "(" and ")"

TrafficIncidents <- TrafficIncidents %>% separate(col = Location, into = c("lat", "lon"), sep = ",") #split latitude and longitude
TrafficIncidents$lat <- as.numeric(TrafficIncidents$lat) #convert to integer
TrafficIncidents$lon <- as.numeric(TrafficIncidents$lon) #convert to integer

# Subset data to accidents and hit & runs
# Subset by day of week (business week, without holidays)
# Subset by time of day (commute hours)

### create spatial data frame for Accidents
TrafficSP <- TrafficIncidents 
coordinates(TrafficSP) <- c("lon", "lat") #transform to Spatial Data Frame
proj4string(TrafficSP) <- CRS("+init=epsg:4326") ## Find corrrect SOURCE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
TrafficSP <- spTransform(TrafficSP, CRS( "+init=epsg:3347" ) )  #transform


## Load SF Bus Stop locations 
# http://www.gtfs-data-exchange.com/agency/san-francisco-municipal-transportation-agency/
BusStops <- read.csv("stops.csv", header = TRUE)
BusStops <- subset(BusStops, stop_name!="REFERENCE & REFERENCE")#remove reference
head(BusStops)
BusStops <- rename(BusStops, lon = stop_lon, lat = stop_lat) #rename lat and lon
BusSP <- BusStops #seperate date frame
coordinates(BusSP) <- c("lon", "lat") #transform to spatial
proj4string(BusSP) <- CRS("+init=epsg:4326") ## Find corrrect SOURCE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
BusSP <- spTransform(BusSP, CRS( "+init=epsg:3347" ) )  #transform
summary(BusSP)
plot(BusSP) #plot of bus stops

## Load SF Semi-private bus stop locations
# source: https://www.sfmta.com/sites/default/files/projects/2015/Shuttles%20Network%20150818%20%28list%29.pdf
#SemiPrivateStop <- read.csv("CommuterShuttlePilotStops.csv", stringsAsFactors = FALSE)
#SemiPrivateStop <- SemiPrivateStop[,-4] #remove double zone column
#SemiPrivateStop <- rename(SemiPrivateStop, c("Designated.Stop.Location"="stop_id", "Muni"="Zone")) #rename stops and Muni/White column "Zone"
#SemiPrivateStop$Address <- paste(SemiPrivateStop$stop_id, SemiPrivateStop$City, sep = ", ") #merge street and city into address column
#SemiPrivateStop$Geocode <- geocode(SemiPrivateStop$Address)#get lat and lon data by address


### Counts of incidents within 10 meters

#set bufffer
StopRadius <- 61 #unit of measure is meters, 200 feet measure
BusBuffer <- gBuffer(BusSP, width = StopRadius, byid = TRUE)
plot(BusBuffer)

#count observations in buffer
InsideBusArea <- which(gContains(BusBuffer, TrafficSP, byid = TRUE))
AccidentInsideRadius <- TrafficSP[InsideBusArea,]
length(InsideBusArea) #count of incidents inside bus area, 75,079. There is overlap in Stop Areas downtown and elsewhere.


#Stops <- merge(BusStops, SemiPrivateStop, by="stop_id", all=TRUE) #outer join of busstops and semiprivate stops into one data frame
#head(Stops) ### needs to be fixed


## Weather by Day in SF
# NOAA Rainfall and Temperature 1/1/2003 to 11/1/2015 Downtown SF: http://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USW00023272/detail
weather <- read.csv("SF Weather 2003 to 2015.csv", stringsAsFactors = FALSE)
summary(weather)
 #convert to date format


# Set variable for stop geographic boundary (easier to adjust for sensitivity analsis)



### Models #########################################




### Visualizations #################################

# All traffic incidents
palette(rainbow(10))
map <- get_map(location = "San Francisco", zoom = 13, source = "stamen", maptype = "terrain") #create map
ggmap(map) #review map

mapPoints <- ggmap(map) +
        geom_point(data = BusStops, aes(x = lon, y = lat)) +
        geom_point(data = TrafficIncidents, aes(x = Location$lon, y = Location$lat, alpha = 0.5, color = year(DateTime))) +
        scale_color_gradient(limits=c(2005, 2015), low="white", high="blue")
mapPoints #call map
