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
# 3. Set Geographic Boundaries and Panel Data Set
# 4. Model
# 5. Visualizations 

## 1 ## Set Workspace ############################
rm(list = ls()) #clear workspace
# Load Packages
library(sp) #spatial pacakge
library(plyr)
library(reshape2)
library(ggmap)
library(lubridate)

## 2 ## Load and Setup Data ######################

## SFPD Traffic Incidents ####
# Source: Open Data SF 
TrafficIncidents <- read.csv("SFPD_Incidents_Accidents.csv", stringsAsFactors = FALSE) #load data
summary(TrafficIncidents) #quick summary
head(TrafficIncidents) #header view

TrafficIncidents$DateTime <- mdy_hm(paste(TrafficIncidents$Date, TrafficIncidents$Time)) # Merge date and time and convert to date & time format

TrafficIncidents$Location <- gsub("\\(|\\)", "", TrafficIncidents$Location)  #remove "(" and ")"
TrafficIncidents <- transform(TrafficIncidents, Location = colsplit(TrafficIncidents$Location, pattern = ",", c("lat", "lon"))) #split latitude and longitude
names(TrafficIncidents)



map <- get_map(location = "San Francisco", zoom = 13, source = "stamen", maptype = "terrain") #create map
ggmap(map) #review map

mapPoints <- ggmap(map) +
        geom_point(aes(x = Location$lon, y = Location$lat, alpha = 0.0001, shape = Category), data = TrafficIncidents) # quick overview map
mapPoints #call map
 



# COnvert date and time to date & time format
# Subset data to accidents and hit & runs
# Subset by day of week (business week, without holidays)
# Subset by time of day (commute hours)

## Load SF Bus Stop locations 
# http://www.gtfs-data-exchange.com/agency/san-francisco-municipal-transportation-agency/
BusStops <- read.csv("stops.csv", header = TRUE)
head(BusStops)

## Load SF Semi-private bus stop locations
# source: https://www.sfmta.com/sites/default/files/projects/2015/Shuttles%20Network%20150818%20%28list%29.pdf
SemiPrivateStop <- read.csv("CommuterShuttlePilotStops.csv", stringsAsFactors = FALSE)
SemiPrivateStop <- SemiPrivateStop[,-4] #remove double zone column
SemiPrivateStop <- rename(SemiPrivateStop, c("Designated.Stop.Location"="stop_id", "Muni"="Zone")) #rename stops and Muni/White column "Zone"
SemiPrivateStop$Address <- paste(SemiPrivateStop$stop_id, SemiPrivateStop$City, sep = ", ") #merge street and city into address column
SemiPrivateStop$Geocode <- geocode(SemiPrivateStop$Address)#get lat and lon data by address


names(SemiPrivateStop)
head(SemiPrivateStop)




Stops <- merge(BusStops, SemiPrivateStop, by="stop_id", all=TRUE) #outer join of busstops and semiprivate stops into one data frame
head(Stops) ### needs to be fixed


## Weather by Day in SF
# NOAA Rainfall and Temperature 1/1/2003 to 11/1/2015 Downtown SF: http://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USW00023272/detail
weather <- read.csv("SF Weather 2003 to 2015.csv", stringsAsFactors = FALSE)
summary(weather)
 #convert to date format


# Set variable for stop geographic boundary (easier to adjust for sensitivity analsis)



### Models #########################################




### Visualizations #################################
