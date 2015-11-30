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
library(zoo)

## 2 ## Load and Setup Data ######################

## SFPD Traffic Incidents ####
# Source: Open Data SF 
TrafficViolation<- read.csv("SFPD_Incidents_Accidents.csv", stringsAsFactors = FALSE) #load data

TrafficViolation$DateTime <- mdy_hm(paste(TrafficViolation$Date, TrafficViolation$Time)) # Merge date and time and convert to date & time format
TrafficViolation$Month <- month(TrafficViolation$DateTime) # Separate out Month
TrafficViolation$Year <- year(TrafficViolation$DateTime) # separate out Year
TrafficViolation$MonthYear <- as.yearmon(TrafficViolation$DateTime)

TrafficViolation$Location <- gsub("\\(|\\)", "", TrafficViolation$Location)  #remove "(" and ")"
TrafficViolation <- TrafficViolation %>% separate(col = Location, into = c("lat", "lon"), sep = ",") #split latitude and longitude
TrafficViolation$lat <- as.numeric(TrafficViolation$lat) #convert to integer
TrafficViolation$lon <- as.numeric(TrafficViolation$lon) #convert to integer
TrafficViolation <- subset(TrafficViolation, lat < 90) # remove incorrectly recorded locations

# Subset data to accidents and hit & runs
# Subset by day of week (business week, without holidays)
# Subset by time of day (commute hours)

### create spatial data frame for Accidents
TrafficSP <- TrafficViolation
coordinates(TrafficSP) <- c("lon", "lat") #transform to Spatial Data Frame
proj4string(TrafficSP) <- CRS("+init=epsg:4326") ## Find corrrect SOURCE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
TrafficSP <- spTransform(TrafficSP, CRS( "+init=epsg:3347" ) )  #transform

#Subset Traffic Violations removing Saturday and sunday
WeekdayViolation <- subset(TrafficViolation, DayOfWeek != "Saturday" & DayOfWeek != "Sunday")
#Further Subset by only keeping times between 6am-10am and 3p-8p
RushHourAM <- subset(WeekdayViolation, hour(DateTime) >= 6 & hour(DateTime) <= 9)
RushHourPM <- subset(WeekdayViolation, hour(DateTime) >= 15 & hour(DateTime) <= 19)
RushHour <- rbind(RushHourAM, RushHourPM) # Rush hour dataframes stacked


## Load SF Bus Stop locations 
# http://www.gtfs-data-exchange.com/agency/san-francisco-municipal-transportation-agency/
BusStops <- read.csv("stops.csv", header = TRUE)
BusStops <- subset(BusStops, stop_name!="REFERENCE & REFERENCE")#remove reference
head(BusStops)
BusStops <- rename(BusStops, lon = stop_lon, lat = stop_lat) #rename lat and lon
BusStops$stop_id <- as.factor(BusStops$stop_id)
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



### Set Geographic Boundaries and Create Panel Data Set ####################

#set bufffer/radisu
StopRadius <- 60.96 #unit of measure is meters, 200 feet measure
BusBuffer <- gBuffer(BusSP, width = StopRadius, byid = TRUE)
head(BusBuffer)
plot(BusBuffer)
points(TrafficSP, col="blue", cex=0.1)

# Intersections between traffic sp and bus buffer

test <- TrafficSP[BusBuffer,]
plot(TrafficSP)
points(test, col = "light blue")
test.count <- aggregate(TrafficSP, BusBuffer, length)
head(test.count@data)

x <- BusStops
head(x)

y <- test.count@data

intersections <- gIntersects(TrafficSP, BusBuffer, byid = TRUE)
dim(intersections) #confirm rows = bus stop count
head(apply(intersections, MARGIN = 2, FUN = which))
Bstop.indexes <- which(intersections, arr.ind = TRUE) #setup array
summary(Bstop.indexes) #note rows line up with BUs Stops
Bstop.names <- BusStops$stop_name[Bstop.indexes[,1]] #match names from origional bust stop data frame
Bstop.count <- aggregate(Bstop.indexes ~ Bstop.names, FUN = length) #count number of occurrences of True (traffic violation) for each stop
head(Bstop.count)


#clipped <- apply(intersections == F, MARGIN = 2, all) #clip outside
#plot(clipped, col="light blue")
#plot(TrafficSP[which(clipped),])
#traffic.cl <- TrafficSP[which(!clipped),] 
#head(traffic.cl)
#points(traffic.cl, col="green")

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






### Models #########################################
FEmodel <- plm(violations ~ Private + month1 + month2 ... month12 + weather, index=c("Stop_ID", "MonthYear"), model="within", data= x) # fixed effects model



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
