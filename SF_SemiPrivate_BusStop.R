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

## Weather by Day in SF
# NOAA Rainfall and Temperature 1/1/2003 to 11/1/2015 Downtown SF: http://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USW00023272/detail
weather <- read.csv("SF Weather 2003 to 2015.csv", stringsAsFactors = FALSE)
summary(weather)
#convert to date format




### Set Geographic Boundaries and Create Panel Data Set ####################

#set bufffer/radius for each bus stop
StopRadius <- 60.96 #unit of measure is meters, 60.96m = 200 ft
BusBuffer <- gBuffer(BusSP, width = StopRadius, byid = TRUE)
head(BusBuffer)
plot(BusBuffer)
points(TrafficSP, col="blue", cex=0.1)

# Intersections between traffic sp and bus buffer
test <- TrafficSP[BusBuffer,] #select incidents in bus areas
plot(TrafficSP) #view all incidents
points(test, col = "light blue") #overlay incidents wihtint bus boundaries
test.count <- aggregate(TrafficSP, BusBuffer, length) # total incidents wihin bus boundares
count(test.count@data) #count of total incidents


intersections <- gIntersects(TrafficSP, BusBuffer, byid = TRUE) # where incidents overlayy bus stop areas
dim(intersections) #confirm rows = bus stop count
head(apply(intersections, MARGIN = 1, FUN = which))

Bstop.indexes <- which(intersections, arr.ind = TRUE) #setup array
summary(Bstop.indexes) #note rows line up with BUs Stops
Bstop.names <- BusStops$stop_name[Bstop.indexes[,1]] #match names from origional bust stop data frame
Bstop.count <- aggregate(Bstop.indexes ~ Bstop.names, FUN = length) #count number of occurrences of True (traffic violation) for each stop
Bstop.count <- rename(Bstop.count, Violation_Count = row)
head(Bstop.count)

## Aggregate count for each year and month
Sub_MonthYear <- function(x, y, z) {
        d <- subset(x, Month == y & Year == z )
        return(d)
}

ViolationCount <- function(x) {
        z1SP <- x 
        coordinates(z1SP) <- c("lon", "lat") 
        proj4string(z1SP) <- CRS("+init=epsg:4326")        
        z1SP <- spTransform(z1SP, CRS( "+init=epsg:3347" ) )        
        z2 <- gIntersects(z1SP, BusBuffer, byid = TRUE)
        z3.indexes <- which(z2, arr.ind = TRUE)
        z3.names <- BusStops$stop_id[z3.indexes[,1]]
        z3.count <- aggregate(z3.indexes ~ z3.names, FUN = length)
        z3.count <- rename(z3.count, Violation_Count = row)
        z4 <- z3.count
        z4 <- rename(z4, stop_id = z3.names)
        z4 <- z4[,-3]
        z4$Year <- x$Year[1]
        z4$Month <- x$Month[1]
        return(z4)
}

MonthlyMerge <- function(x) {
        z1 <- merge(x, BusStops, by="stop_id", all = TRUE)
        z1[is.na(z1)] <- 0
        z1$Year <- x$Year[1]
        z1$Month <- x$Month[1]
        return(z1)
}

script_m_yyyy <- function(x, y) {
        z1 <- Sub_MonthYear(TrafficViolation, x, y)
        z1 <- ViolationCount(z1)
        z1 <- MonthlyMerge(z1)
        return(z1)
}

jan2015 <- script_m_yyyy(1, 2015)
feb2015 <- script_m_yyyy(2, 2015)
mar2015 <- script_m_yyyy(3, 2015)
apr2015 <- script_m_yyyy(4, 2015)
may2015 <- script_m_yyyy(5, 2015)
jun2015 <- script_m_yyyy(6, 2015)
jul2015 <- script_m_yyyy(7, 2015)
aug2015 <- script_m_yyyy(8, 2015)
sep2015 <- script_m_yyyy(9, 2015)
oct2015 <- script_m_yyyy(10, 2015)
year2015 <- rbind(jan2015, feb2015, mar2015, apr2015, may2015, jun2015, jul2015, aug2015, sep2015, oct2015)


jan2014 <- script_m_yyyy(1, 2014)
feb2014 <- script_m_yyyy(2, 2014)
mar2014 <- script_m_yyyy(3, 2014)
apr2014 <- script_m_yyyy(4, 2014)
may2014 <- script_m_yyyy(5, 2014)
jun2014 <- script_m_yyyy(6, 2014)
jul2014 <- script_m_yyyy(7, 2014)
aug2014 <- script_m_yyyy(8, 2014)
sep2014 <- script_m_yyyy(9, 2014)
oct2014 <- script_m_yyyy(10, 2014)
nov2014 <- script_m_yyyy(11, 2014)
dec2014 <- script_m_yyyy(12, 2014)
year2014 <- rbind(jan2014, feb2014, mar2014, apr2014, may2014, jun2014, jul2014, aug2014, sep2014, oct2014)

jan2013 <- script_m_yyyy(1, 2013)
feb2013 <- script_m_yyyy(2, 2013)
mar2013 <- script_m_yyyy(3, 2013)
apr2013 <- script_m_yyyy(4, 2013)
may2013 <- script_m_yyyy(5, 2013)
jun2013 <- script_m_yyyy(6, 2013)
jul2013 <- script_m_yyyy(7, 2013)
aug2013 <- script_m_yyyy(8, 2013)
sep2013 <- script_m_yyyy(9, 2013)
oct2013 <- script_m_yyyy(10, 2013)
nov2013 <- script_m_yyyy(11, 2013)
dec2013 <- script_m_yyyy(12, 2013)
year2013 <- rbind(jan2013, feb2013, mar2013, apr2013, may2013, jun2013, jul2013, aug2013, sep2013, oct2013)

jan2012 <- script_m_yyyy(1, 2012)
feb2012 <- script_m_yyyy(2, 2012)
mar2012 <- script_m_yyyy(3, 2012)
apr2012 <- script_m_yyyy(4, 2012)
may2012 <- script_m_yyyy(5, 2012)
jun2012 <- script_m_yyyy(6, 2012)
jul2012 <- script_m_yyyy(7, 2012)
aug2012 <- script_m_yyyy(8, 2012)
sep2012 <- script_m_yyyy(9, 2012)
oct2012 <- script_m_yyyy(10, 2012)
nov2012 <- script_m_yyyy(11, 2012)
dec2012 <- script_m_yyyy(12, 2012)
year2012 <- rbind(jan2012, feb2012, mar2012, apr2012, may2012, jun2012, jul2012, aug2012, sep2012, oct2012)

jan2011 <- script_m_yyyy(1, 2011)
feb2011 <- script_m_yyyy(2, 2011)
mar2011 <- script_m_yyyy(3, 2011)
apr2011 <- script_m_yyyy(4, 2011)
may2011 <- script_m_yyyy(5, 2011)
jun2011 <- script_m_yyyy(6, 2011)
jul2011 <- script_m_yyyy(7, 2011)
aug2011 <- script_m_yyyy(8, 2011)
sep2011 <- script_m_yyyy(9, 2011)
oct2011 <- script_m_yyyy(10, 2011)
nov2011 <- script_m_yyyy(11, 2011)
dec2011 <- script_m_yyyy(12, 2011)
year2011 <- rbind(jan2011, feb2011, mar2011, apr2011, may2011, jun2011, jul2011, aug2011, sep2011, oct2011)

jan2010 <- script_m_yyyy(1, 2010)
feb2010 <- script_m_yyyy(2, 2010)
mar2010 <- script_m_yyyy(3, 2010)
apr2010 <- script_m_yyyy(4, 2010)
may2010 <- script_m_yyyy(5, 2010)
jun2010 <- script_m_yyyy(6, 2010)
jul2010 <- script_m_yyyy(7, 2010)
aug2010 <- script_m_yyyy(8, 2010)
sep2010 <- script_m_yyyy(9, 2010)
oct2010 <- script_m_yyyy(10, 2010)
nov2010 <- script_m_yyyy(11, 2010)
dec2010 <- script_m_yyyy(12, 2010)
year2010 <- rbind(jan2010, feb2010, mar2010, apr2010, may2010, jun2010, jul2010, aug2010, sep2010, oct2010)

PanelAll <- rbind(year2015, year2014, year2013, year2012, year2011, year2010)

        
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
