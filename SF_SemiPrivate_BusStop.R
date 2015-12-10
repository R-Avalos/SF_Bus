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
        # FE Model 200'
        # FE Model 300'
        # FE MOdel 100'
        # FE vs OLS
# 5. Visualizations 

#####################################################
## 1 ## Set Workspace   ############################
###################################################

rm(list = ls()) #clear workspace

# Load Packages
library(sp) #spatial pacakge
library(rgdal)
library(plyr)
library(reshape2)
library(ggmap)
library(lubridate)
library(rgeos)
library(tidyr)
library(dplyr)
library(zoo)
library(plm)
library(stargazer)

# Functions Created to Automate a few tasks
Sub_MonthYear <- function(x, y, z) {
        d <- subset(x, Month == y & Year == z )
        return(d)
}


ViolationCount <- function(d) {
        z1SP <- d 
        coordinates(z1SP) <- c("lon", "lat") 
        proj4string(z1SP) <- CRS("+init=epsg:4326")        
        z1SP <- spTransform(z1SP, CRS( "+init=epsg:3347" ) )        
        z2 <- gIntersects(z1SP, BusBuffer, byid = TRUE)
        z2.indexes <- which(z2, arr.ind = TRUE)
        z2.names <- BusStops$stop_id[z2.indexes[,1]]
        z2.count <- aggregate(z2.indexes ~ z2.names, FUN = length)
        z2.count <- rename(z2.count, Violation_Count = row)
        z3 <- z2.count
        z3 <- rename(z3, stop_id = z2.names)
        z3 <- z3[,-3]
        z3$Year <- d$Year[1]
        z3$Month <- d$Month[1]
        return(z3)
}

MonthlyMerge <- function(x) {
        z1 <- merge(x, BusStopsMerged, by="stop_id", all = TRUE)
        z1[is.na(z1)] <- 0
        z1$Year <- x$Year[1]
        z1$Month <- x$Month[1]
        return(z1)
}

script_m_yyyy <- function(x, y) {
        z1 <- Sub_MonthYear(RushHour, x, y)
        z1 <- ViolationCount(z1)
        z1 <- MonthlyMerge(z1)
        return(z1)
} # combination of 3 functions

#####################################################
## 2 ## Load and Setup Data   ######################
###################################################

## SFPD Traffic Incidents ##### Source: Open Data SF 
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
#Subset Traffic Violations removing Saturday and sunday
WeekdayViolation <- subset(TrafficViolation, DayOfWeek != "Saturday" & DayOfWeek != "Sunday")

#Further Subset by only keeping times between 6am-10am and 3p-8p
RushHourAM <- subset(WeekdayViolation, hour(DateTime) >= 6 & hour(DateTime) <= 9)
RushHourPM <- subset(WeekdayViolation, hour(DateTime) >= 15 & hour(DateTime) <= 19)
RushHour <- rbind(RushHourAM, RushHourPM) # Rush hour dataframes stacked
remove(RushHourAM, RushHourPM)


### create spatial data frame for Accidents
#TrafficSP <- TrafficViolation #first run
#coordinates(TrafficSP) <- c("lon", "lat") #transform to Spatial Data Frame
#proj4string(TrafficSP) <- CRS("+init=epsg:4326") ## Find corrrect SOURCE #TrafficSP <- spTransform(TrafficSP, CRS( "+init=epsg:3347" ) )  #transform

### create  spatial for rush hour
TrafficRushSP <- RushHour #first run
coordinates(TrafficRushSP) <- c("lon", "lat") #transform to Spatial Data Frame
proj4string(TrafficRushSP) <- CRS("+init=epsg:4326") ## Find corrrect SOURCE
TrafficRushSP <- spTransform(TrafficRushSP, CRS( "+init=epsg:3347" ) )  #transform


## Load SF Bus Stop locations ####### http://www.gtfs-data-exchange.com/agency/san-francisco-municipal-transportation-agency/

BusStops <- read.csv("stops.csv", header = TRUE, stringsAsFactors = FALSE) #read data
BusStops <- subset(BusStops, stop_name!="REFERENCE & REFERENCE")#remove reference
BusStops <- rename(BusStops, lon = stop_lon, lat = stop_lat) #rename lat and lon

### Load SF Semi-private bus stop locations
# source: https://www.sfmta.com/sites/default/files/projects/2015/Shuttles%20Network%20150818%20%28list%29.pdf
SemiPrivateFull <- read.csv("CommuterShuttlePilotStopsByHandCodeStopID.csv", header = TRUE, stringsAsFactors = FALSE) # load date
SemiPrivateFull <- SemiPrivateFull[,-5] #remove double zone column
SemiPrivateFull <- rename(SemiPrivateFull, Zone = Muni) #rename zone
SemiPrivateFull$Private <- as.numeric(1) #add indicator variable
SemiPrivateWhite <- SemiPrivateFull[which(SemiPrivateFull$Zone == "White"),] #create white zone list
SemiPrivateMuni <- SemiPrivateFull[which(SemiPrivateFull$Zone != "White"),] #create munit list
## Get Geocodes for white zone list ###
SemiPrivateWhite$City <- "San Francisco"
SemiPrivateWhite$Address <- paste(SemiPrivateWhite$Designated.Stop.Location, SemiPrivateWhite$City, sep = ", ") #merge street and city into address column
SemiPrivateWhite$Geocode <- geocode(SemiPrivateWhite$Address)#get lat and lon data by address
SemiPrivateWhite$lat <- SemiPrivateWhite$Geocode$lat
SemiPrivateWhite$lon <- SemiPrivateWhite$Geocode$lon
SemiPrivateWhite$stop_id <- as.numeric(rownames(SemiPrivateWhite))
SemiPrivateWhite <- subset(SemiPrivateWhite, select = c(stop_id, Designated.Stop.Location, Hours, Zone, Status, Private, lat, lon))
SemiPrivate <- merge(SemiPrivateMuni, SemiPrivateWhite, by = c("stop_id", "Designated.Stop.Location", "Hours", "Zone", "Private", "Status"),  all = TRUE) #merge back together
SemiPrivate <- subset(SemiPrivate, select = -c(Designated.Stop.Location, Status))

## Merge private bus stop indicator variables
BusStops$Private <- as.numeric(0)
BusStopsMerged <- merge(BusStops, SemiPrivate, by = c("stop_id", "Private", "lat", "lon"), all= TRUE) #merge
BusStopsMerged$duplicate <- duplicated(BusStopsMerged$stop_id)
BusStopsMerged$duplicate <- as.numeric(BusStopsMerged$duplicate)
BusStopsMerged$Private2 <- BusStopsMerged$Private + BusStopsMerged$duplicate
BusStopsMerged <- subset(BusStopsMerged, Private2 != 2)
BusStopsMerged <- subset(BusStopsMerged, select = -c(stop_desc, zone_id, stop_url, duplicate, Private2))

BusSP <- BusStopsMerged #seperate date frame
coordinates(BusSP) <- c("lon", "lat") #transform to spatial
proj4string(BusSP) <- CRS("+init=epsg:4326")
BusSP <- spTransform(BusSP, CRS( "+init=epsg:3347" ) )  #transform
#summary(BusSP)
plot(BusSP) #plot of bus stops


### Weather by Day in SF
# NOAA Rainfall and Temperature 1/1/2003 to 11/1/2015 Downtown SF: http://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USW00023272/detail

weather <- read.csv("SF Weather 2003 to 2015.csv", stringsAsFactors = FALSE) #read weather
summary(weather)
weather$Date <- ymd(weather$DATE) #convert to date format
weather$year <- year(weather$Date) #break out year
weather$month <- month(weather$Date) #break out month
head(weather)


#####################################################################
## 3 # Set Geographic Boundaries and Create Panel Dataset   ########
###################################################################
 
#set bufffer/radius for each bus stop all incidents #################
StopRadius <- 152.40
#unit of measure is meters, 
                    # 30.48m = 100 ft
                    # 91.44m = 300 ft
                    # 152.40m = 500 ft
BusBuffer <- gBuffer(BusSP, width = StopRadius, byid = TRUE)
head(BusBuffer)
plot(BusBuffer)

# Intersections between traffic sp and bus buffer Test 
#test <- TrafficSP[BusBuffer,] #select incidents in bus areas
#plot(TrafficSP) #view all incidents
#points(test, col = "light blue") #overlay incidents wihtint bus boundaries
#test.count <- aggregate(TrafficSP, BusBuffer, length) # total incidents wihin bus boundares
#count(test.count@data) #count of total incidents


#### Create intersection matrix   ##########################
###########################################################

#### Select (Traffic Data, Bus Radius)
intersections <- gIntersects(TrafficRushSP, BusBuffer, byid = TRUE) # where incidents overlayy bus stop areas


#dim(intersections) #confirm rows = bus stop count
# head(apply(intersections, MARGIN = 1, FUN = which)) #test
#Bstop.indexes <- which(intersections, arr.ind = TRUE) #setup array
#summary(Bstop.indexes) #note rows line up with BUs Stops
#Bstop.names <- BusStops$stop_name[Bstop.indexes[,1]] #match names from origional bust stop data frame
#Bstop.count <- aggregate(Bstop.indexes ~ Bstop.names, FUN = length) #count number of occurrences of True (traffic violation) for each stop
#Bstop.count <- rename(Bstop.count, Violation_Count = row)
#head(Bstop.count)

## Aggregate count for each year and month
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
year2014 <- rbind(jan2014, feb2014, mar2014, apr2014, may2014, jun2014, jul2014, aug2014, sep2014, oct2014, nov2014, dec2014)

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
year2013 <- rbind(jan2013, feb2013, mar2013, apr2013, may2013, jun2013, jul2013, aug2013, sep2013, oct2013, nov2013, dec2013)

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
year2012 <- rbind(jan2012, feb2012, mar2012, apr2012, may2012, jun2012, jul2012, aug2012, sep2012, oct2012, nov2012, dec2012)

#jan2011 <- script_m_yyyy(1, 2011)
#feb2011 <- script_m_yyyy(2, 2011)
#mar2011 <- script_m_yyyy(3, 2011)
#apr2011 <- script_m_yyyy(4, 2011)
#may2011 <- script_m_yyyy(5, 2011)
#jun2011 <- script_m_yyyy(6, 2011)
#jul2011 <- script_m_yyyy(7, 2011)
#aug2011 <- script_m_yyyy(8, 2011)
#sep2011 <- script_m_yyyy(9, 2011)
#oct2011 <- script_m_yyyy(10, 2011)
#nov2011 <- script_m_yyyy(11, 2011)
#dec2011 <- script_m_yyyy(12, 2011)
#year2011 <- rbind(jan2011, feb2011, mar2011, apr2011, may2011, jun2011, jul2011, aug2011, sep2011, oct2011, nov2011, dec2011)

#jan2010 <- script_m_yyyy(1, 2010)
#feb2010 <- script_m_yyyy(2, 2010)
#mar2010 <- script_m_yyyy(3, 2010)
#apr2010 <- script_m_yyyy(4, 2010)
#may2010 <- script_m_yyyy(5, 2010)
#jun2010 <- script_m_yyyy(6, 2010)
#jul2010 <- script_m_yyyy(7, 2010)
#aug2010 <- script_m_yyyy(8, 2010)
#sep2010 <- script_m_yyyy(9, 2010)
#oct2010 <- script_m_yyyy(10, 2010)
#nov2010 <- script_m_yyyy(11, 2010)
#dec2010 <- script_m_yyyy(12, 2010)
#year2010 <- rbind(jan2010, feb2010, mar2010, apr2010, may2010, jun2010, jul2010, aug2010, sep2010, oct2010, nov2010, dec2010)

# PanelAllRush <- rbind(year2015, year2014, year2013, year2012, year2011, year2010) # stack each year, too much data
remove(PanelAllRush) #clear some space
remove(RushHourAM, RushHourPM, SemiPrivate, SemiPrivateFull, SemiPrivateMuni, WeekdayViolation, SemiPrivateWhite)# clear some space
gc() #clear more RAM

PanelAllRush <- rbind(year2015, year2014, year2013, year2012) # stack each year


remove(jan2012, jan2013, jan2014, jan2015, feb2012, feb2013, feb2014, feb2015, mar2012, mar2013, mar2014, mar2015, apr2012, apr2013, apr2014, apr2015, may2012, may2013, may2014, may2015, jun2012, jun2013, jun2014, jun2015, jul2012, jul2013, jul2014, jul2015, aug2012, aug2013, aug2014, aug2015, sep2012, sep2013, sep2014, sep2015, oct2012, oct2013, oct2014, oct2015, nov2012, nov2013, nov2014, dec2012, dec2013, dec2014) #cleanup memory
#remove(jan2010, jan2011, jan2012, jan2013, jan2014, jan2015, feb2010, feb2011, feb2012, feb2013, feb2014, feb2015, mar2010, mar2011, mar2012, mar2013, mar2014, mar2015, apr2010, apr2011, apr2012, apr2013, apr2014, apr2015, may2010, may2011, may2012, may2013, may2014, may2015, jun2010, jun2011, jun2012, jun2013, jun2014, jun2015, jul2010, jul2011, jul2012, jul2013, jul2014, jul2015, aug2010, aug2011, aug2012, aug2013, aug2014, aug2015, sep2010, sep2011, sep2012, sep2013, sep2014, sep2015, oct2010, oct2011, oct2012, oct2013, oct2014, oct2015, nov2010, nov2011, nov2012, nov2013, nov2014, dec2010, dec2011, dec2012, dec2013, dec2014) #cleanup memory

### Combine weather with panel data
PanelAllRush$yearmon <- as.yearmon(paste(PanelAllRush$Year, PanelAllRush$Month, sep = "-"))
weather$yearmon <- as.yearmon(weather$Date)
weather <- subset(weather, yearmon > "Dec 2010")
#head(weather)
#weater <- subset(weather, Date > 12-31-2009)
PanelAllRush <- merge(PanelAllRush, weather, by = "yearmon", all = FALSE) # add month rainfall totals to panel data
PanelAllRush <- subset(PanelAllRush, Date > 2011)
PanelAllRush$Date <- paste(1, PanelAllRush$Month, PanelAllRush$Year)
PanelAllRush$Date <- dmy(PanelAllRush$Date)
#PanelAllRush$PrivateStop <- factor(with(PanelAllRush, ifelse(Date<ymd("2014-8-1") , 0, 1))) # fix private stop to correct month

### data will be specifc panel data, bus radius meters. df100, df200, df300 #########
gc()
PanelAllRush <- subset(PanelAllRush, select = c("Date", "Year", "Month", "stop_id", "Violation_Count", "Private", "PRCP", "TMAX", "TMIN")) #select variaables of interest
df_melt <- melt(PanelAllRush, id = c("Date", "Year", "Month", "stop_id"))
gc()


#### 100ft radius ####
df100 <- dcast(df_melt, Year + Month + stop_id ~ variable, sum) # specify bus raidus
gc()
df100$Date <- as.yearmon(paste(df100$Year, df100$Month, sep = "-"), "%Y-%m")
df100$Private[df100$Date < "Aug 2014"] <- 0 #fix private to specific month
df100$Private[df100$Private>=1] <- 1 #correct private
df100$stop_id <- as.character(df100$stop_id)

#### 300ft radius ####
df300 <- dcast(df_melt, Year + Month + stop_id ~ variable, sum) # specify bus raidus
gc()
df300$Date <- as.yearmon(paste(df300$Year, df300$Month, sep = "-"), "%Y-%m")
df300$Private[df300$Date < "Aug 2014"] <- 0 #fix private to specific month
df300$Private[df300$Private>=1] <- 1 #correct private
df300$stop_id <- as.character(df300$stop_id)
summary(df300)


#### 500ft radius ######
df500 <- dcast(df_melt, Year + Month + stop_id ~ variable, sum) # specify bus raidus
df500$Date <- as.yearmon(paste(df500$Year, df500$Month, sep = "-"))
df500$Private[df500$Date < "Aug 2014"] <- 0 #fix private to specific month
df500$Private[df500$Private>=1] <- 1 #correct private
df500$stop_id <- as.character(df500$stop_id)
head(df500)



########################################################
###  Models   #########################################
######################################################

modelOLS100 <- lm(Violation_Count ~ Private + PRCP + TMAX + TMIN + factor(Month), data= df100) 
modelOLS100Summary <- summary(modelOLS100)
modelOLS100Summary



model100 <- plm(Violation_Count ~ Private + PRCP + TMAX + TMIN + factor(Month), index=c("stop_id", "Date"), model="within", data= df100) 
model100summary <- summary(model100)
model100summary

model100.res <- resid(model100)
plot(df100$Private, model100.res)
plot(df100$PRCP, model100.res)
abline(0, 0)

stargazer(df100, type="html", out="df100summary.htm")
stargazer(model100, type="html", out="model100.htm") #regression output



model300 <- plm(Violation_Count ~ Private + PRCP + TMAX + TMIN + factor(Month), index=c("stop_id", "Date"), model="within", data= df300) 
model300summary <- summary(model300)
model300summary

model300.res <- resid(model300)
plot(df300$Private, model300.res)
plot(df300$PRCP, model300.res)
abline(0, 0)

modelOLS300 <- lm(Violation_Count ~ Private + PRCP + TMAX + TMIN + factor(Month), data= df300) 
modelOLS300Summary <- summary(modelOLS300)
modelOLS300Summary


stargazer(df300, type="html", out="df300summary.htm")
stargazer(model100, model300, type="html", out="model100and300.htm") #fixed effects output



model500 <- plm(Violation_Count ~ Private + PRCP + TMAX + TMIN + factor(Month), index=c("stop_id", "Date"), model="within", data= df500) 
model500summary <- summary(model500)
model500summary

model500.res <- resid(model500)
plot(df500$Private, model500.res)
plot(df500$PRCP, model500.res)
abline(0, 0)

modelOLS500 <- lm(Violation_Count ~ Private + PRCP + TMAX + TMIN + factor(Month), data= df500) 
modelOLS500Summary <- summary(modelOLS500)
modelOLS500Summary









### base model with white zones, 200'
FEmodel <- plm(Violation_Count ~ Private, index=c("stop_id", "Year"), model="within", data= PanelYear2) # fixed effects model
        
#summary(FEmodel)
#FEmodelsummary <- summary(PanelYear2)
#FEmodelsummary
#summary(PanelYear2)        
#PanelYear2summary <- summary(PanelYear2)
### base model with weather, 200'
#FEmodel2 <- plm(Violation_Count ~ PRCP + Private, index=c("stop_id", "Year"), model="within", data= PanelYear2) # fixed effects model
#summary(FEmodel2)
### base model subset to driving hours with weather, 200'
#FEmodel3 <- plm(Violation_Count ~ PRCP + Private, index=c("stop_id", "Year"), model="within", data= PanelYearRush) # fixed effects model
#summary(FEmodel3)
#FEmodel3summary <- summary(PanelYearRush)
#FEmodel3summary

# Breusch-Pagan Test: Are the variances across entities zero for RANDOM EFFECTS?
pool <- plm(Violation_Count ~ PRCP + Private, index=c("stop_id", "Year"), model="pooling", data= PanelYearRush) # fi
summary(pool)

bptest <- plmtest(pool, type=c("bp")) # we significantly reject the null hypotheis and conclude that a random effects approach is appropriate over linear regression
bptest
### driving hours, weather, and 100' radius
#FEmodel4 <- plm(Violation_Count ~ PRCP + Private, index=c("stop_id", "Year"), model="within", data= PanelYearRush) # fixed effects model
#summary(FEmodel4)
#FEmodel4random <- plm(Violation_Count ~ PRCP + Private, index=c("stop_id", "Year"), model="random", data= PanelYearRush) # fixed effects model
#summary(FEmodel4random)
#FEmodel4summary <- summary(PanelYearRush)
#FEmodel4summary
## driving hours, weather, and 300' radius
#FEmodel5 <- plm(Violation_Count ~ PRCP + Private, index=c("stop_id", "Year"), model="within", data= PanelYearRush) # fixed effects model
#summary(FEmodel5)
#FEmodel5random <- plm(Violation_Count ~ PRCP + Private, index=c("stop_id", "Year"), model="random", data= PanelYearRush) # fixed effects model
#summary(FEmodel5)
#FEmodel5summary <- summary(PanelYearRush)
#FEmodel5summary
#stargazer(FEmodel, FEmodel2, FEmodel3, type="html", column.labels = c("All Violations", "All with Weather", "Rush Hour with Weather"), out="models2.htm")
#stargazer(FEmodel3, pool, type="html", column.labels = c("Fixed Effects", "OLS"), out="FEorPool2.htm")
#stargazer(FEmodel3, FEmodel4, FEmodel5, type="html", column.labels = c("Base 200' Model", "100'", "300'"), out="sensitivity2.htm")
#stargazer(PanelYear2, type="html", out="panelsummary.htm" )

### Visualizations #################################

# Traffic incidents by year
ggplot(data= PanelAll, aes(x=Year, y=Violation_Count)) +
        stat_summary(fun.y = sum, geom = "line")

# All traffic incidents
palette(rainbow(10))
map <- get_map(location = "San Francisco", zoom = 13) #create map
ggmap(map) #review map

mapPoints <- ggmap(map) +
        geom_point(data = BusStops, aes(x = lon, y = lat)) +
        geom_point(data = TrafficIncidents, aes(x = Location$lon, y = Location$lat, color = year(DateTime), size = 2)) +
        scale_color_gradient(limits=c(2005, 2015), low="white", high="blue")
mapPoints #call map

# Rush Hour incidents

# PanelAll map
map3 <- ggmap(map) +
        geom_point(data = BusStops, aes(x = lon, y = lat)) + 
        geom_point(PanelAll, aes(x = lon, y = lat, group = Year))
map3



### Plot bus buffer
plot(BusBuffer)
points(TrafficSP, col="blue", cex=0.1)
points(TrafficSP2, col="red", cex=0.3)

##################################################


