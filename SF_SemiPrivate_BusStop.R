##################################################
##     SF Semi-Private Bus Stop Project        ##
################################################

# 1. Set Workspace
# 2. Load and Setup Data
        # Traffic Incident Data
        # Bus Stop Location
        # Weather Data
# 3. Set Geographic Boundaries and Panel Data Set
# 4. Model
# 5. Visualizations 

## 1 ## Set Workspace ############################
# Load Packages


## 2 ## Load and Setup Data ######################

## SFPD Traffic Incidents
# Source: Open Data SF 
TrafficIncidents <- read.csv("SFPD_Incidents_Accidents.csv", stringsAsFactors = FALSE) #load data
summary(TrafficIncidents) #quick summary
head(TrafficIncidents) #header view
# COnvert date and time to date & time format
# Subset data to accidents and hit & runs
# Subset by day of week (business week, without holidays)
# Subset by time of day (commute hours)

## Load SF Bus Stop locations 

## Load SF Semi-private bus stop locations
# source: https://www.sfmta.com/sites/default/files/projects/2015/Shuttles%20Network%20150818%20%28list%29.pdf
SemiPrivateStop <- read.csv("CommuterShuttlePilotStops.csv", stringsAsFactors = FALSE)
SemiPrivateStop <- SemiPrivateStop[,-4] #remove double zone column
SemiPrivateStop <-    #name Muni/White column "Zone"
summary(SemiPrivateStop)
head(SemiPrivateStop)
# 

## Rainfall by Day in SF
# NOAA Rainfall and Temperature 1/1/2003 to 11/1/2015 Downtown SF: http://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USW00023272/detail
weather <- read.csv("SF Weather 2003 to 2015.csv", stringsAsFactors = FALSE)
summary(weather)
 #convert to date format


# Set variable for stop geographic boundary (easier to adjust for sensitivity analsis)

