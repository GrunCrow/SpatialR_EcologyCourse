
######## PRE-PROCESS RAW GPS DATA ######## 


install.packages('lubridate') # for data manipulation
install.packages('geosphere') # for geographical data manipulation
install.packages('sp') # classes and methods for spatial data
install.packages('sf') # binds to 'GDAL' for reading and writing data, to 'GEOS' for geometrical operations, and to 'PROJ' for projection conversions and datum transformation
install.packages('shapefiles') # read and write ESRI shapefiles
install.packages('dplyr') # for data manipulation
install.packages('lubridate') # for dates and time series;
install.packages('ggplot2') # for plotting
install.packages('plyr') # for data manipulation
install.packages('fuzzyjoin') # join two tables based on a function describing whether two vectors are matched or not
install.packages('track2KBA') # functions for preparing and analyzing animal tracking data


library(lubridate)
library(geosphere)
library(sp)
library(sf)
library(shapefiles)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plyr)
library(fuzzyjoin)
library(track2KBA)



# set my working directory
# setwd("/Users/jorgepereira/Documents/My Files/Classes/2024_Spatial_Data_Analysis_Ecology_UCA/Session 2/Data_processing/Class")

setwd("Practices/S2_Data_processing/")

# Import raw files (downloaded directly from the device)
f = readLines("Class/1_L91287.csv")
df <- read.csv2(
  text   = f, 
  header = FALSE,
  sep = ",",
  quote="\"", 
  dec=",", 
  skip=7, # here check for the number of rows in your file header
  col.names=c("Date","Time","Latitude","Longitude","Altitude","Satellites","HDOP","PDOP","Temperature","Speed","TTFF","SNR","tbd"),
  nrows = length(f),
  encoding = "latin1",
  stringsAsFactors = FALSE
)



# Fill the database with the following parameters 
df$Date = mdy(df$Date)
df$Bird_ID = "1_L91287"
df$lat_col = 39.41 # change the latitude of the colony or population you are study
df$long_col = -9.51 # change the longitude of the colony or population you are study
df$DateGMT = as.POSIXct(paste(df$Date, df$Time), format="%Y-%m-%d %H:%M:%S")
head(df)
tail(df)
df$Latitude = as.numeric(as.character(df$Latitude))
df$Longitude = as.numeric(as.character(df$Longitude))
df$lat_col = as.numeric(as.character(df$lat_col))
df$long_col = as.numeric(as.character(df$long_col))



# Compute some other metrics that can be useful in the data cleaning or further in the analysis
df$dist_col = distGeo(df[,c("Longitude","Latitude")], df[,c("long_col","lat_col")])/1000 # in km, but you can change the data format
df$COL = ifelse( df$dist_col < 1, "colony", "sea" ) # in km
df$heading = bearing(df[,c("Longitude","Latitude")], df[,c("long_col","lat_col")])



# Function to convert bearing angle from -180:180 to 0:360
convert_bearing <- function(angle) {
  ifelse(angle < 0, angle + 360, angle)
}



# Apply the conversion to the 'heading' column
df$heading_360 <- convert_bearing(df$heading)

head(df)



# Save the interpolated data in a different working directory
# setwd("/Users/jorgepereira/Documents/My Files/Classes/2024_Spatial_Data_Analysis_Ecology_UCA/Session 2/Data_processing/Class/Proc_data")



# Export the new data into a tab delimited *.txt
write.table(df, file="Proc_data/1_L91287.txt", quote=F, row.names=F, sep="\t") 



# Define and project the coordinate system of the dataframe you are working one and export it as a KML file 
df1 = df
coordinates(df1) = c("Longitude", "Latitude")
proj4string(df1) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
df1 = spTransform(df1, CRS("+proj=longlat +datum=WGS84"))
crs.WGS84 = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
df1_sf <- st_as_sf(df1, coords = c("Longitude", "Latitude"))  
st_write(df1_sf, "Proc_data/1_L91287.kml", driver = "KML")



# Export to a shapefile format to open it in a GIS software
st_write(df1_sf, "Proc_data/1_L91287.shp")



# GPS data interpolation by a defined time period
df$DateGMT<-as.POSIXct(df$DateGMT, "%Y-%m-%d %H:%M:%S") # ensure that DateGMT is in POSIXct format
par(mfrow=c(2,2), las=1)
plot(df$Longitude, df$Latitude, asp=1, type="b", main="Original_trip")
plot(df$DateGMT, df$Latitude, type="b", main="Original_Lat")



# Interpolate a track by a defined time interval 
newdates<-seq(df$DateGMT[1], df$DateGMT[nrow(df)], by=60*10) # set the number of minutes
df_int<-data.frame(Latitude=approx(df$DateGMT, df$Latitude, xout = newdates)$y)
df_int$Longitude<-approx(df$DateGMT, df$Longitude, xout = newdates)$y
df_int$DateGMT<-newdates
plot(df_int$Longitude, df_int$Latitude, asp=1, type="b", pch="+", cex=.8, col=2, main="Interpolated_trip")
plot(df_int$DateGMT, df_int$Latitude, col=2, pch="+", cex=.8, main="10 min interpolation (60 * 10 sec)")



# Fill interpolated data with the original data
df_int$Bird_ID = "1_L91287"
df_int$lat_col = 39.41 # change the latitude of the colony or population you are study
df_int$long_col = -9.51 # change the longitude of the colony or population you are study
df_int$Latitude = as.numeric(as.character(df_int$Latitude))
df_int$Longitude = as.numeric(as.character(df_int$Longitude))
df_int$lat_col = as.numeric(as.character(df_int$lat_col))
df_int$long_col = as.numeric(as.character(df_int$long_col))



# Compute the same metrics as before for the interpolated data
df_int$dist_col = distGeo(df_int[,c("Longitude","Latitude")], df_int[,c("long_col","lat_col")])/1000 # in km, but you can change the data format
df_int$COL = ifelse( df_int$dist_col < 1, "colony", "sea" ) # in km
df_int$heading = bearing(df_int[,c("Longitude","Latitude")], df_int[,c("long_col","lat_col")])



# Function to convert bearing angle from -180:180 to 0:360
convert_bearing <- function(angle) {
  ifelse(angle < 0, angle + 360, angle)
}



# Apply the conversion to the 'heading' column
df_int$heading_360 <- convert_bearing(df_int$heading)



# Ensure that DateGMT is in POSIXct format
df <- df %>%
  mutate(DateGMT = as.POSIXct(DateGMT, format="%Y-%m-%d %H:%M:%S"))
df_int <- df_int %>%
  mutate(DateGMT = as.POSIXct(DateGMT, format="%Y-%m-%d %H:%M:%S"))



# Perform a fuzzy join function to find the nearest temperature values based on DateGMT
fuzzy_join_df <- df_int %>%
  fuzzy_left_join(df, by = "DateGMT", match_fun = `<=`) %>%
  group_by(DateGMT.x) %>%
  slice_min(abs(difftime(DateGMT.x, DateGMT.y, units = "mins"))) %>%
  ungroup()


# Save the new merged file with the two datasets
# setwd("/Users/jorgepereira/Documents/My Files/Classes/2024_Spatial_Data_Analysis_Ecology_UCA/Session 2/Data_processing/Class/Proc_data/Interpolated_data")
write.table(fuzzy_join_df,  file="Proc_data/Interpolated_data/1_L91287.txt", quote=F, row.names=T, sep="\t")



# List all .txt files in Proc_data folder
txt_files <- list.files(path = "Proc_data/Interpolated_data/", pattern = "\\.txt$", full.names = TRUE)


# Read and merge all the txt files into one data frame
merged_data <- do.call(rbind, lapply(txt_files, read.table, header = TRUE, sep = "\t", stringsAsFactors = FALSE))



# Save the merged data into a new file
write.table(merged_data, "Proc_data/Interpolated_data/merged_data.txt", sep = "\t", row.names = FALSE, col.names = TRUE)



# Import interpolated merged data 
# setwd("/Users/jorgepereira/Documents/My Files/Classes/2024_Spatial_Data_Analysis_Ecology_UCA/Session 2/Data_processing/Class/Proc_data/Interpolated_data")
merged_data_clean = read.table("Proc_data/Interpolated_data/merged_data.txt", header = T, sep="\t")
head(merged_data_clean)



# Plot the trips for each bird;
ggplot(merged_data_clean, aes(Longitude, Latitude)) + geom_path(aes(colour = Bird_ID))   



# Summary stats for Bird_ID - useful to check for errors on our dataset
data_bird_char = ddply(merged_data_clean, .(Bird_ID), summarise,
                       min_Date = min(DateGMT, na.rm=T),
                       max_Date = max(DateGMT, na.rm=T),
                       min_Latitude = min(Latitude, na.rm=T),
                       max_Latitude = max(Latitude, na.rm=T),
                       min_Longitude = min(Longitude, na.rm=T),
                       max_Longitude = max(Longitude, na.rm=T),
                       track_dur = round(max(DateGMT)-min(DateGMT)),
                       total_distance= sum(dist_col, na.rm=T),
                       max_Dcol = max(dist_col, na.rm=T),
                       mean_Temperature= mean(Temperature, na.rm=T),
                       sd_Temperature= sd(Temperature, na.rm=T),
                       mean_Speed= mean(Speed, na.rm=T),
                       sd_Speed= sd(Speed, na.rm=T))



head(data_bird_char)

write.table(data_bird_char, file="birds_chars_example.txt", quote=F, row.names=F, sep="\t") # export the new data into a tab delimited *.txt


# Import a testing dataset from the trips
# setwd("/Users/jorgepereira/Documents/My Files/Classes/2024_Spatial_Data_Analysis_Ecology_UCA/Session 2/Data_processing/Class/")
trip_data_example = read.table("Class/Teste_trips.txt", header = T, sep="\t")
head(trip_data_example)
str(trip_data_example)

# Asegúrate de que las columnas Date y Time sean del tipo correcto
trip_data_example$DateGMT <- as.POSIXct(paste(trip_data_example$Date, trimws(trip_data_example$Time)), 
                                        format = "%d/%m/%Y %H:%M:%S", 
                                        tz = "GMT")


# Summary of trip_data_example
summary(trip_data_example)

# Summary stats for Trip_ID 
# Calcular estadísticas resumidas
data_trip_char <- ddply(trip_data_example, .(Bird_ID, Trip_ID), summarise,
                        min_Date = min(DateGMT, na.rm = TRUE),
                        max_Date = max(DateGMT, na.rm = TRUE),
                        min_Latitude = min(Latitude, na.rm = TRUE),
                        max_Latitude = max(Latitude, na.rm = TRUE),
                        min_Longitude = min(Longitude, na.rm = TRUE),
                        max_Longitude = max(Longitude, na.rm = TRUE),
                        track_dur = difftime(max(DateGMT, na.rm = TRUE), 
                                             min(DateGMT, na.rm = TRUE), 
                                             units = "hours"),
                        total_distance = sum(dist_col, na.rm = TRUE),
                        max_Dcol = max(dist_col, na.rm = TRUE),
                        mean_Temperature = mean(Temperature, na.rm = TRUE),
                        sd_Temperature = sd(Temperature, na.rm = TRUE),
                        mean_Speed = mean(Speed, na.rm = TRUE),
                        sd_Speed = sd(Speed, na.rm = TRUE))





head(data_trip_char)


write.table(data_trip_char, file="Class/trips_chars_example.txt", quote=F, row.names=F, sep="\t") # export the new data into a tab delimited *.txt




######## TRACK2KBA ######## 

# This code was adapted from Beal et al (2021). Methods in Ecology and Evolution
# https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13713



install.packages("track2KBA")
install.packages("devtools", dependencies = TRUE) 
devtools::install_github("BirdLifeInternational/track2kba", dependencies=TRUE) # development version - add argument 'build_vignettes = FALSE' to speed it up 



# Now we will use tracking data collected at a seabird breeding colony to illustrate a track2KBA workflow for 
# identifying important sites. It is important to note that the specific workflow you use 
# (i.e., which functions and in what order) will depend on the species of interest and the associated data at hand.

# First, in order for the data to work in track2KBA functions, we can use the formatFields function to format the 
# important data columns needed for analysis. These are: a DateTime field, Latitude and Longitude fields, and an ID 
# field (i.e. individual animal, track, or trip).
data(boobies)
?boobies  # for some background info on the example data set 



dataGroup <- formatFields(
  dataGroup = boobies, 
  fieldID   = "track_id", 
  fieldDate = "date_gmt", 
  fieldTime = "time_gmt",
  fieldLon  = "longitude", 
  fieldLat  = "latitude")

str(dataGroup)



# If your data come from a central-place foraging species (i.e. one which makes trips out from a centrally-located place,
# such as a nest in the case of a bird), you can use tripSplit to split up the data into discrete trips.

# In order to do this, you must identify the location(s) of the central place(s) (e.g. colony-center, or nest sites).

# here we know that the first points in the data set are from the colony center
colony <- dataGroup %>%
  summarise(
    Longitude = first(Longitude), 
    Latitude  = first(Latitude))



# Our colony dataframe tells us where trips originate from. Then we can set some parameters to decide what constitutes
# a trip. To do that we should use our understanding of the movement ecology of the study species. In this case we know 
# our seabird travels out to sea on the scale of tens of kilometers, so we set innerBuff (the minimum distance from 
# the colony) to 3 km, and duration (minimum trip duration) to 1 hour. returnBuff can be set further out in order to
# catch incomplete trips, where the animal began returning, but perhaps due to device failure the full trip wasn’t 
# captured. 

# Optionally, we can set rmNonTrip to TRUE which will remove the periods when the animals were not on trips. 
# The results of tripSplit can be plotted using mapTrips to see some examples of trips.
str(dataGroup)



trips <- tripSplit(
  dataGroup  = dataGroup,
  colony     = colony,
  innerBuff  = 3,      # kilometers
  returnBuff = 10,
  duration   = 1,      # hours
  rmNonTrip  = TRUE)

mapTrips(trips = trips, colony = colony)



# Then we can summarize the trip movements, using tripSummary. First, we can filter out data from trips that did not 
# return to the vicinity of the colony (i.e. within returnBuff), so they don’t skew the estimates.
trips <- subset(trips, trips$Returns == "Yes" )
sumTrips <- tripSummary(trips = trips, colony = colony)
sumTrips



# Now that we have an idea how the animals are moving, we can start with the process of estimating their space use areas,
# and identifying potentially important sites for the population!
  
# track2KBA uses Kernel Density Estimation (KDE) to produce space use estimates for each individual track. 
# In order for these to be accurate, we need to transform the tracking data to an equal-area projection. 
# We can use the convenience function projectTracks to perform this projection. 
# We can select between an azimuthal or cylindrical projection, and decide whether to center the projection on the data 
# itself. Custom-centering is generally a good idea for quick analyses as this will minimize distortion,
# however it is important to remember that the resulting projection will be data specific. 
# So if you remove even one track and re-analyze, the projection will differ between datasets. 
# For formal analysis, the best solution is to find a standard projection that is appropriate for your study region.
tracks <- projectTracks( dataGroup = trips, projType = 'azim', custom=TRUE )
class(tracks)



# findScale provides options for setting the all-important smoothing parameter in the KDE. 
# This parameter decisions is of the utmost importance, as it determines the scale at which the tracking locations 
# will be ‘smoothed’ into an estimate of the probability of use of space by the animal. 
# findScale calculates candidate smoothing parameter values using several different methods.

# If we know our animal uses an area-restricted search (ARS) strategy to locate prey, then we can set the scaleARS=TRUE. 
# This uses First Passage Time analysis to identify the spatial scale at which area-restricted search is occuring, 
# which may then be used as the smoothing parameter value.
hVals <- findScale(
  tracks   = tracks,
  scaleARS = TRUE,
  sumTrips = sumTrips)

hVals



# The other values provided by findScale are more simplistic methods of calculating the smoothing parameter. 
# href is the canonical reference method, and relates to the number of points in the data and their spatial variance. 
# mag is the log of the average foraging range (med_max_dist in the sumTrips output); this methods only works for 
# central-place foragers.

# Next, we must select a smoothing parameter value. To inform our decision, we ought to use our understanding of the 
# species’ movement ecology to guide our decision about what scale make sense. 
# That is, from the findScale output, we want to avoid using values which may under- or over-represent the area used 
# by the animals while foraging.

# Once we have chosen a smoothing value, we can produce KDEs for each individual, using estSpaceUse. 
# By default this function isolates the core range of each track (i.e. the 50% utilization distribution, or where the
# animal spends about half of its time) which is a commonly used standard (Lascelles et al. 2016). 
# However, another quantile can be chose using the levelUD argument, or the full utilization distritbution can
# be returned using polyOut=FALSE.

# The resulting KDEs can be plotted using mapKDE, which if polyOut=TRUE shows each tracks’s core range in a different color.

# Note: here we might want to remove the trip start and end points that fall within the innerBuff (i.e. 3 km) we 
# set in tripSplit, so that they don’t skew the at-sea distribution towards to colony.
tracks <- tracks[tracks$ColDist > 3, ] # remove trip start and end points near colony

KDE <- estSpaceUse(
  tracks = tracks, 
  scale = hVals$scaleARS, 
  levelUD = 50, 
  polyOut = TRUE)

mapKDE(KDE = KDE$UDPolygons, colony = colony)



# At this step we should verify that the smoothing parameter value we selected is producing reasonable space use 
# estimates, given what we know about our study animals. Are the core areas much larger than expected? Much smaller? 
# If so, consider using a different value for the `scale` parameter.

# The next step is to estimate how representative this sample of animals is of the population. 
# That is, how well does the variation in space use of this sample of tracks encapsulate variation in the wider 
# population? To do this we can use the repAssess function. 
# This function repeatedly samples a subset of track core ranges, averages them together, and quantifies how many 
# points from the unselected tracks fall within this combined core range area. 
# This process is run across the range of the sample size, and iterated a chosen number of times.

# To do this, we need to supply Utilization Distributions to repAssess (e.g., the output of estSpaceUse) and the 
# tracking data. We can choose the number of times we want to re-sample at each sample size by setting the iteration
# argument. The higher the number the more confident we can be in the results, but the longer it will take to compute.

# The output is a dataframe, with the estimated percentage of representativeness given in the out column.

# The relationship between sample size and the percent coverage of un-tested animals’ space use areas (i.e. Inclusion) 
# is visualized in the output plot seen below.

# By quantifying this relationship, we can estimate how close we are to an information asymptote. 
# Put another way, we have estimated how much new space use information would be added by tracking more animals. 
# In the case of this seabird dataset, we estimate that ~98% of the core areas used by this population are captured 
# by the sample of 39 individuals. Highly representative!
repr <- repAssess(
  tracks    = tracks, 
  KDE       = KDE$KDE.Surface,
  levelUD   = 50,
  iteration = 1, 
  bootTable = FALSE)



# Now, using findSite we can identify areas where animals are overlapping in space and delineate sites that meet 
# some criteria of importance. 
# Using the core area estimates of each individual track we can calculate where they overlap. 
# Then, we estimate the proportion of the larger population in a given area by adjusting our overlap estimate 
# based on the degree of representativeness.

# Here, if we have population size estimates, we can include this value (using the popSize argument) to estimate
# the number of individuals using a space, which can then use to compare against population-level importance
# criteria (e.g. KBA criteria).
# If we don’t have population size estimates to provide, findSite this will output a proportion of the population instead.

# If you desire polygon output of the overlap areas, instead of a gridded surface, you can indicate this using the
# polyOut argument.
Site <- findSite(
  KDE = KDE$KDE.Surface,
  represent = repr$out,
  levelUD = 50,
  popSize = 500,     # 500 individual seabirds breed one the island
  polyOut = TRUE)

class(Site)



# This plot shows the minimum estimated number of birds using the space around the breeding island.
Sitemap <- mapSite(Site, colony = colony)





# End of script

