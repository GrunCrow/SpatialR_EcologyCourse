# WORKSHOP SPATIAL ANALYSIS AND HABITAT MODELLING #
# Univ. Cádiz - 29/10/24 - 31/10/24
# Session 1: Introduction and basics of spatial analysis #

# Practice (3 hours):
# - Installation and configuration of R and RStudio.
# - Introduction to the main packages in R for spatial analysis (sp, sf, raster, ggplot2, etc.).
# - Loading and visualising spatial data.
# - Importing GPS data.
# - Creating basic maps.  
# - Motion animation (moveVis package). 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~+
#### settings: packages and directory
# TO SET WORKING DIRECTORY GO TO "SESSION > SET WORKING DIRECTORY > CHOOSE DIRECTORY"



# Define the packages needed
packages <- c(
  # "moveVis",    # For movement visualization and animations
  "move",       # For handling movement data
  "sp",         # For spatial data structures and analysis
  "tmap",       # For thematic mapping
  "sf",         # For simple features and handling spatial objects
  "maps",       # For base maps
  "ggplot2",    # For plotting data
  "dplyr",      # For data manipulation
  "raster",     # For visualization of interactive maps
  "mapview",    # For raster data operations
  "httpgd"
)

# Install missing packages
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# install.packages("httpgd", dependencies = TRUE)
install.packages("languageserver", dependencies = TRUE)

# library(httpgd)
library(languageserver)

# Set option to open plots in internal VS Code viewer
# options(device = function(...) httpgd::httpgd())


# Load all installed packages
lapply(packages, library, character.only = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 1. Basic Spatial Data Handling with sf ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls()) # Elimina todos los objetos creados previamente en el entorno de trabajo de R.
gc() # Liberar memoria no utilizada.

# Load necessary packages
library(sf)

# Create a simple point dataset
points_data <- data.frame(
  id = 1:3,
  name = c("Point1", "Point2", "Point3"),
  lon = c(10, 20, 30),
  lat = c(50, 40, 60)
)

# Convert to an `sf` object
sf_points <- st_as_sf(points_data, coords = c("lon", "lat"), crs = 4326)  # WGS84

# Print and plot the points
print(sf_points)
plot(sf_points)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 2. Raster Data Handling with raster ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load necessary package
library(raster)

# Create a simple raster dataset
r <- raster(nrows = 10, ncols = 10, xmn = 0, xmx = 10, ymn = 0, ymx = 10)
values(r) <- runif(ncell(r))  # Assign random values to raster cells

# Plot the raster
plot(r, main = "Random Raster Data")

# Write raster to a GeoTIFF file
writeRaster(r, filename = "random_raster.tif", format = "GTiff", overwrite = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Visualizing Spatial Data with different R packages ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load necessary libraries
library(dplyr)
library(leaflet)
library(ggplot2)
library(sf)
library(tmap)
library(mapview)

# Step 1: Download and Load Tracking Data
tracking_data = read.delim("DesertasPetrel.txt",sep="\t",h=T)
head(tracking_data)

# Step 2: Prepare the Data
# Assuming the tracking data contains columns like id, longitude, latitude, and timestamp

# Convert to an sf object (for use with various mapping packages)
tracking_data_sf <- tracking_data %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326)  # EPSG:4326 for WGS84

# Step 3: Example 1 - Visualize with Leaflet
leaflet(tracking_data) %>%
  addTiles() %>%
  addCircleMarkers(~LON, ~LAT, color = ~factor(ID), popup = ~paste("ID:", ID, "<br>Time:", timestamp)) %>%
  addLegend("bottomright", pal = colorFactor(rainbow(17), tracking_data$ID), values = ~ID, title = "Animal ID") %>%
  setView(lng = mean(tracking_data$LON), lat = mean(tracking_data$LAT), zoom = 5)

# Step 4: Example 2 - Visualize with ggplot2 and sf
ggplot(tracking_data_sf) +
  geom_sf(aes(color = factor(ID)), size = 2) +
  labs(title = "Animal Tracking Data", color = "Animal ID") +
  theme_minimal()

# Step 5: Example 3 - Visualize with tmap
# tmap allows easy switching between static and interactive maps
tmap_mode("view")  # Switch to interactive mode
tm_shape(tracking_data_sf) +
  tm_dots(col = "ID", palette = "Dark2", size = 0.5, popup.vars = c("ID", "timestamp")) +
  tm_layout(title = "Tracking Data of Four Animals")

# Step 6: Example 4 - Visualize with Mapview
mapview(tracking_data_sf, zcol = "ID", legend = TRUE)

# Optional: Save Maps or Animations
# e.g., ggsave("tracking_ggplot.png")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### WORKING MOVEVIS - Motion Animation (moveVis package) ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Install moveVis and move2 packages from CRAN
install.packages("moveVis")
install.packages("move")
#install.packages("move2")

# If moveVis is not available on CRAN, install from GitHub using devtools
if (!require(devtools)) install.packages("devtools")
devtools::install_github("16EAGLE/moveVis",force=TRUE)

# Load required packages
library(moveVis)
require(move)
#library(move2)

# Load example movement data from the 'move' package
data("move_data", package = "moveVis")

# Let's inspect the data structure
head(move_data)

# Align the movement data to a uniform time scale
# (needed to create a smooth animation)
move_data_aligned <- moveVis::align_move(move_data, res = 4, unit = "mins")

# Check the summary of the aligned data
summary(move_data_aligned)

# Check all maps available
moveVis::get_maptypes()

# Check available map types for the OSM map service
get_maptypes("esri")

# Now we can animate the movement tracks
frames <- frames_spatial(move_data_aligned, 
                         map_service = "esri", map_type = "world_topo_map")

# Add customization options to the frames
frames <- add_labels(frames, x = "Longitude", y = "Latitude", title = "Animal Movement")
frames <- add_northarrow(frames)
frames <- add_scalebar(frames, dist = 100, height = 0.02)

# Inspect frame number 20
frames[[20]]

# Animate the frames and save the animation as a GIF
animate_frames(frames, out_file = "animal_movement.gif",overwrite = T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### QUESTION: Change the base maps get_maptypes() function and try different maps as background
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check all maps available
get_maptypes()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Leave trail of different tracks

# Now we can animate the movement tracks with trails
frames <- frames_spatial(move_data_aligned, 
                         path_colours = c("red", "blue", "green"), 
                         map_service = "esri", 
                         map_type = "world_topo_map", 
                         trace_show = TRUE,              # Show the trace of the path
                         trace_colour = "grey",          # Change trace color to grey
                         trace_size = 2,                 # Size of the trace line
                         trace_alpha = c(0.2, 0.8),      # Fading effect for the trace
                         fade_ribbon = TRUE)             # Enable fading effect for trails

# Add customization options to the frames
frames <- add_labels(frames, x = "Longitude", y = "Latitude", title = "Animal Movement with Trails")
frames <- add_northarrow(frames)
frames <- add_scalebar(frames, dist = 100, height = 0.02)

# Animate the frames and save the animation as a GIF
animate_frames(frames, out_file = "animal_movement_with_trails.gif",overwrite = T)

print("Animation created and saved as 'animal_movement_with_trails.gif'")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test moveVis with tracks of Audouin's Gulls from southern Portugal
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Step 1: Load your tracking data
# Replace "path/to/your_tracking_data.csv" with the actual path to your CSV file
move_data <- read.csv("Iaudouinii_May24F_5days.csv")
move_data$id = as.factor(move_data$id) # Convert continuous to factor

# Step 2: Inspect the data structure
str(move_data)  # Check structure of the data frame
head(move_data)  # Preview the first few 6 rows of move_data

# Step 3: Ensure timestamp is in POSIXct format
move_data$timestamp <- as.POSIXct(move_data$timestamp)
str(move_data)

# Step 4: Sort the data by timestamp to ensure ascending order
move_data <- move_data[order(move_data$timestamp), ]

# Step 5: Remove duplicate timestamps
move_data <- move_data[!duplicated(move_data$timestamp), ]

# Step 6: Create a move object
# Replace 'ID' with a common identifier for all tracks if necessary
move_object <- move(x = move_data$longitude,   # Column for longitude
                    y = move_data$latitude,    # Column for latitude
                    time = move_data$timestamp, # Column for time
                    animal = "id")              # Use a common identifier or omit
# Set the CRS to WGS84
proj4string(move_object) <- CRS("+proj=longlat +datum=WGS84")

# Step 7: Align the movement data to a uniform time scale
move_data_aligned <- align_move(move_object, res = 15, unit = "mins")

# Generate 3 distinct colors using a color space palette
library(colorspace)
colors_3 <- qualitative_hcl(3, palette = "Dynamic")  # Change palette for different styles
colors <- c("red", "green", "violet") # Add more colors if needed

# Check all background maps available
get_maptypes()

# Step 8: Animate the movement tracks with specified path colors
frames <- frames_spatial(move_data_aligned,
                         map_service = "esri", 
                         map_type = "world_topo_map", 
                         trace_show = TRUE, 
                         trace_colour = "grey", 
                         trace_size = 3, 
                         trace_alpha = c(0.2, 0.8), 
                         fade_ribbon = TRUE,
                         verbose=T)

# Step 9: Customize the frames
frames <- add_labels(frames, x = "Longitude", y = "Latitude", title = "Animal Movement with Specified Colors")
frames <- add_northarrow(frames)
frames <- add_scalebar(frames, dist = 30, height = 0.02)
# Add timestamps directly to the frames
frames <- add_timestamps(frames, size = 4, colour = "black", type="label")

frames[[20]] # preview one of the frames, e.g. the 20th frame

# Step 10: Animate the frames and save the animation as a GIF
animate_frames(frames, out_file = "animal_movement_with_colours.gif",overwrite = T)

print("Animation created and saved as 'animal_movement_with_colours.gif'")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Year-round foraging distribution of Deserta's petrel #### 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Step 1: Load your tracking data
pdeserta = read.table("DesertasPetrel.txt", h=T, sep="\t") # import
head(pdeserta)   # check the structure of the object
names(pdeserta)

# Load necessary library
library(dplyr)
set.seed(123)  # Set a seed for reproducibility

# Step 1: Randomly select 5 unique BIRD_IDs
selected_bird_ids <- pdeserta %>%
  distinct(BIRD_ID) %>%
  sample_n(5)

# Step 2: Filter the original data frame for the selected BIRD_IDs
pdeserta_5 <- pdeserta %>%
  filter(BIRD_ID %in% selected_bird_ids$BIRD_ID)

# View the sampled data
print(pdeserta_5)
unique(pdeserta_5$BIRD_ID)
head(pdeserta_5)

move_data = pdeserta_5

# Step 2: Inspect the data structure
print("Inspecting the structure of the tracking data:")
str(move_data)  # Check structure of the data frame
print("First few rows of the tracking data:")
head(move_data)  # Preview the first few rows of move_data

# Step 3: Ensure timestamp is in POSIXct format
move_data$timestamp <- as.POSIXct(move_data$timestamp)
str(move_data)

# Step 4: Sort the data by timestamp to ensure ascending order
move_data <- move_data[order(move_data$timestamp), ]

# Step 5: Remove duplicate timestamps
move_data <- move_data[!duplicated(move_data$timestamp), ]
names(move_data)

#Use Movevis to  make the animaion
#convert the data to a move object
require(moveVis)
move_object <-
  df2move(
    move_data,
    proj = "+proj=longlat +datum=WGS84",
    x = "LON",
    y = "LAT",
    time = "timestamp",
    track_id = "BIRD_ID"
  )

# Step 7: Align the movement data to a uniform time scale
move_data_aligned <- align_move(move_object, res = 10, unit = "hours")

# Step 7: Define colors based on the number of unique tracks
# Ensure there are enough colors for the number of tracks
# colors <- c("red", "blue", "green", "violet", "orange") # Add more colors if needed

# Generate 17 distinct colors using a color space palette
library(colorspace)
colors_5 <- qualitative_hcl(5, palette = "Dark 3")  # Change palette for different styles

# Check all background maps available
get_maptypes()

# Step 8: Animate the movement tracks with specified path colors
# Run the function and capture any error message
frames <- frames_spatial(move_data_aligned,
                         map_service = "esri", map_type = "world_topo_map")

# Step 9: Customize the frames
frames <- add_labels(frames, x = "Longitude", y = "Latitude", title = "Animal Movement with Specified Colors")
frames <- add_northarrow(frames)
frames <- add_progress(frames) # add a progress bar
frames <- add_scalebar(frames, dist = 500, height = 0.02)
# Add timestamps directly to the frames (no need to pass 'move_data_aligned')
frames <- add_timestamps(frames, size = 4, colour = "black", type="label")

frames[[20]] # preview one of the frames, e.g. the 20th frame

# Step 10: Animate the frames and save the animation as a GIF
animate_frames(frames, fps = 20, out_file = "animate_pdeserta.gif",overwrite = T)

print("Animation created and saved as 'animate_pdeserta.gif'")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### HANDS-ON EXERCISES WITH SEABIRD TRACKING DATA ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Q1: Plot the foraging distribution of Bugio's (Deserta's) Petrel 
# (Pterodroma deserta), construct two plots for the breeding and non-breeding of the species ####

#### Q2: Plot the at-sea distribution of male and female Cory's shearwaters
# (Calonectris borealis) during their breeding and non-breeding periods ####

#### Q3: Plot the at-sea distribution of experienced and inexperienced Cory's shearwaters
# (Calonectris borealis) during their breeding and non-breeding periods ####

#### Q4: Map the at-sea breeding and non-breeding distribution of Sooty Terns
# (Onychoprion fuscatus) from Ascension Island. Which environmental predictors seem to drive the 
# foraging distribution of this population? ####

#### Q5: Explore the at sea movements of Macaronesian sharwaters (Puffinus baroli) 
# from Selvagem Grande during their breeding and non-breeding periods ####

#### Q6: Inspect the at sea movements of Macaronesian sharwaters (Puffinus baroli) 
# from Porto Santo during their breeding and non-breeding periods ####


# Which environmental predictors seem to influence the foraging 
# at-sea distribution of the former species/ populations in the present
# and in future times?

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### DESERTAS PETRELS ####

#### data import and plotting
pdeserta = read.table("DesertasPetrel.txt", h=T, sep="\t")     # import
str(pdeserta)                                               # check the structure of the object
plot(LAT~LON, data=pdeserta)

plot(LAT~LON, data=pdeserta, type="o", pch=20, lty=3, col=as.numeric(pdeserta$ID),
     xlim=c(-80,35), ylim=c(-55,60))     # draw a plot of locations as lots and a dashed line

plot(LAT~LON, data=pdeserta, type="o", pch=20, lty=3, col=as.numeric(pdeserta$ID),
     xlim=c(-80,35), ylim=c(-55,60))     # draw a plot of locations as lots and a dashed line
require(maps)
map('world', add=T, fill=T, col="grey")              # add a map of the world on the region

par(mfrow=c(1,2)) # more plots in the same frame (1 row, 2 columns)
pdeserta_breed = subset(pdeserta, STAGE == "BREEDING", select= FID:ID) # To select at-sea locations during the BREEDING period
plot(LAT~LON, data=pdeserta_breed, type="p", pch=20, lty=3, col=as.numeric(pdeserta$ID),
     xlim=c(-80,35), ylim=c(-55,60), main="Breeding period")     		# draw a plot of locations as a line
map('world', add=T, fill=T, col="grey")              # add a map of the world on the region

pdeserta_winter = subset(pdeserta, STAGE == "NOT_BREEDING", select= FID:ID) # To select at-sea locations during the NON BREEDING period (winter time)
plot(LAT~LON, data=pdeserta_winter, type="p", pch=20, lty=3, col=as.numeric(pdeserta$ID),
     xlim=c(-80,35), ylim=c(-55,60), main="Wintering period")     		# draw a plot of locations as a line
map('world', add=T, fill=T, col="grey")    # add a map of the world on the region

require(adehabitatHR)
coordinates(pdeserta_breed) = ~LON + LAT  # promote to "sp" object (SpatialPointsDataFrame class)
locs = pdeserta_breed
head(locs)

coordinates(pdeserta_winter) = ~LON + LAT  # promote to "sp" object (SpatialPointsDataFrame class)
locs2 = pdeserta_winter
head(locs2)


# Download WorldClim bio climatic environmental variables
install.packages("geodata")
require(geodata)
sst <- geodata::bio_oracle(path=tempdir(), "Temperature", "Mean", 
                           benthic=TRUE, depth="Mean", time="Present")
pp <- geodata::bio_oracle(path=tempdir(), "Primary.productivity", "Mean", 
                          benthic=TRUE, depth="Mean", time="Present")
chl <- geodata::bio_oracle(path=tempdir(), "Chlorophyll", "Mean", 
                           benthic=TRUE, depth="Mean", time="Present")
sal <- geodata::bio_oracle(path=tempdir(), "Salinity", "Mean", 
                           benthic=TRUE, depth="Mean", time="Present")
sst_rcp26_2100 <- geodata::bio_oracle(path=tempdir(), "Temperature", "Mean", 
                                      benthic=TRUE, rcp=26, depth="Mean", time="2100") # Representative Concentration Pathway (RCP)
sst_rcp45_2100 <- geodata::bio_oracle(path=tempdir(), "Temperature", "Mean", 
                                      benthic=TRUE, rcp=45, depth="Mean", time="2100")
sst_rcp85_2100 <- geodata::bio_oracle(path=tempdir(), "Temperature", "Mean", 
                                      benthic=TRUE, rcp=85, depth="Mean", time="2100")

plot(chl)
log_chl = log(chl)
log_pp = log(pp)
log_sst = log(sst)
log_sst_rcp85_2100 = log(sst_rcp85_2100)
plot(log_sst_rcp85_2100)

color = colorRampPalette(c("#3E49BB",
                           "#3498DB",
                           "yellow",
                           "orange",
                           "red",
                           "darkred"))

color2 = colorRampPalette(c("red",
                            "orange",
                            "yellow",
                            "grey",
                            "green",
                            "blue"))

require(mapview)
mapview(sst_rcp85_2100,
        col.regions = color(200),         # Color for raster
        na.color = "transparent",
        maxpixels = 9331200) + 
  mapview(locs2, 
          zcol = "BIRD_ID",      # Column with the categories for coloring
          legend = TRUE)                 # Add a legend to show category colors

# OTHER OPTION
library(mapview)
library(RColorBrewer)

# Assume 'category_column' is the column in 'locs2' containing your categorical variable
unique_categories <- unique(locs2$BIRD_ID)

# Create a color ramp palette based on the number of unique categories
color_ramp <- colorRampPalette(brewer.pal(9, "Set1"))(length(unique_categories))

# Map each category to a color
category_colors <- setNames(color_ramp, unique_categories)

# Use mapview to visualize 'locs2' with the color ramp
mapview(log(sst_rcp85_2100),
        col.regions = category_colors,
        na.color = "transparent",
        maxpixels = 9331200) + 
  mapview(locs2, 
          zcol = "BIRD_ID",            # Column with the categories
          col.regions = category_colors,       # Apply the color ramp palette
          legend = TRUE)



# End of script
# Enjoy R!