
# This code was adapted from Garriga et al. 2016 (PLoS ONE); 
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0151984




# The Expectation-Maximization binary Clustering (EMbC) is a non-supervised multivariate clustering algorithm that 
# label eachGPS-location based on the estimated velocity and turning angle. 

# Different interpretations were attributed to each behavioural state following Louzao et al. 2014 (Movement Ecology)
# https://movementecologyjournal.biomedcentral.com/articles/10.1186/2051-3933-2-8


install.packages('EMbC')

# Let's load the EMbC R package; 
library (EMbC)


# Clean the environment directory;
rm(list=ls()) 
ls()


# Set the working directory where the code is stored;
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


# Load your tracking dataset;

# 'cs_dta' for Cory's shearwater (Calonectris borealis) at Corvo Island, during the chick-rearing period;
cs <- read.csv('cs_dta.csv', header=TRUE, sep=",")
names(cs) # to get the headers of an object
head(cs) # to display the first rows in the input data frame
str(cs) # to display the internal structure of an object 


# To perform the standard velocity/turn clustering of the trajectory, we just call the constructor by 
# passing a data.frame with the trajectory;
# Note that the data.frame includes a first column with a row id that we are discarding to meet the required format;
csbc <- stbc(cs[,2:4],info=-1) 



##### QUESTION 1 #####

# What this error message means? error: max(): object has no elements; 

# HINT: Date column must be always in the format YYYY-DD-MM HH:MM:SS


# The function stts() offers a fast compact view of the parameter set;
# For each cluster we have two columns per feature (i.e. velocity - in m/s; and turning angles - in radians)
# with the mean and standard deviation of the corresponding Gaussian component; 
# The last two columns show the marginal distribution of the cluster in absolute and percentage values;
stts(csbc)



##### QUESTION 2 #####

# What do 'LL', 'LH', 'HL' and 'HH' annotations stand for? 
# What can these clusters/ classifications tell us about animals' behaviour?

# HINT: read the methods section 'Coupling instantaneous energy-budget models and behavioural mode analysis' in 
# Louzao et al. 2014 (Movement Ecology) https://movementecologyjournal.biomedcentral.com/articles/10.1186/2051-3933-2-8



# The csbc@R slot is a matrix with the values that delimit each binary region. 
# For each cluster we have the min and max values of each variable (i.e. velocity and turn);
csbc@R


# We can see the clustering as a coloured scatter-plot of the input data with grey lines depicting the values 
# of the delimiters;
# NC in the legend stands for not classified data points (e.g. the first or the last point of the trajectory);
sctr(csbc)


# We can make use of the labelling values stored in csbc@A and the values of the delimiters stored in csbc@A;
# To visualize the binary splitting of velocities separately for low and high values of turns;
hist(csbc@X[which(csbc@A%in%c(1,3)),1],breaks=seq(0,max(csbc@X[,1]), max(csbc@X[,1])/50),include.lowest=TRUE,xlim=range(csbc@X[,1]), 
     xlab='velocity (m/s)',main='velocity distribution for LOW turns',cex.main=0.8)

hist(csbc@X[which(csbc@A%in%c(2,4)),1],breaks=seq(0,max(csbc@X[,1]), max(csbc@X[,1])/50),include.lowest=TRUE,xlim=range(csbc@X[,1]), 
     xlab='velocity (m/s)',main='velocity distribution for HIGH turns',cex.main=0.8)


# Or, we can see the binary split of turns separately for low and high values of velocity;
hist(csbc@X[which(csbc@A%in%c(1,2)),2],breaks=seq(0,pi,pi/50),
     xlab='turn (rad)',main='turn distribution for LOW velocities',cex.main=0.8)

hist(csbc@X[which(csbc@A%in%c(3,4)),2],breaks=seq(0,pi,pi/50),
     xlab='turn (rad)',main='turn distribution for HIGH velocities',cex.main=0.8)


# We can perform a fast visual check of the behaviourally annotated trajectory. 
# Each location is depicted with the colour of the cluster it belongs to; 
# A behavioural temporal profile is included the parameter lims allows focusing on a particular portion 
# of the trajectory;
view(csbc,lims=c(0,5000))



##### QUESTION 3 #####

# Are you happy with the output or do you feel there is anything wrong?

# HINT: always look to your data first, and remove the outliers or positional errors if necessary!
# If you are from the team shearwater, move forward!


# To generate a point-wise kml doc and that we can import to google earth or to a GIS software 
# You can also open the kml file directly in google earth if you already have it installed in your laptop
pkml(csbc)


# To create a new object with the behaviours classified for each point by EMbC
Behaviours <- csbc@A


# Now we can view a list of each unique behaviour 
unique_behaviours <- unique(Behaviours)
head(unique_behaviours)


# We can also merge the behaviours with our original tracking dataset 
cs_with_behaviours <- cbind(cs, Behaviour = Behaviours)
head(cs_with_behaviours)


install.packages('dplyr') # for data manipulation

library(dplyr) 


# Rename the labels with the matching behavioural classification
behaviour_labels <- c("Resting", "Intensive search", "Traveling", "Extensive search", "Not classified")


# Update the behaviour column with the labels created above
cs_with_behaviours <- cs_with_behaviours %>%
  mutate(Behaviour_class = behaviour_labels[Behaviours])  


# See the changes
head(cs_with_behaviours)


install.packages('suncalc') # to compute sun positioning, sunlight phases, moon position and lunar phase
install.packages('tidyverse') # for data manipulation, exploration and visualisation
install.packages('scales') # provides the internal scaling infrastructure used by ggplot2 (data visualisation)


library(suncalc) 
library(tidyverse) 
library(scales) 



##### SPOILER ALERT BEFORE THE NEXT FUNCTION! #####

# HINT 1: check for the first and last time of your tracking dataset (tracking duration);
# HINT 2: get the coordinates (in decimal degrees) for Corvo Island (Azores); 
# https://www.findlatitudeandlongitude.com

# ... and please complete the exercise first before you start looking for flights to Corvo ;)


DaytimeHours <- getSunlightTimes(
  date = seq.Date(as.Date("yyyy-mm-dd"), as.Date("yyyy-mm-dd"), by = 1),  
  keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"),
  lat = YY,  
  lon = -XX, 
  tz = "UTC")

	
install.packages('tidyr') # for data manipulation
install.packages('lubridate') # for data manipulation

library (tidyr) 
library (lubridate) 


# Ensure the Dategmt column is a character type (if it's not already)
cs_with_behaviours$Dategmt <- as.character(cs_with_behaviours$Dategmt)



# Split 'Dategmt' into 'Date' and 'Time', keeping the original column
cs_with_behaviours <- cs_with_behaviours %>%
  separate(Dategmt, into = c("Date", "Time"), sep = " ", remove = FALSE)


# Ensure 'Time' is properly formatted as hh:mm:ss (adding seconds if needed)
cs_with_behaviours$Time <- ifelse(nchar(cs_with_behaviours$Time) == 5, 
                                  paste0(cs_with_behaviours$Time, ":00"), 
                                  cs_with_behaviours$Time)


# Convert the 'Time' column to a time object 
cs_with_behaviours$Time <- hms(cs_with_behaviours$Time)



##### QUESTION 4 #####

# We want to create a new column in our dataset that tell us whether gps positions are occurring during 
# the day or night;
# But how do we know the exact time we should consider day and night periods?

# HINT: Inspect the objects you created with the information on sunrise and sunset hours;



# Define sunrise and sunset times in 'hh:mm:ss' format
sunrise <- hms("hh:mm:ss")  
sunset <- hms("hh:mm:ss")   


# Classify time of day as "Night" or "Day" based on comparison with sunrise/sunset times
cs_with_behaviours$Time_of_day <- ifelse(cs_with_behaviours$Time < sunrise | cs_with_behaviours$Time > sunset, 
                                         "Night", "Day")


# See the changes
View(cs_with_behaviours)


# Remove points with behaviours that were 'Not classified' 
cs_with_behaviours <- cs_with_behaviours %>% 
  filter(Behaviour_class != "Not classified")


# See the changes
head(cs_with_behaviours)


install.packages('ggplot2') # for data manipulation

library(ggplot2) 


# Prepare the summary data for both day and night
# Create a behaviour summary with counts and percentages
behaviour_summary <- cs_with_behaviours %>%
  group_by(Behaviour_class, Time_of_day) %>%
  summarise(count = n(), .groups = 'drop') %>%
  complete(Behaviour_class = unique(cs_with_behaviours$Behaviour_class), 
           Time_of_day, fill = list(count = 0)) %>%  # Fill missing with zero
  group_by(Time_of_day) %>%  # Group by Time_of_day for percentage calculation
  mutate(percentage = (count / sum(count)) * 100) %>%
  ungroup()  # Ungroup for final output



# Create a pie chart for Day behaviours
day_behaviours <- behaviour_summary %>%
  filter(Time_of_day == "Day") %>%
  ggplot(aes(x = "", y = percentage, fill = Behaviour_class)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Each Behaviour During Day",
       fill = "Behaviour") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())


# Create a pie chart for Night behaviours
night_behaviours <- behaviour_summary %>%
  filter(Time_of_day == "Night") %>%
  ggplot(aes(x = "", y = percentage, fill = Behaviour_class)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Each Behaviour During Night",
       fill = "Behaviour") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())


install.packages('ggpubr') # to create multiplots

library("ggpubr") 

Day_night_behaviours <- ggarrange(day_behaviours, night_behaviours)
Day_night_behaviours



##### QUESTION 5 #####

# To conclude, you got a pie chart with the summarise of behaviours displayed by individuals during the 
# day/ night periods;
# Now, you want to specifically look for the proportion of time spent in each behaviour during those periods;
# How can you get this information? How long did shearwaters spend resting during the night in comparison to day?

# HINT: Inspect the objects you created and find the one with the information that further feeds the pie chart;



# End of script

