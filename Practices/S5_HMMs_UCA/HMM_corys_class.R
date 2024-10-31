
# The majority of this code was adapted from the Clay et al (2019). Journal of Applied Ecology; 
# https://besjournals.onlinelibrary.wiley.com/doi/10.1111/1365-2656.13267



# Also, there were some other functions adapted from several demos codes available online, like:

# DFO 2022 Hidden Markov Model Workshop by Marie Auger-Méthé;
# https://github.com/MarieAugerMethe/DFO_HMM_2022

# Behavior and_ paceUse Workshop by Josh Cullen;
# https://joshcullen.github.io/Behavior_and_SpaceUse_Workshop/workshop-materials.html

# Statistical Methods Seminar Series by Théo Michelot;
# https://github.com/eco4cast/Statistical-Methods-Seminar-Series/tree/main/michelot_movement



# One think that we have to keep in mind, is that we should run HMMs separately for each dataset,
# as differences in sampling interval mean that the suitable values (particularly for step lengths) 
# for the main states will be different; 



# Clean the environment directory;
rm(list=ls()) 
ls()


# Set the working directory where the code is stored;
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


# Install required packages
install.packages('ggplot2') # Installs ggplot2 for plotting
install.packages('sf') # Installs sf for spatial/GIS data handling
install.packages('sp') # Installs sp, another package for spatial data handling
install.packages('lubridate') # Installs lubridate for handling dates and times
install.packages('dplyr') # Installs dplyr for data manipulation
install.packages('adehabitatLT') # Installs adehabitatLT for trajectory interpolation
install.packages('momentuHMM') # Installs momentuHMM for Hidden Markov Models on movement data


# Load libraries
library(ggplot2) # Loads ggplot2 for data visualization
library(sf) # Loads sf for handling spatial/GIS data
library(sp) # Loads sp for handling spatial data
library(lubridate) # Loads lubridate for date and time handling
library(dplyr) # Loads dplyr for data manipulation
library(adehabitatLT) # Loads adehabitatLT for trajectory interpolation
library(momentuHMM) # Loads momentuHMM for analyzing movement with HMMs
source("utility_functions.R") # Sources custom functions from "utility_functions.R"


# Data pre-processing required to run HMMs 


# Load the full tracking dataset;
data <- read.csv('cs_dta_hmm.csv')
names(data) # Shows column names in the dataset
head(data) # Displays the first few rows of data


# Converts bird ID column to a factor
data$ID <- as.factor(as.character(data$ID)) 
nlevels(data$ID) # Counts the number of unique birds (levels of the ID factor)


# Plot the trips for each bird
ggplot(data, aes(x, y)) + geom_path(aes(colour = ID))         


# Convert the time column to POSIXct;
data$time <- as.POSIXct(data$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")


# Plot data time intervals;
plot(table(diff(data$time)), xlim = c(0, 500),  xlab = "time interval (min)", ylab = "count")


# Create adehabitat trajectory padded with NAs;
data_ade <- setNA(ltraj = as.ltraj(xy = data[, c("x", "y")], 
                                   date = data$time, 
                                   id = data$ID), 
                  date.ref = data$time[1], 
                  dt = 15, tol = 30, units = "min") # Interpolates trajectory with 15-minute time steps, using NAs for missing values


# Transform back to dataframe;
data_na <- ld(data_ade)[, c("id", "x", "y", "date")] # Converts the ltraj object back to a dataframe
colnames(data_na) <- c("id", "x", "y", "date") # Renames columns



# Add temperature and instantaneous speed for non-missing locations;
data_na$temperature <- NA # Adds a temperature column initialized with NA
data_na$temperature[which(!is.na(data_na$x))] <- data$temperature # Fills temperature column with NA values from the original data
data_na$speed <- NA
data_na$speed[which(!is.na(data_na$x))] <- data$speed


# See the changes
View(data_na)


# Setting the initial parameters to run a HMM 


# For details read this short guide to choosing initial parameter values for the estimation in moveHMM
# https://cran.r-project.org/web/packages/moveHMM/vignettes/moveHMM-starting-values.pdf


# Prepare data for HMM (compute step lengths and turning angles);
# type = "LL" means that we will use longitude ('x') - latitude ('y') data
data_hmm1 <-prepData(data_na, type = "LL", coordNames = c("x", "y"))  # Computes step lengths and turning angles for HMM
head(data_hmm1)


# Explicitly assign the correct ID to the data_hmm1 object
data_hmm1$ID <- data_na$id  # Preserve the original ID from data_na
head(data_hmm1)


# Plotting step lengths (distance an animal moves between consecutive observations);
hist(data_hmm1$step, breaks = 20)
#abline(v=1, col='red', lwd=2)
#abline(v=0.50, col='red', lwd=1, lty='dashed')
#abline(v=7, col='blue', lwd=2)
#abline(v=4, col='blue', lwd=1, lty='dashed')
#abline(v=13, col='green', lwd=3)
#abline(v=10, col='green', lwd=1, lty='dashed')


# The order of these labels correspond to the one in our final results;
stateNames<-c("travel","search", "rest") # Defines names for behavioral states in HMM 



# Step length initial values, based on the histogram distribution;
# (if only 1 peak -> 2-state hmm; if > 1 peak; 3-state hmm, and so on...);
shape_0 <- c(10, 7, 1) # Initial shape parameters for step length in each state


# When the gamma distribution is used to model step lengths, the standard deviation is usually 
# of the same order of magnitude as the mean of the distribution; 
scale_0 <- c(10, 7, 1)  # Initial scale (variance) parameters for step length in each state
stepPar0 <- c(shape_0,scale_0)


# HMMs do not run with step length = 0; so we should remove them;
ind_zero <- which(data_hmm1$step == 0) # Finds indices with step length = 0
if (length(ind_zero)>0){
  data_hmm1$step[ind_zero] <- runif(length(ind_zero))/10000 # Replaces step length = 0 with small random values
}
ind_zero <- which(data_hmm1$step == "NA") # Finds indices with NA step length
if (length(ind_zero)>0){
  data_hmm1$step[ind_zero] <- runif(length(ind_zero))/10000 # Replaces NA step lengths with small random values
}



# Plotting turning angles (angle between the direction of movement from one location to another);
hist(data_hmm1$angle)


# Turning angle initial values;
# We assume they all have mean of around '0' (i.e. no bias in a particular direction);
# The value can not be '0', so we set to '0.01';
# However, the concentration can differ between behavioural states.
location_0 <- c(0.01, 0.01, 0.01) # Initial mean values for turning angle


# To choose the concentration values look for the von Mises theoretically distribution:   
# https://cran.r-project.org/web/packages/moveHMM/vignettes/moveHMM-starting-values.pdf


# concentration (i.e. how directed the movement is) 
concentration_0 <- c(20, 1, 10) # Initial concentration parameters for turning angle (higher = more directional)
anglePar0 <- c(location_0,concentration_0)


# Running an initial 3-state hmm based on the mean observed step lengths and turning angles


# Gamma distribution for step lengths and von Mises for turning angles;
hmm1 <- fitHMM(data=data_hmm1, nbStates=3,
             dist=list(step="gamma",angle="vm"),
             Par0=list(step=stepPar0, angle=anglePar0),
             control = list(maxit = 1),
             estAngleMean = list(angle=TRUE), 
             stateNames = stateNames)


# Model outputs
print(hmm1) # Displays model summary
AIC(hmm1) # Calculates AIC for model comparison (lower is better)


# Plot distribution of states and estimated state sequences over time
plot(hmm1)


# Calculate pseudo-residuals;
pseudo_res <- pseudoRes(hmm1)


# Plot histograms of step length residuals;
hist(pseudo_res$stepRes, breaks = 20, col = 'skyblue', 
     main = 'Histogram of Step Length Residuals', xlab = 'Residuals')


# Plot histograms of turning angle residuals;
hist(pseudo_res$angleRes, breaks = 20, col = 'lightcoral', 
     main = 'Histogram of Angle Residuals', xlab = 'Residuals')



# Extracting behavioral states for each locations

# Get most likely sequence of states (Viterbi algorithm);
head(viterbi(hmm1))


# Save most likely state sequences from hmm as a new column
data_hmm1$state <- factor(viterbi(hmm1))


# Save our data with the behavioural states
write.table(data_hmm1, file="cs_hmm_states.txt", quote=F, row.names=F, sep="\t") # export to .txt




####### INCLUDE COVARIATES IN THE MODEL #######


# Setting the parameters to run a hmm with covariates uing the estimates from the best model


# As it takes a very long time (i.e hours) to run hmms with covariates - we'll subset to 2 individuals
subset_data_na <- data_na %>% filter(id %in% c("30B_LV10287", "25_L93194"))

# Check the result
head(subset_data_na)


# Prepare data for HMM (compute step lengths and turning angles);
data_hmm3 <-prepData(subset_data_na, type = "LL", covNames = c("temperature", "speed"), coordNames = c("x", "y"))
head(data_hmm3)


# Explicitly assign the correct ID to the data_hmm2 object
data_hmm3$ID <- subset_data_na$id  # Preserve the original ID from data_na
head(data_hmm3)


# The order of these labels correspond to the one in our final results;
stateNames<-c("travel","search", "rest")


# Step length values based on the first hmm estimates
shape_0 <- c(4.624939, 1.136302, 0.16329792) # setting shape (i.e. means for the 3 states; travel, search and rest);
scale_0 <- c(1.561878, 1.290396, 0.08166287)  # setting variance (i.e. standard deviaton);
stepPar0 <- c(shape_0,scale_0)


# Hmms don't run with step length = 0, so we should remove them from our data;
ind_zero <- which(data_hmm3$step == 0)
if (length(ind_zero)>0){
  data_hmm1$step[ind_zero] <- runif(length(ind_zero))/10000
}
ind_zero <- which(data_hmm3$step == "NA")
if (length(ind_zero)>0){
  data_hmm1$step[ind_zero] <- runif(length(ind_zero))/10000
}


# Turning angle values based on the first hmm estimates;
location_0 <- c(0.02551571, 0.07455359, 0.02687547) # mean values;
concentration_0 <- c(6.40189920, 0.33963921, 11.67758926) # concentration (large values = more concentrated angles);
anglePar0 <- c(location_0,concentration_0)




# Running 3-state hmm using the estimates from the best model


# Gamma distribution for step lengths and von mises for turning angles;
hmm3 <- fitHMM(data=data_hmm3, nbStates=3,
               dist=list(step="gamma",angle="vm"),
               Par0=list(step=stepPar0, angle=anglePar0),
               estAngleMean = list(angle=TRUE), 
               stateNames = stateNames,
               formula= ~ temperature + speed)


# Model outputs; shows what model selects as "best" parameters for different states;
print(hmm3) # higher  value (less negative) indicates a better fit of the model to the observed data;
AIC(hmm3) # the model with the lowest AIC is typically the best supported;


# calculate pseudo-residuals;
pseudo_res <- pseudoRes(hmm3)


# Plot histograms of step length residuals;
hist(pseudo_res$stepRes, breaks = 20, col = 'skyblue', 
     main = 'Histogram of Step Length Residuals', xlab = 'Residuals')


# plot histograms of turning angle residuals
hist(pseudo_res$angleRes, breaks = 20, col = 'lightcoral', 
     main = 'Histogram of Angle Residuals', xlab = 'Residuals')



# Plot estimated distributions and transition probabilities as functions of temperature and wind 
plot(hmm3, plotTracks = FALSE, ask = FALSE, plotCI = TRUE)


# Plot stationary state probabilities as functions of temperature and speed. 
# Stationary probabilities represent the equilibrium of the Markov process (i.e. whether it changes as time progresses)
plotStationary(hmm3, plotCI = TRUE)



# End of script