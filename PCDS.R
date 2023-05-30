
# This script is used to calculate Guyana Red Howler Cue Density with 
# a point count distance sampling approach. 

rm(list=ls())

library(Distance)


# read the data

data(amakihi) # test data

# read audiostats for the grid (these are also the detections), from audiostats_PCDS
File_audiostats <- readRDS(paste0("E:/Triangulation 2023/clips/TR", letter.group, "/AudioStats_Results.rData"))

#filter on PeakPowerdensity (see script with relation) for calls that are whitin desired area
File_audiostats <- subset(File_audiostats, `Peak Power Density (dB FS/Hz)` >= -50)

#read temperatures for the files, made in Hakken&Plakken_PCDS
temp <- readRDS("E:/Triangulation 2023/temp_lengths.rData") #nog specifyen naar goede temp_length df

#add filenames, timestamps and convert temp to templength df
temp_lengths$Filenames <- gsub(".*/", "", temp_lengths$PATH)
temp_lengths$Timestamp <- gsub("\\..*$", " ", str_after_nth(temp_lengths$PATH, "_", -2))
temp_lengths$TEMP <- as.numeric(temp_lengths$TEMP)


#left_join (zie acoustic triangulation)
dfcues <- left_join(PD, temp_lengths, by = "Filenames")

#rename variables to glm variables (Peak Power to PeakPower, Avg power to AvgPower, temperature to TEMP)

#predict distances using glm 

#load glm from WIP power distance script
glm <- Distancepowerid_glm # glm(Distance ~ PeakPower, family=gaussian(link = "identity"), data=PD)
 

#predict distance
dfcues$Distance <- predict(glm, dfcues, type="response")


# Create the input data.frame

# ds_input should be a data.frame with the following columns:
# Sample.Label = recorder ID
# Effort = a numeric vector of 1's (times the point was visited)
# Region.Label = "CSG"
# Area = 10000 (for cues per square km)
# object = a numeric index indicating each detection
# distance = a numeric vector indicating the distance to each cue, in m
# temperature  = a numeric vector with temperature in degrees
# other covariates of interest

ds_input <- amakihi

# Fit the detection function

#willen we truncation op 1000 m zetten? 

ds_function <- ds(amakihi, truncation=150, transect="point", formula=~OBs)
plot(ds_function, showpoints=FALSE, pdf=TRUE)

# Add covariates of interest to the plot
add_df_covar_line(ds_function,
                  data.frame(OBs=na.omit(unique(amakihi$OBs))), pdf=TRUE)

# Calculate cue density
ds_densities <- dht2(ds_function, flatfile=amakihi, stratification="geographical",
                   strat_formula=~Region.Label)

# This should output: cues per square kilometer during the entire survey period

# Divide this by the estimated cue rate to get density





