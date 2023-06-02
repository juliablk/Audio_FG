# This script examines the relationship between peak power density (dB Fs /Hz) of howler calls
# and distance to receiver for Northern primary rainforest in French-Guyana

# for this script to run, please provide localize_df with triangulation coordinates of calls
# and df mics with for each recorder the east and north coordinate

# if you are ok with the parameters of the script (initially filter on dist <1500 m, aiSNR <1 and
#  avg Power density > -80) you can skip to line 184 (second ###### line)


# clear list
rm(list = ls())

# load libraries
library(dplyr)
library("sp") # library for calculating distances between two geographical points
library(ggplot2)
library(Rraven)
library(strex) # library for easily extracting substrings
library(data.table) # for the function %like%


###################### creating PD #######################################

# load the df
# create localize_df and mics.all with all grids

wd <- "E:/Triangulation 2023/"


letters.group <- c("A", "C", "D", "E", "B")

localizedf <- data.frame()
mics.all <- data.frame()

for (i in 1:length(letters.group)) {
  letter.group <- letters.group[i]


  X <- readRDS(paste0("E:/Triangulation 2023/cormatrices/TR", letter.group, "/localizedf_TR-", letter.group, ".rData"))
  Xs <- X %>%
    select((ncol(X) - 9):ncol(X)) %>%
    mutate(group = letter.group)

  localizedf <- rbind(localizedf, Xs)

  mics_X <- readRDS(paste0("E:/Triangulation 2023/cormatrices/TR", letter.group, "/mics_TR-", letter.group, ".rData"))
  mics_X <- mics_X %>% mutate(group = letter.group)

  mics.all <- rbind(mics.all, mics_X)
}


# create PD, a PowerDensity df with for all points the distance to recorder and the
# peak and average power of the recording on the recorder

PD <- data.frame()

for (i in 1:length(letters.group)) {
  letter.group <- letters.group[i]
  localize_df <- subset(localizedf, group == letter.group)
  mics <- subset(mics.all, group == letter.group)

  # Creating a dataframe with the coordinates of the howler calls as calculated by the localisation script
  Coordinates_localized <- data.frame(localize_df$name, localize_df$east, localize_df$north)

  # Copying the mics dataframe (so getting the coordinates for all recorders)
  Coordinates_recorders <- mics

  # Preparing for the upcoming for loop
  crs <- CRS("+lat_0=49 +lon_0=-2 +k=0.999601 +x_0=400000 +y_0=-100000 +ellps=airy +datum=0SGB36 +units=m +no_defs")
  Distances_to_recorders <- data.frame()

  # For loop that calculates the distances in metres between the coordinates of howler calls
  # and the coordinates of all recorders
  for (i in 1:length(Coordinates_recorders$deployment_ID)) {
    for (j in 1:length(Coordinates_localized$localize_df.name)) {
      Coordinate_recorder <- c(Coordinates_recorders$north[i], Coordinates_recorders$east[i])
      Coordinate_localized <- c(Coordinates_localized$localize_df.north[j], Coordinates_localized$localize_df.east[j])
      Coordinate_recorder_sp <- SpatialPoints(matrix(Coordinate_recorder, ncol = 2))
      Coordinate_localized_sp <- SpatialPoints(matrix(Coordinate_localized, ncol = 2))
      proj4string(Coordinate_recorder_sp) <- crs
      proj4string(Coordinate_localized_sp) <- crs
      distance <- spDists(Coordinate_localized_sp, Coordinate_recorder_sp)
      df <- data.frame(Coordinates_recorders$sensor_id[i], Coordinates_localized$localize_df.name[j], distance)
      colnames(df) <- c("Sensor ID", "Soundfile Timestamp", "Distance")
      Distances_to_recorders <- rbind(Distances_to_recorders, df)
    }
  }

  # Renaming things within the output dataframe of the for loop, so that the right file names can be selected
  # from the Audiostats dataframe later
  Distances_to_recorders$`Sensor ID` <- toupper(Distances_to_recorders$`Sensor ID`)
  Distances_to_recorders$filenames <- paste(Distances_to_recorders$`Sensor ID`, Distances_to_recorders$`Soundfile Timestamp`, sep = "_")
  Distances_to_recorders$filenames <- paste(Distances_to_recorders$filenames, ".wav", sep = "")
  Distances_to_recorders <- subset(Distances_to_recorders, select = -c(1, 2))


  # Importing the audiostats dataframe
  File_audiostats <- readRDS(paste0("E:/Triangulation 2023/clips/TR", letter.group, "/AudioStats_Results.rData"))

  # Making a dataframe of filenames, avg power density, and distance between origin of call and audio recorder.
  Power <- data.frame(
    File_audiostats$`Begin File`, File_audiostats$`Avg Power Density (dB FS/Hz)`,
    File_audiostats$artificialSNR, File_audiostats$`Peak Power Density (dB FS/Hz)`
  )
  colnames(Power) <- c("filenames", "Avg Power Density", "aiSNR", "Peak Power Density")
  Power$filenames <- sub("_2k", "", Power$filenames)
  Power <- Power[Power$filenames %in% Distances_to_recorders$filenames, ]
  Power_and_distance <- left_join(Power, Distances_to_recorders, by = "filenames")
  colnames(Power_and_distance) <- c("Filenames", "AvgPower", "aiSNR", "PeakPower", "Distance")

  # Removing files from said dataframe that were not taken into account for the localisation function
  # Aka, files that were put to NA in the Toa/sounds/localize_df dataframes

  PD <- rbind(PD, Power_and_distance)
}

PD <- unique(PD)

# temperatuur hieraan koppelen
temp_lengths <- data.frame()

# import temp_length for all groups
for (i in 1:length(letters.group)) {
  letter.group <- letters.group[i]

  temp_length <- readRDS(paste0("E:/Triangulation 2023/cormatrices/TR", letter.group, "/templengthsTR-", letter.group, ".rData"))
  temp_lengths <- rbind(temp_lengths, temp_length)
}

PD_above1500 <- PD

# now we have PD with all howler localizations with distance to recorder <1500
saveRDS(temp_lengths, "E:/Triangulation 2023/templengths_ABCDE.rData")


# add filenames, timestamps and convert temp to templength df
temp_lengths$Filenames <- gsub(".*/", "", temp_lengths$PATH)
temp_lengths$Timestamp <- gsub("\\..*$", " ", str_after_nth(temp_lengths$PATH, "_", -2))
temp_lengths$TEMP <- as.numeric(temp_lengths$TEMP)


# left_join
PD_above1500 <- left_join(PD_above1500, temp_lengths, by = "Filenames")
PD_above1500$Timestamp <- substr(PD_above1500$Timestamp, 1, 15)
saveRDS(PD_above1500, "E:/Triangulation 2023/PowerDistanceTemp_ABCDE.rData")



##########   excluding files not close enough to middle rec  #################################

# read the df
PD_above1500 <- readRDS("E:/Triangulation 2023/PowerDistanceTemp_ABCDE.rData")


# filter on distance lower than 1500 m
PD <- subset(PD_above1500, Distance <= 1500)


# filter on AiSNR and Avg Power density
PD <- subset(PD, aiSNR <= 1.0 & AvgPower >= -80)

# read deployments (this needs to be changed to updated deployments file)
deployments <- read.csv("C:/Users/Beheerder/Documents/UU/A GSLS/MinPr/R/Audio2023/deployment_tr.csv", sep = ";")

# remove audiomods
Audiomods <- c("A23", "A21", "A02", "A27", "A200", "A26", "A18", "A13")
deployments <- deployments[!deployments$sensor_id %in% Audiomods, ]

# select files with high peak power density and avg power density on the middle rec of the grid
deployments <- deployments[, colnames(deployments) %in% c("UTMX.AVG", "UTMY.AVG", "elevation", "deployment_id", "sensor_id")]

# vector with grid ids for in the loop
letters.group <- c("A", "C", "D", "E", "B")

# add the id of the centre rec
middle.numbers <- c(8, 8, 7, 8, 8)

groups_middlenumbers <- as.data.frame(cbind(letters.group, middle.numbers))


# df to store the powerdistance values for files with high enough avgpower and peakpower for the centre rec
PD_powercentre <- data.frame()


# for each grid, select the files from calls that were close enough to the centre
for (i in 1:length(letters.group)) {
  letter <- letters.group[i]
  middle.number <- groups_middlenumbers$middle.numbers[groups_middlenumbers == letter]



  # select centre recorder
  centre_df <- deployments[grepl(paste0("TR-", letter, middle.number), deployments$deployment_id), ]
  centre_rec <- centre_df$sensor_id

  # exlude files from timestamps with too low peakpower and average power for centre recorder

  # get files from centre rec from the current grid
  PD.i <- subset(PD, PATH %like% paste0("TR-", letter))
  PD.centre <- subset(PD.i, Filenames %like% centre_rec)

  # select timestamps to remove
  PD.remove <- subset(PD.centre, AvgPower < -70 | PeakPower < -40)
  Timestamps.remove <- PD.remove$Timestamp

  # remove the timestamps
  PD.i <- PD.i[!PD.i$Timestamp %in% Timestamps.remove, ]

  # bind te dfs together

  PD_powercentre <- rbind(PD_powercentre, PD.i)
}

PD_powercentre$Timestamp <- substr(PD_powercentre$Timestamp, 1, 15)

############################  running generalized linear models  #################################

# creating log and ^2 versions of variables

PD_powercentre$logDistance <- log(PD_powercentre$Distance)
PD_powercentre$Distance2 <- PD_powercentre$Distance^2
PD_powercentre$AvgPower2 <- PD_powercentre$AvgPower^2
PD_powercentre$trialPower <- 100 + PD_powercentre$AvgPower
PD_powercentre$PeakPower2 <- PD_powercentre$PeakPower^2


###############  looking at power ~ dist relationship ###############3

# selecting <=1000 distances for relationship peakpower and distances sub 1000 m, as those
# localizations are more reliable
PD_powercentrex <- subset(PD_powercentre, Distance <= 1000)


# first looking at the relation between PeakPower and Distance
# models with Peak Power
PeakPowerlogdistance_lm <- lm(PeakPower ~ log(Distance), data = PD_powercentrex)
PeakPowerpolydistance_lm <- lm(PeakPower ~ Distance2 + Distance, data = PD_powercentrex)
PeakPowerdistance_lm <- lm(PeakPower ~ Distance, data = PD_powercentrex)



summary(PeakPowerlogdistance_lm)
summary(PeakPowerpolydistance_lm)
summary(PeakPowerdistance_lm)


# which model is the best?
aic_logdistance <- AIC(PeakPowerlogdistance_lm)
aic_polydistance <- AIC(PeakPowerpolydistance_lm)
aic_distance <- AIC(PeakPowerdistance_lm)

# alle modellen liggen AIC <2 bij elkaar, sws kijken welke link het beste werkt bij dist ~ power model



ggplot(
  PD_powercentrex,
  aes(x = Distance, y = PeakPower)
) +
  geom_point(shape = 1) +
  stat_smooth(
    method = "lm",
    formula = y ~ log(x),
    col = "red"
  )

# relatie ziet er decent uit, fijn, maar punten wel erg breed verspreid
# als we punten tot 1000 mee willen nemen lijkt -50 dB me een goede cutoff. maar, sws
# moeten we de punten nog opschonen want voor 0-500 m zijn er best wel wat punten tussen de -50 en -60
# peak power density.. ik hoop dat dit punten zijn die dicht bij de rand liggen maar ver van de middelste

layout(matrix(c(1, 2, 3, 4), 2, 2)) # optional 4 graphs/page
plot(PeakPowerlogdistance_lm)
plot(PeakPowerpolydistance_lm)
plot(PeakPowerdistance_lm)

# alle qqplots zien er oke uit

ggplot(PD_powercentrex, aes(x = PeakPower)) +
  geom_histogram(bins = 60)

# histogram lijkt ook normaal genoeg


###################  looking at distance ~ power relationship ######################

# met filter op peakpower middelste rec -40 (loop) en peakpower alle recs -50 of hoger
# want we willen calls die luid en close enough zijn voor voorspellen distance
PD_powercentre <- subset(PD_powercentre, PeakPower >= -70)


# optioneel filter <1000 erbij
# PD powercentre <- subset(PD_powercentre, Distance <= 1000)

# now the real deal, looking at dist ~ power relation

Distancepolypower_glm <- glm(Distance ~ PeakPower2 + PeakPower, family = gaussian(link = "log"), data = PD_powercentre)
Distancepolypowerid_glm <- glm(Distance ~ PeakPower2 + PeakPower, family = gaussian(link = "identity"), data = PD_powercentre)
Distancepower_glm <- glm(Distance ~ PeakPower, family = gaussian(link = "log"), data = PD_powercentre)
Distancepowerid_glm <- glm(Distance ~ PeakPower, family = gaussian(link = "identity"), data = PD_powercentre)

# log kan niet want negatieve waardes voor Power
# Distancelogpower_glm <- glm(Distance ~ log(PeakPower), family=gaussian(link = "log"), data=PD)


# met temp
Distancepolypowertemp_glm <- glm(Distance ~ PeakPower2 + PeakPower + TEMP, family = gaussian(link = "log"), data = PD_powercentre)
Distancepolypowertempid_glm <- glm(Distance ~ PeakPower2 + PeakPower + TEMP, family = gaussian(link = "identity"), data = PD_powercentre)
Distancepowertemp_glm <- glm(Distance ~ PeakPower + TEMP, family = gaussian(link = "log"), data = PD_powercentre)
Distancepowertempid_glm <- glm(Distance ~ PeakPower + TEMP, family = gaussian(link = "identity"), data = PD_powercentre)


summary(Distancepolypowertemp_glm)
summary(Distancepower_glm)
summary(Distancepowerid_glm)
summary(Distancepolypowertempid_glm)
summary(Distancepowertempid_glm)

aic_polypowerlog <- AIC(Distancepolypower_glm)
aic_polypowerid <- AIC(Distancepolypowerid_glm)
aic_powerlog <- AIC(Distancepower_glm)
aic_powerid <- AIC(Distancepowerid_glm)

aic_polypowertemplog <- AIC(Distancepolypowertemp_glm)
aic_polypowertempid <- AIC(Distancepolypowertempid_glm)
aic_powertemplog <- AIC(Distancepowertemp_glm)
aic_powertempid <- AIC(Distancepowertempid_glm)

# het alle modellen liggen AIC <2 bij elkaar, het beste model is Distancepowertempid,
# maar omdat modellen minder dan 2 verschillen kiezen we het simpelste,
# dus Distancepowerid_glm

# bijbehorend plot, ziet er niet suuuper uit
ggplot(
  PD_powercentre,
  aes(x = PeakPower, y = Distance)
) +
  geom_point(shape = 1) +
  stat_smooth(
    method = "glm",
    formula = y ~ x,
    col = "red"
  )



layout(matrix(c(1, 2, 3, 4), 2, 2)) # optional 4 graphs/page
plot(Distancepowerid_glm)

# qqplots zijn oke, wel een lichte afwijking van normaal voor hogere values

# hoe ziet het histogram van distance eruit?
ggplot(PD_powercentre, aes(x = Distance)) +
  geom_histogram(bins = 60)

# dus rechterstaart ook te zien in histogram, again, we kunnen de relatie maken voor punten onder de 1000 m
# dist, omdat localization voor hogere distances wss minder accurate is.. we kunnen dit doen
# door afstand tot middelste rec op een max te zetten.

# misschien eerst even kijken waarom dit fout is gegaan:


# nu wil je voor deze punten eigenlijk de localization checken...
# dus kijk naar localizedf all (gemaakt in het eerste deel van dit script)
colnames(localizedf)[which(names(localizedf) == "name")] <- "Timestamp"

PD_powercentre_xl <- left_join(PD_powercentre, localizedf, by = "Timestamp")

PD_above1000 <- subset(PD_powercentre_xl, Distance >= 1000)
PD_above750 <- subset(PD_powercentre_xl, Distance >= 750)

# look for which timestamps this is the case
Timestamps_above1000 <- unique(PD_above1000$Timestamp)
Timestamps_above750 <- unique(PD_above750$Timestamp)


# to improve the scattering of points, only base the relationship on points with likely localizations
# to make sure that we trust localizations, we take localizations <=600 m of centre recorders



# df to store the powerdistance values for files with high enough avgpower and peakpower for the centre rec
PD_powerdistcentre <- data.frame()


# store bad localizations for examination
bad_localizations <- data.frame()
deleted_localizations <- data.frame()


# for each grid, select the files from calls that were close enough to the centre
for (i in 1:length(letters.group)) {
  letter <- letters.group[i]
  middle.number <- groups_middlenumbers$middle.numbers[groups_middlenumbers == letter]



  # select centre recorder
  centre_df <- deployments[grepl(paste0("TR-", letter, middle.number), deployments$deployment_id), ]
  centre_rec <- centre_df$sensor_id

  # exlude files from timestamps with too low peakpower and average power for centre recorder

  # get files from centre rec from the current grid
  PD.c <- subset(PD_above1500, PATH %like% paste0("TR-", letter)) # above1500 because in the other PDs, centre files could have been removed due to filters
  PD.centre <- subset(PD.c, Filenames %like% centre_rec)
  PD.x <- subset(PD.c, Filenames %like% "ALOUATTA")

  # select timestamps to remove
  PD.remove <- subset(PD.centre, Distance > 600)
  Timestamps.remove <- PD.remove$Timestamp


  # remove the timestamps

  # subset powerdistance for this grid
  PD.i <- subset(PD_powercentre, PATH %like% paste0("TR-", letter))
  Del_localizations <- PD.i[PD.i$Timestamp %in% Timestamps.remove, ]
  PD.i <- PD.i[!PD.i$Timestamp %in% Timestamps.remove, ]


  # bind te dfs together
  PD_powerdistcentre <- rbind(PD_powerdistcentre, PD.i)

  # store bad localizations for examination
  bad_localizations <- rbind(bad_localizations, PD.remove)
  deleted_localizations <- rbind(deleted_localizations, Del_localizations)
}

# the df with localizations that are max. 600 m from the centre recs of the grid
View(PD_powerdistcentre)

# localizations that are furhter than 600 m from the centre
View(bad_localizations)

View(deleted_localizations)

# All power distances (so for the other recs as well instead of only the centre rec) that belong to the bad localizations
deleted_localizationsxl <- left_join(deleted_localizations, localizedf, by = "Timestamp")

# add info about the localizations
colnames(localizedf)[which(names(localizedf) == "name")] <- "Timestamp"
PD_powerdistcentre_xl <- left_join(PD_powerdistcentre, localizedf, by = "Timestamp")


# the plot without localizations to far from the centre:
ggplot(
  PD_powerdistcentre_xl,
  aes(x = PeakPower, y = Distance)
) +
  geom_point(shape = 1) +
  stat_smooth(
    method = "glm",
    formula = y ~ x,
    col = "red"
  )


# improved slightly, but still a few high distance values left.
# we will first take a look at what went wrong for the removed values that had a
# distance that was too high for their corresponding PeakPower density value.

# goodtimestamps <- unique(PD_powerdistcentre_xl$Timestamp)
# sub_localize <- localizedf[localizedf$Timestamp %in% goodtimestamps,]


# after checking the deleted audiofiles, one audiofile (SMA11405_20230408_065322.wav) had a very loud signal caused by a cicade next to it,
# causing distortion of the file. see if this warbleR function takes it out

# rewrote the warbleR function find_clipping and amended our selection tables to assess the probability of 'clipping' aka
# amplitude saturation of our files. Sadly, it does not produce probabilities larger than 0 for most files, including
# SMA11405_20230408_065322.wav

library(warbleR)

# df to store clipping probablities for each file
df_clipped <- data.frame()

for (i in 1:length(letters.group)) {
  letter <- letters.group[i]
  print(letter)
  path1 <- paste0("E:/Triangulation 2023/clips/TR", letter, "/AudioStats_Results1")
  path2 <- paste0("E:/Triangulation 2023/clips/TR", letter, "/AudioStats_Results2")

  File_audiostats1 <- imp_raven(
    path = path1, all.data = TRUE, freq.cols = FALSE,
    warbler.format = T, name.from.file = TRUE,
    ext.case = "lower"
  )

  if (dir.exists(path2)) {
    File_audiostats2 <- imp_raven(
      path = path2, all.data = TRUE, freq.cols = FALSE,
      warbler.format = T, name.from.file = TRUE,
      ext.case = "lower"
    )

    File_audiostats <- rbind(File_audiostats1, File_audiostats2)
  } else {
    File_audiostats <- File_audiostats1
  }


  File_audiostats$Timestamp <- sub("_2k.wav", "", str_after_first(File_audiostats$`Begin File`, "_"))

  # vervang back slash naar forward slash voor file path
  File_audiostats$`Begin Path` <- gsub("\\\\", "/", File_audiostats$`Begin Path`)

  # vervang TableOfHope.wav met echte filename
  File_audiostats$sound.files <- File_audiostats$`Begin File`
  File_audiostats$start <- 20
  File_audiostats$Filenames <- gsub("_2k", "", File_audiostats$`Begin File`)
  File_audiostatsx <- left_join(File_audiostats, temp_lengths, by = "Filenames")

  # set start and end times per file (currently present as amount of seconds since start of the FIRST file in the selection table)
  File_audiostatsx$end <- as.numeric(File_audiostatsx$LENGTH) + 20
  File_audiostatsy <- File_audiostatsx[, 1:26]
  colnames(File_audiostatsy)[which(names(File_audiostatsy) == "TEMP.x")] <- "TEMP"
  colnames(File_audiostatsy)[which(names(File_audiostatsy) == "Timestamp.x")] <- "Timestamp"


  powerdiststamps <- unique(PD_powercentre_xl$Timestamp)
  File_audiostats_sub <- subset(File_audiostats, Timestamp %in% powerdiststamps)
  df_stats <- data.frame()

  for (i in 1:nrow(File_audiostats_sub)) {
    print(i)

    X <- File_audiostats_sub[i, ]


    wv <- tuneR::readWave(
      filename = X$`Begin Path`,
      header = FALSE, units = "seconds", from = X$start,
      to = X$end, toWaveMC = F
    )


    bit_ranges <- data.frame(bit = c(
      1, 8, 16, 24, 32,
      64
    ), low = c(
      -1, 0, -32767, -8388607, -2147483647,
      -1
    ), high = c(
      1, 254, 32767, 8388607, 2147483647,
      1
    ))
    bit_range <- bit_ranges[bit_ranges$bit == wv@bit,
      c("low", "high"),
      drop = TRUE
    ]
    prop.clipped <- sum(wv@left >= bit_range$high) / length(wv@left)
    if (wv@stereo) {
      prop.clipped <- mean(prop.clipped, sum(wv@right >=
        bit_range$high) / length(wv@right))
    }

    sound.files <- X$sound.files
    selec <- X$selec
    row.i <- cbind(sound.files, selec, prop.clipped, letter)



    df_stats <- rbind(df_stats, row.i)
  }



  df_clipped <- rbind(df_clipped, df_stats)
}



# df with files with probability > 0
df_clipped_above0 <- subset(df_clipped, prop.clipped > 0)

# only a few files with probability larger than 0, checked those files, some have a single saturation line,
# changing peak power density, but for all files average power density seems unaffected by saturation

# we can use this on all our future files and check the prob>0 files to exclude files with large clipped proportions that are maybe present,
# but for this subset it does not improve the data.

# going back to the initial deleted_files from the PD_powercentre df with too high distances for their PeakPower value.

# Found a new problem. For CEBUS_20230421_063512_2k.wav there is no howler call present but aiSNR is smaller than 1. I propose setting aiSNR
# filter more strict.

PD_powerdistSNR <- subset(PD_powerdistcentre_xl, aiSNR < 0.90)

# the plot with aiSNR<0.9:
ggplot(
  PD_powerdistSNR,
  aes(x = PeakPower, y = Distance)
) +
  geom_point(shape = 1) +
  stat_smooth(
    method = "glm",
    formula = y ~ poly(x,2),
    col = "red"
  )

#look at the outliers of plot



# the old plot for comparison
ggplot(
  PD_powerdistcentre,
  aes(x = PeakPower, y = Distance)
) +
  geom_point(shape = 1) +
  stat_smooth(
    method = "glm",
    formula = y ~ x,
    col = "red"
  )

# oke so there are less outliers again, a lot of the low peak power at close distance points have disappeared and also
# some low peak power points at very far distances which is nice.

# checking the outliers
PD_above750 <- subset(PD_powerdistSNR, Distance >= 750)


#trying glm with avg power as well

#old model
Distancepowertempid_glm <- glm(Distance ~ PeakPower + TEMP, family = gaussian(link = "identity"), data = PD_powerdistSNR)
Distancepowerid_glm <- glm(Distance ~ PeakPower, family = gaussian(link = "identity"), data = PD_powerdistSNR)

#with avg power
Distanceavgpowertempid_glm <- glm(Distance ~ PeakPower + AvgPower + TEMP, family = gaussian(link = "identity"), data = PD_powerdistSNR)
Distanceavgpowerid_glm <- glm(Distance ~ PeakPower + AvgPower, family = gaussian(link = "identity"), data = PD_powerdistSNR)

aic_pt <- AIC(Distancepowertempid_glm)
aic_p <- AIC(Distancepowerid_glm)
aic_ppt <- AIC(Distanceavgpowertempid_glm)
aic_pp <- AIC(Distanceavgpowerid_glm)

#model with peak and average power without temperature performs best!

#look at the residuals and select those that are large
residuals <- abs(Distanceavgpowerid_glm$residuals )
mean(residuals)

high_res <- residuals[residuals > 200]
names(high_res)

#extract filenames
high_resfiles <- PD_powerdistSNR[rownames(PD_powerdistSNR) %in% names(high_res),]

#look at files
#first three file seem normal..


################    write a predictive model  ######################################

#the final model
Distancepower_glm <- glm(Distance ~ PeakPower + TEMP, family = gaussian(link = "identity"), data = PD_powerdistSNR)


# use model to predict value of Distance
PD_powerdistSNR$PredictedDistance <- predict(Distanceavgpowerid_glm, PD_powerdistSNR, type = "response")

#look if the relation between real distances and predicted distances is ok (plotted lm line should fall roughly on black y=x line)
ggplot(
  PD_powerdistSNR,
  aes(x = Distance, y = PredictedDistance)
) +
  geom_point(shape = 1) +
  stat_smooth(
    method = "glm",
    formula = y ~ x,
    col = "red"
  ) +  ylim(0,500) + xlim(0,500) +
  geom_abline(intercept = 0, slope = 1) # add a y = x line to see if predicted distance match distances reasonably
 
#not yet the case





#some code for pretty plots
x <- 1:10
y <- jitter(x^2)

DF <- data.frame(x, y)

p <- ggplot(DF, aes(x = x, y = y)) +
  geom_point() +
  stat_smooth(method = "lm", aes(colour = "linear"), se = FALSE) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), aes(colour = "polynomial"), se = FALSE) +
  stat_smooth(method = "nls", formula = y ~ a * log(x) + b, aes(colour = "logarithmic"), se = FALSE, start = list(a = 1, b = 1)) +
  stat_smooth(method = "nls", formula = y ~ a * exp(b * x), aes(colour = "Exponential"), se = FALSE, start = list(a = 1, b = 1))

plot(p)
