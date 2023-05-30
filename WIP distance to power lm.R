
# This script examines the relationship between peak power density (dB Fs /Hz) of howler calls
# and distance to receiver for Northern primary rainforest in French-Guyana

# for this script to run, please provide localize_df with triangulation coordinates of calls 
# and df mics with for each recorder the east and north coordinate 


#clear list
rm(list=ls())

#load libraries
library(dplyr)
library("sp") #library for calculating distances between two geographical points 
library(ggplot2)
library(Rraven)
library(strex)


#load the df
#maak localize_df met alle grids

wd = "E:/Triangulation 2023/"

# dirs <- c("TR-A", "TR-C", "TR-D", "TR-E", "TR-B")
# groups <- c("TRA", "TRC", "TRD", "TRE",  "TR-B")
letters.group <- c("A", "C", "D", "E", "B")

localizedf <- data.frame()
mics.all <- data.frame()

for(i in 1:length(letters.group)){
  
letter.group = letters.group[i]


X <- readRDS(paste0("E:/Triangulation 2023/cormatrices/TR", letter.group, "/localizedf_TR-", letter.group, ".rData"))
Xs <-  X %>% select((ncol(X)-9) : ncol(X)) %>% mutate(group = letter.group)

localizedf <- rbind(localizedf, Xs)

mics_X <- readRDS(paste0("E:/Triangulation 2023/cormatrices/TR", letter.group, "/mics_TR-", letter.group, ".rData"))
mics_X <- mics_X %>% mutate(group = letter.group)

mics.all <- rbind(mics.all, mics_X)

}



PD <- data.frame()

for (i in 1:length(letters.group)){

  
  
letter.group = letters.group[i] 
localize_df <- subset(localizedf, group == letter.group)
mics <- subset(mics.all, group == letter.group)

# Creating a dataframe with the coordinates of the howler calls as calculated by the localisation script
Coordinates_localized<- data.frame(localize_df$name, localize_df$east, localize_df$north)

#Copying the mics dataframe (so getting the coordinates for all recorders)
Coordinates_recorders<- mics

#Preparing for the upcoming for loop
crs<- CRS("+lat_0=49 +lon_0=-2 +k=0.999601 +x_0=400000 +y_0=-100000 +ellps=airy +datum=0SGB36 +units=m +no_defs")
Distances_to_recorders<- data.frame()

#For loop that calculates the distances in metres between the coordinates of howler calls
#and the coordinates of all recorders
for (i in 1:length(Coordinates_recorders$deployment_ID)){
  for (j in 1:length(Coordinates_localized$localize_df.name)){
    Coordinate_recorder<- c(Coordinates_recorders$north[i], Coordinates_recorders$east[i])
    Coordinate_localized<- c(Coordinates_localized$localize_df.north[j], Coordinates_localized$localize_df.east[j])
    Coordinate_recorder_sp<- SpatialPoints(matrix(Coordinate_recorder, ncol=2))
    Coordinate_localized_sp<- SpatialPoints(matrix(Coordinate_localized, ncol=2))
    proj4string(Coordinate_recorder_sp)<- crs
    proj4string(Coordinate_localized_sp)<- crs
    distance<- spDists(Coordinate_localized_sp, Coordinate_recorder_sp)
    df<- data.frame(Coordinates_recorders$sensor_id[i], Coordinates_localized$localize_df.name[j], distance)
    colnames(df)<- c("Sensor ID", "Soundfile Timestamp", "Distance")
    Distances_to_recorders<- rbind(Distances_to_recorders, df)
  }
}

#Renaming things within the output dataframe of the for loop, so that the right file names can be selected
#from the Audiostats dataframe later
Distances_to_recorders$`Sensor ID`<- toupper(Distances_to_recorders$`Sensor ID`)
Distances_to_recorders$filenames<- paste(Distances_to_recorders$`Sensor ID`, Distances_to_recorders$`Soundfile Timestamp`, sep="_")
Distances_to_recorders$filenames<- paste(Distances_to_recorders$filenames, ".wav", sep="")
Distances_to_recorders<- subset(Distances_to_recorders, select= -c(1,2))


#Importing the audiostats dataframe
File_audiostats <- readRDS(paste0("E:/Triangulation 2023/clips/TR", letter.group, "/AudioStats_Results.rData"))

#filter on AiSNR and Avg Power density
File_audiostats <- subset(File_audiostats, artificialSNR <= 1.0 &
                            `Avg Power Density (dB FS/Hz)` >= -80)

# File_audiostats<- imp_raven(path="E:/Triangulation 2023/TRA_AudioStats_Raven_Results",all.data = TRUE, freq.cols = FALSE,
#                             warbler.format=F, name.from.file = TRUE,
#                             ext.case ="lower")

#Making a dataframe of filenames, avg power density, and distance between origin of call and audio recorder.
Power<- data.frame(File_audiostats$`Begin File`, File_audiostats$`Avg Power Density (dB FS/Hz)`, File_audiostats$`Peak Power Density (dB FS/Hz)`)
colnames(Power)<- c("filenames", "Avg Power Density", "Peak Power Density")
Power$filenames<- sub("_2k", "", Power$filenames)
Power<- Power[Power$filenames %in% Distances_to_recorders$filenames,]
Power_and_distance<- left_join(Power, Distances_to_recorders, by = "filenames")
colnames(Power_and_distance)<- c("Filenames", "AvgPower", "PeakPower", "Distance")

#Removing files from said dataframe that were not taken into account for the localisation function
#Aka, files that were put to NA in the Toa/sounds/localize_df dataframes

#willen we dit wel? lijkt me dat we ze wel mee kunnen nemen voor afstand
## als de sound pressure minimaal een bepaalde waarde heeft kan het wel, anders niet
## dus Thomas zn lines uitgecomment
# 
# micnames<- c("Chiropotes", "Alouatta", "Saimiri", "SMA11410", "Ateles", "Pithecia", "SMA11381", "SMA11405")
# micnames<- toupper(micnames)
# Localized<- localize_df
# colnames(Localized)[1:8]<- micnames
# nas<- which(is.na(Localized), arr.ind=T)
# where_are_nas<- data.frame(rownumber= nas[,1], columnname= colnames(Localized)[nas[,2]])
# Filenames_nas<- Localized$name[where_are_nas$rownumber]
# where_are_nas$Filenames<- Filenames_nas
# Files_to_drop<- paste(where_are_nas$columnname, where_are_nas$Filenames, sep="_")
# Files_to_drop<- paste(Files_to_drop, ".wav", sep="")
# Power_and_distance<- Power_and_distance[!(Power_and_distance$Filenames %in% Files_to_drop),]



Power_and_distance <- subset(Power_and_distance, Distance <=1500) 
PD <- rbind(PD, Power_and_distance)

}

PD3 <- PD
#temperatuur hieraan koppelen
temp_lengths <- data.frame()

#import temp_length voor alles
for (i in 1:length(letters.group)){

letter.group = letters.group[i]

temp_length <- readRDS(paste0("E:/Triangulation 2023/cormatrices/TR", letter.group, "/templengthsTR-", letter.group, ".rData"))
temp_lengths <- rbind(temp_lengths, temp_length)

}


#now we have PD with all howler localizations with distance to recorder <1500, aiSNR <1 and
#avgPower >-80

saveRDS(PD, "E:/Triangulation 2023/PowerDistance_ABCDE.rData")
saveRDS(temp_lengths, "E:/Triangulation 2023/templengths_ABCDE.rData")


PD <- readRDS("E:/Triangulation 2023/PowerDistance_ABCDE.rData")

#add filenames, timestamps and convert temp to templength df
temp_lengths$Filenames <- gsub(".*/", "", temp_lengths$PATH)
temp_lengths$Timestamp <- gsub("\\..*$", " ", str_after_nth(temp_lengths$PATH, "_", -2))
temp_lengths$TEMP <- as.numeric(temp_lengths$TEMP)


#left_join (zie acoustic triangulation)
PD <- left_join(PD, temp_lengths, by = "Filenames")
saveRDS(PD, "E:/Triangulation 2023/PowerDistanceTemp_ABCDE.rData")



##########   excluding files not close enough to middle rec  #################################

#read the df
PD <- readRDS( "E:/Triangulation 2023/PowerDistanceTemp_ABCDE.rData")

#read deployments
deployments <- read.csv("C:/Users/Beheerder/Documents/UU/A GSLS/MinPr/R/Audio2023/deployment_tr.csv", sep = ";")

#remove audiomods
Audiomods <- c("A23", "A21", "A02", "A27", "A200", "A26", "A18", "A13")
deployments <- deployments[!deployments$sensor_id %in% Audiomods,]

#filter for files for which the file has a high enough peak power density on the middle rec of the grid
deployments <- deployments[,colnames(deployments) %in% c("UTMX.AVG","UTMY.AVG","elevation","deployment_id","sensor_id")]

#vector with grid ids for in the loop
letters.group <- c("A", "C", "D", "E", "B")

#add the id of the centre rec
middle.numbers <- c( 8,  8 , 7, 8 , 8)
groups_middlenumbers <- as.data.frame(cbind(letters.group, middle.numbers))


#df to store the powerdistance values for files with high enough avgpower and peakpower for the centre rec
PD_powercentre <- data.frame()


#for each grid, select the files from calls that were close enough to the centre
for (i in 1:length(letters.group)){



letter <- letters.group[i]
middle.number <- groups_middlenumbers$middle.numbers[groups_middlenumbers == letter]



#select centre recorder
centre_df <- deployments[grepl(paste0("TR-", letter, middle.number), deployments$deployment_id),]
centre_rec <- centre_df$sensor_id

#exlude files from timestamps with too low peakpower and average power for centre recorder

#get files from centre rec from the current grid 
PD.i <- subset(PD, PATH %like% paste0("TR-", letter)) 
PD.centre <- subset(PD.i, Filenames %like% centre_rec)

#select timestamps to remove
PD.remove <- subset(PD.centre, AvgPower < -70 | PeakPower < -40)
Timestamps.remove <- PD.remove$Timestamp

#remove the timestamps
PD.i <- PD.i[!PD.i$Timestamp %in% Timestamps.remove, ]

#bind te dfs together

PD_powercentre <- rbind(PD_powercentre, PD.i)

}


############################  running generalized linear models  #################################

#creating log and ^2 versions of variables

PD$logDistance <- log(PD$Distance)
PD$Distance2 <- PD$Distance^2
PD$AvgPower2 <- PD$AvgPower^2
PD$trialPower <- 100 + PD$AvgPower
PD$PeakPower2 <- PD$PeakPower^2 

#selecting <=1000 distances 
#PD <- subset(PD, Distance <= 1000)


#first looking at the relation between PeakPower and Distance
# models with Avg Power
PeakPowerlogdistance_lm<- lm(PeakPower ~ log(Distance), data=PD)
PeakPowerpolydistance_lm <- lm(PeakPower ~ Distance2 + Distance, data=PD)
PeakPowerdistance_lm <- lm(PeakPower ~ Distance, data = PD)
GPPlogdist <- glm(PeakPower ~ log(Distance), family = Gamma, data=PD)
?glm()

summary(PeakPowerlogdistance_lm)
summary(PeakPowerpolydistance_lm)
summary(PeakPowerdistance_lm)


#which model is the best?
aic_logdistance <- AIC(PeakPowerlogdistance_lm) 
aic_polydistance <- AIC(PeakPowerpolydistance_lm)
aic_distance <- AIC(PeakPowerdistance_lm)

#log doet het het beste, dus voor dist ~ power de log link!

ggplot(PD_powercentre,
       aes(x = Distance, y = PeakPower)) + 
  geom_point(shape = 1) + 
  stat_smooth(method = "lm", 
              formula = y ~ poly(x,2), 
              col = "red")


ggplot(PD_powercentre,
       aes(x = Distance, y = PeakPower)) + 
  geom_point(shape = 1) + 
  stat_smooth(method = "lm", 
              formula = y ~ log(x), 
              col = "red")



layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(PeakPowerlogdistance_lm)
plot(PeakPowerpolydistance_lm)

ggplot(PD_powercentre, aes(x=Distance)) + 
  geom_histogram(bins = 60)

shapiro.test(PD$PeakPower)
#ziet er niet super normaal uit, eerder gamma (maar dit hoeven
# we niet te predicten haha dus maakt niet uit)


#now the distance ~ peakpower model with maybe temp

PD <- subset(PD, PeakPower >= -60)
PD_powercentre <- subset(PD_powercentre, PeakPower >= -50)

#use calls that are loud enough, so close enough!




Distancepolypower_glm <- glm(Distance ~ PeakPower2 + PeakPower, family=gaussian(link = "log"), data=PD)
Distancepolypowerid_glm <- glm(Distance ~ PeakPower2 + PeakPower, family=gaussian(link = "identity"), data=PD)
Distancepower_glm <- glm(Distance ~ PeakPower, family=gaussian(link = "log"), data=PD)
Distancepowerid_glm <- glm(Distance ~ PeakPower, family=gaussian(link = "identity"), data=PD)
Distancepolypowertemp_glm <- glm(Distance ~ PeakPower2 + PeakPower + TEMP, family=gaussian(link = "log"), data=PD)
Distancepolypowertempx_glm <- glm(Distance ~ PeakPower2 + PeakPower * TEMP, family=gaussian(link = "log"), data=PD)
#log kan niet want negatieve waardes voor Power
#Distancelogpower_glm <- glm(Distance ~ log(PeakPower), family=gaussian(link = "log"), data=PD)


#met filter op peakpower middelste rec -40 (loop) en peakpower alle recs -50 of hoger
PD_powercentre <- subset(PD_powercentre, PeakPower >= -50)


#ook filter <1000 erbij
PD_powercentre <- subset(PD_powercentre, Distance <= 1000)

#creating log and ^2 versions of variables

PD_powercentre$logDistance <- log(PD_powercentre$Distance)
PD_powercentre$Distance2 <- PD_powercentre$Distance^2
PD_powercentre$AvgPower2 <- PD_powercentre$AvgPower^2
PD_powercentre$trialPower <- 100 + PD_powercentre$AvgPower
PD_powercentre$PeakPower2 <- PD_powercentre$PeakPower^2 


Distancepolypower_glm <- glm(Distance ~ PeakPower2 + PeakPower, family=gaussian(link = "log"), data=PD_powercentre)
Distancepolypowerid_glm <- glm(Distance ~ PeakPower2 + PeakPower, family=gaussian(link = "identity"), data=PD_powercentre)
Distancepower_glm <- glm(Distance ~ PeakPower, family=gaussian(link = "log"), data=PD_powercentre)
Distancepowerid_glm <- glm(Distance ~ PeakPower, family=gaussian(link = "identity"), data=PD_powercentre)


#met temp
Distancepolypowertemp_glm <- glm(Distance ~ PeakPower2 + PeakPower + TEMP, family=gaussian(link = "log"), data=PD_powercentre)
Distancepolypowertempid_glm <- glm(Distance ~ PeakPower2 + PeakPower + TEMP, family=gaussian(link = "identity"), data=PD_powercentre)
Distancepowertemp_glm <- glm(Distance ~  PeakPower + TEMP, family=gaussian(link = "log"), data=PD_powercentre)
Distancepowertempid_glm <- glm(Distance ~  PeakPower + TEMP, family=gaussian(link = "identity"), data=PD_powercentre)


summary(Distancepolypowertemp_glm)
summary(Distancepower_glm)
summary(Distancepolypowertempx_glm)
summary(Distancepowerid_glm)

aic_polypowerlog <- AIC(Distancepolypower_glm)
aic_polypowerid <- AIC(Distancepolypowerid_glm)
aic_powerlog <- AIC(Distancepower_glm)
aic_powerid <- AIC(Distancepowerid_glm)

aic_polypowertemplog <- AIC(Distancepolypowertemp_glm)
aic_polypowertempid <- AIC(Distancepolypowertempid_glm)
aic_powertemplog <- AIC(Distancepowertemp_glm)
aic_powertempid <- AIC(Distancepowertempid_glm)

aic_polypowertemplog <- AIC(Distancepolypowertemp_glm)
aic_polypowertemplogx <- AIC(Distancepolypowertempx_glm)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(Distancepower_glm)

#dus distance polypower temp met log doet het het beste!
# also gaussian want is gelukkig normaal verdeeld (zie hist en 
# qqplot)


#plot, ziet er niet suuuper uit maar ach
ggplot(PD_powercentre,
       aes(x = PeakPower, y = Distance)) + 
  geom_point(shape = 1) + 
  stat_smooth(method = "glm", 
              formula = y ~ x, 
              col = "red")

#schrijf een predictive model

#generate random samples from your data
nrow(PD)
PDsample <- PD[1:500,]
nrow(PDsample)

PDsample_below50 <- subset(PD, PeakPower >= -50)


#use model to predict value of Distance
PDsample_below50$PredictedDistance <- predict(Distancepolypowertemp_glm, PDsample_below50, type="response")

PD_powercentre$PredictedDistance <- predict(Distancepower_glm, PD_powercentre, type="response")


ggplot(PD_powercentre,
       aes(x = Distance, y = PredictedDistance)) + 
  geom_point(shape = 1) + 
  stat_smooth(method = "glm", 
              formula = y ~ x, 
              col = "red") +  geom_abline(intercept = 0, slope = 1) # add a y = x line to see if the errors scale linearly


Distancepower_glm <- glm(Distance ~ PeakPower + TEMP, family=gaussian(link = "log"), data=PDsample_below50)

#use model to predict value of Distance
PDsample_below50$PredictedDistance <- predict(Distancepower_glm, PDsample_below50, type="response")



x <- 1:10
y <- jitter(x^2)

DF <- data.frame(x, y)

p <- ggplot(DF, aes(x = x, y = y)) + geom_point() +
  stat_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se= FALSE) +
  stat_smooth(method = 'nls', formula = y ~ a * log(x) +b, aes(colour = 'logarithmic'), se = FALSE, start = list(a=1,b=1)) +
  stat_smooth(method = 'nls', formula = y ~ a*exp(b *x), aes(colour = 'Exponential'), se = FALSE, start = list(a=1,b=1))

plot(p)


PD2$

