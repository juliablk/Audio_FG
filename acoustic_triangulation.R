
rm(list=ls())

library(readxl)
library(bioacoustics)
library(data.table)
library(Rraven)
library(lubridate)
library(plyr)
library(dplyr)
library(data.table)
library(strex)

# currently this script works per grid, so takes 1 set of 8 microphones and 
# their batch correlations made in raven

TR.group = "TR-B"
TRgroup = "TRB"

#wd <- "E:/test_points/Grouped/_clipped_ds"

#TOATestPoints <- read.csv("E:/test_points/test points2.csv", sep=";")



# This script localizes an acoustic signal in 2 or 3D space based on differential time of arrival.

# We use the localize function from the Sound Finder package for R. It requires four variables:
# 'mics' =  a data frame with columns 'east', 'north', and 'z' (optional) which are 3D-coordinates
# of the acoustic recording units (ARUs).

##### Make 'mics' ##############

################################

#ARU_metadata <- read_excel("C:/Users/Beheerder/Documents/UU/A GSLS/MinPr/R/Audio2023/deployment_tr.xlsx", 
#                           sheet = "ARUs 2023")

ARU_metadata <- read.csv("C:/Users/Beheerder/Documents/UU/A GSLS/MinPr/R/Audio2023/deployment_tr.csv", sep = ";")


#ARU_metadata <- ARU_metadata[ARU_metadata$make == "Song Meter",]
  
mics <- ARU_metadata[,colnames(ARU_metadata) %in% c("UTMX.AVG","UTMY.AVG","elevation","deployment_id","sensor_id")]

#select mics from the current grid
mics <- mics[grepl(TR.group, mics$deployment_id),]

#if present, remove audiomods
Audiomods <- c("A23", "A21", "A02", "A27", "A200", "A26", "A18", "A13")
mics <- mics[!mics$sensor_id %in% Audiomods,]


colnames(mics) <- c("deployment_ID","sensor_id","z","east","north")
mics <- with (mics, mics [order (sensor_id) , ])

# sensors were installed at 2 m height 
mics$z <- mics$z+2
#mics <- mics[, !names(mics) %in% "z"]

# 'sounds' =  a data frame with columns 't1', 't2', ..., 'tn', indicating the relative time of arrival (TOA)
# of a signal between recorders (same order as 'mics'). Missing values are allowed. There is also a column
# 'Temp' which should yield the temperature registered by the recorder when the sound was detected.

############### Make 'sounds' ##############

###########################################

# Import recording metadata (mostly for temperature)

# Import all audio files
# audio_files <- list.files(path=wd,recursive=T,pattern=".wav",full.names = T)
# audio_files <- normalizePath(audio_files)
# audio_files <- audio_files[!(grepl("Grouped",audio_files))]
# audio_files <- audio_files[file.size(audio_files)>1000000]
## RUN THIS OVERNIGHT

# Collect metadata
#audio_md <- lapply(audio_files,function(x){as.data.frame(bioacoustics:::guano_md(x))})
#audio_md <- do.call("rbind.fill",audio_md)
#audio_md$fp <- paste0("F:/Triangulation 2023/Grouped/",audio_md$Original.Filename)

#saveRDS(audio_md,"audio_md.rData")
audio_md <- readRDS("E:/Triangulation 2023/audio_md_ABCDE.rData")
audio_md$Timestamp <- sub("^[^_]*_(.*)\\..*$", "\\1", audio_md$Original.Filename)

#temp_length <- readRDS("C:/Users/Beheerder/Documents/UU/A GSLS/MinPr/R/Audio2023/lengthsTRC.rData")
temp_length <- readRDS(paste0("E:/Triangulation 2023/cormatrices/", TRgroup, "/templengths", TR.group, ".rData"))


temp_length$file <- gsub(".*/", "", temp_length$PATH)
temp_length$Timestamp <- gsub("\\..*$", " ", str_after_nth(temp_length$PATH, "_", -2))

####### First estimate clock drift for each recorder ###############


###################################################################

# Import all text files
wd <- paste0("E:/Triangulation 2023/cormatrices/", TRgroup)
corr_mats <- list.files(wd,pattern="*.txt",recursive=T)

# Get the start signal file

start_file <- corr_mats[which(corr_mats %like% "start")]
start_file <- data.frame(imp_corr_mat(start_file,path=wd)[[2]]) #2 because we want the time lag in s
start_file <- start_file[order(names(start_file)),order(names(start_file))]

start_file_dt  <- sub("^[^_]*_(.*)\\..*$", "\\1", colnames(start_file))[1]
start_file_dt <- parse_date_time(start_file_dt, orders =c("%Y%m%d_%H%M%S"))

# You can see that in practice the sensors do not start at the same time.
# So here we find the recorder that starts first, and then calculate how
# much later the other recorders start


# select the row with only negative values and a 0, turn the negative values positive
offset <- as.numeric(abs(start_file %>%
             rowwise() %>%
             filter(all(c_across(everything()) <= 0,na.rm=T))))

# Get the end signal file

end_file <- corr_mats[which(corr_mats %like% "end")]
end_file <- data.frame(imp_corr_mat(end_file,path=wd)[[2]])
end_file <- end_file[order(names(end_file)),order(names(end_file))]


end_file_dt  <- sub("^[^_]*_(.*)\\..*$", "\\1", colnames(end_file))[1]
end_file_dt <- parse_date_time(end_file_dt, orders =c("%Y%m%d_%H%M%S"))

time_passed <- seconds(end_file_dt)-seconds(start_file_dt)

# Estimated drift between recorders
drift_mat <- end_file-start_file

######################################################

# Now load the actual call recordings, extract ToA at each sensor,
# get temperature, and correct for drift.

####################################################

# Import all cross correlation matrices
#call_corr_mats <- list.files(paste0(wd, "/OutputCorrelator/OutputCorrelatorA0405_220440"),pattern="*.txt",recursive=T)
#call_corr_mats <- list.files("E:/test_points/Grouped/_clipped_ds")

#if peaks & lags are together and matrices from raven
call_corr_mats <- list.files(wd, pattern="*.txt",recursive=F, full.names = F)

#remove start & end file
call_corr_mats <- call_corr_mats[!call_corr_mats %like% "start"]
call_corr_mats <- call_corr_mats[!call_corr_mats %like% "end"]

#listing files again but with fullnames for read.csv (if lags & peaks are split)
call_corr_mats <- list.files(wd, pattern="*.txt",recursive=F, full.names = T)
lags <- call_corr_mats[call_corr_mats %like% "lag"]
peaks <- call_corr_mats[call_corr_mats %like% "peak"]


File_audiostats <- readRDS(paste0("E:/Triangulation 2023/clips/", TRgroup, "/AudioStats_Results.rData"))
# File_audiostats<- imp_raven(path="E:/Triangulation 2023/TRA_AudioStats_Raven_Results",all.data = TRUE, freq.cols = FALSE,
#                             warbler.format=F, name.from.file = TRUE,
#                             ext.case ="lower")


## Options to make lists of the files that meet certain audio statistic requirements (for keeping)

#AvgEntropy_under_x<- File_audiostats$`Begin File`[File_audiostats$`Avg Entropy (bits)`<=2.1]
#MaxEntropy_under_x<- File_audiostats$`Begin File`[File_audiostats$`Max Entropy (bits)`<=3.6]
AvgPowerDensity_above_x<- File_audiostats$`Begin File`[File_audiostats$`Avg Power Density (dB FS/Hz)`>=-80]
#AvgEnergy_above_x<- File_audiostats$`Begin File`[File_audiostats$`Energy (dB FS)`>= -20]
#SNR_above_x<- File_audiostats$`Begin File`[File_audiostats$`SNR NIST Quick (dB)`>=11.5]
AiSNR_below_x <- File_audiostats$`Begin File`[File_audiostats$artificialSNR <= 1]


## Lines that limit the File_audiostats dataframe to those rows that contain the stats for files that should be removed

#File_audiostats<- File_audiostats[!(File_audiostats$`Begin File` %in% AvgEntropy_under_x),]
#File_audiostats<- File_audiostats[!(File_audiostats$`Begin File` %in% MaxEntropy_under_x),]
File_audiostats.AvgP<- File_audiostats[!(File_audiostats$`Begin File` %in% AvgPowerDensity_above_x),]
#File_audiostats<- File_audiostats[!(File_audiostats$`Begin File` %in% AvgEnergy_above_x),]
#File_audiostats<- File_audiostats[!(File_audiostats$`Begin File` %in% SNR_above_x),]
File_audiostats.AiSNR<- File_audiostats[!(File_audiostats$`Begin File` %in% AiSNR_below_x),]


## Line that makes a list of the sound files that should be removed from the correlation matrices
Filenames_to_remove<- unique(c(File_audiostats.AvgP$`Begin File`, File_audiostats.AiSNR$`Begin File`))
#Filenames_to_remove <- c() #if no audiostats restrictions





call_list <- list()

timestamp_name <- c()

for(i in 1:length(call_corr_mats)){
  
  
  # Load the correlation matrix from Raven
  
  #input <- data.frame(imp_corr_mat(call_corr_mats[i],path= wd) [[2]]) #2 because we want the lag(s) and not the amount of overlap
  input <- read.csv(lags[i])
  
  # Put rows and cols in alphabetical order to make sure they match with 'mics'
  input <- input[order(names(input)),order(names(input))]
  
  # Recorder start time is different so subtract the offset
  if(nrow(input)!= nrow(start_file)) { break} #hier nog iets anders voor vinden
  input <- input - start_file
  
  # Get timestamp
  timestamp_name[i]  <- sub("^[^_]*_(.*)\\..*$", "\\1", colnames(input))[1]
  timestamp_name[i] <- substr(timestamp_name[i], 1,15) #if there is sth behind the timestamp (such as 2k) remove it
  
  timestamp <- parse_date_time(timestamp_name[i], orders =c("%Y%m%d_%H%M%S"))
  
  # Assuming linear clock drift, estimate drift that has occurred since the start signal
  time_since_signal <- seconds(timestamp)-seconds(start_file_dt)
  
  # Drift since the start signal
  drift_mat_timestamp <-  drift_mat * as.numeric(time_since_signal)/as.numeric(time_passed) 
  
  # Correct for it
  input <- input - drift_mat_timestamp
  
  #################
  
  # Here we need to make some corrections because cross correlation is not 
  # perfect. If the estimated time lag is too high to be realistic, we
  # set it to NA. 
  
  # Here we could also add other filters, e.g. if the signal to noise
  # ratio is too low, we also set it to NA. 
  
  ################
  
  rnames_input<- rownames(input)
  #rnames_input <- gsub("_2k", "", rnames_input) #if there is sth like _2k make sure to remove it
  File_removal_check<- rnames_input %in% Filenames_to_remove
  temprownumbers<- c(1:length(rnames_input))
  File_removal_check<- data.frame(File_removal_check, temprownumbers)
  Files_to_remove<- File_removal_check$temprownumbers[File_removal_check$File_removal_check==T]
  input[,Files_to_remove]<- NA
  input[Files_to_remove,]<- NA

  
  input <- input %>% mutate_all(funs(ifelse(abs(.)>=2,NA,.)))
  #input <- input %>% mutate_all(funs(ifelse(abs(.)>=1,NA,.)))
  
  #here we set all lags with a low correlation peak to NA
  
  #load correlation peak matrix
  #peaks <- data.frame(imp_corr_mat(call_corr_mats[i],path= wd) [[1]])
  peaks.i <- read.csv(peaks[i])
  
  #get index of values below 0.3
  z <- which(peaks.i < 0.2, arr.ind = TRUE)
  
  #set these values to NA in the matrix
  input[cbind(z[, 1], z[, 2])] <- NA 
  


  # Get mean temperature across the sensors
  #sub_md <- audio_md[audio_md$Original.Filename %like% timestamp_name[i],]
  #temp = mean(sub_md$Temperature.Int)
 
  sub_temp <- temp_length[temp_length$Timestamp %like% timestamp_name[i],]
  temp <- mean(as.numeric(sub_temp$TEMP))
  
  
  # Get Time of Arrivals relative to the first arrival at a recorder
  ToA <- abs(input %>%
               rowwise() %>%
               filter(all(c_across(everything()) <= 0,na.rm=T)))
  
  # If too many correlations are wrong it has multiple options for first recorder,
  # which should be the row with the most detections
  if(nrow(ToA)>1){
    ToA <- ToA[which.min(rowSums(is.na(ToA))),]
    
  
  }

  call_list[[i]] <-  as.numeric(c(ToA,temp))
  
}

# Format for the localize function
sounds <- data.frame(do.call("rbind",call_list))


#sounds <- ToA
#sounds$temp <- 25.75
colnames(sounds) <- c("t1","t2","t3","t4","t5","t6", "t7", "t8", "temp")
#paste("t", 1:nrow(mics), sep="") <- daar iets mee doen

# Add call timestamp
sounds$name <- timestamp_name

#mics <- mics[!mics$sensor_id == "CEBUS",]

localize <- function(mics, sounds, temps=NULL, tol=1e-10) {
  ## maybe read mic positions from a file
  if (is.character(mics))
    mics = read.csv(mics, as.is=TRUE)
  
  ## maybe read sound times from a file
  ## and if so, remember the name
  sounds.from.file = is.character(sounds)
  if (sounds.from.file) {
    sounds.filename = sounds
    sounds = read.csv(sounds, as.is=TRUE)
  }
  
  ## the required names of TOADS / timestamps columns, given the number of mics
  mic.cols = paste("t", 1:nrow(mics), sep="")
  
  ## ensure the correct TOADS / timestamps columns are present in sounds
  if (! all ( mic.cols %in% names(sounds)))
    stop("the sounds object must have a column for TOADS or timestamp at each mic,\nand these must be named t1, t2, ... ", tail(mic.cols, 1))
  
  ## ensure the temps are specified for each sound, if no temps parameter is given
  if (is.null(temps) && is.null(sounds$temp))
    stop("you must either specify a number, data.frame, or filename for temps,\nor the sounds object must have a column named 'temp'")
  
  ## maybe read temps from a file
  if (is.character(temps))
    temps = read.csv(temps, as.is=TRUE)
  
  ## get the temperature for each sound
  if (!is.null(sounds$temp)) {
    ## temperatures are specified for each sound
    temps = sounds$temp
  } else if (is.data.frame(temps)) {
    ## temperatures are given as a time series, so apply an interpolation to the mean TOADS / timestamp
    ## for each sound
    temps = spline(temps$time, temps$temp, xout=apply(as.matrix(sounds[, mic.cols]), 1, mean, na.rm=TRUE), method="natural")$y
  } else {
    ## re-use the value in temps for each sound
    temps = rep(temps, length=nrow(sounds))
  }
  
  ## the number of sounds to localize
  n = nrow(sounds)
  
  ## we're doing 3d if "z" is specified for microphone locations
  is.3d = ! is.null(mics$z)
  
  ## names of coordinate columns for microphones
  mic.coords = cbind(mics$east, mics$north, mics$z)
  
  ## add columns to the input dataframe for the estimates
  sounds$east = sounds$north = sounds$v = numeric(n)
  if (is.3d)
    sounds$z = numeric(n)
  sounds$err.millisecs = sounds$err.metres = sounds$err.rms = sounds$time = numeric(n)
  
  ## for improved numerical stability, translate coordinates
  ## so the origin is at their mean; we'll convert back
  ## after doing the estimation
  
  origin = apply(mic.coords, 2, mean)
  
  mic.coords = sweep(mic.coords, 2, origin)
  
  ## keep track of whether we've warned the user about reversed timestamps
  have.warned = FALSE
  
  ## loop over each sound
  for (i in 1:nrow(sounds)) {
    
    ## keep only mics for which the sound was detected
    keep <- ! is.na(sounds[i, mic.cols])
    used.mics = mic.coords[keep,]
    t = as.numeric(sounds[i, mic.cols[keep]])
    
    ## if all signs are non-positive, assume we need to negate
    ## them (we want increasing time to represent "later", as usual)
    
    if (all(t <= 0)) {
      t = -t
      if (! have.warned) {
        
        warning("localizF: times in the sounds matrix should be oriented so that larger
 is further in the future; I'm negating the values you supplied, as they are all non-positive,
which suggests their orientation is reversed")
        
        have.warned = TRUE
      }
    }
    
    ## calculate the speed of sound, given the interpolated
    ## temperature at the time of the first microphone's detection of
    ## this sound
    
    v = 331.3 * sqrt(1 + temps[i] / 273.15)
    
    ## get "mean" time for translating below (we kludge this and always use 64
    ## as the denominator, to match SoundFinder.xls)
    mean.t = sum(t) / 64
    t = t - mean.t
    
    ## invoke the GPS solver
    
    soln = gps(used.mics, t, v, tol)
    
    ## fill in the output row
    
    sounds$east[i]          = soln$east + origin[1]
    sounds$north[i]         = soln$north + origin[2]
    if (is.3d)
      sounds$z[i]           = soln$z + origin[3]
    sounds$time[i]          = soln$time + mean.t
    sounds$v[i]             = v
    sounds$err.rms[i]       = soln$err.rms
    sounds$err.metres[i]    = soln$err.space
    sounds$err.millisecs[i] = soln$err.time * 1000
  }
  
  if (sounds.from.file) {
    ## write solution-augmented input back to the file it came from
    write.csv(sounds, file=sounds.filename, row.names=FALSE)
  }
  
  return (sounds)
}

gps = function(positions, times, v, tol=1e-10)  {
  ## convert the dataframe to a matrix
  ## we use the notation of reference [2]
  ## The "z" column need not be present.
  
  ## either 2 or 3 dimensions
  dim = ncol(positions)
  
  ## lorentz coefficients are 1 for spatial, -1 for time
  lorentz.coef = c(rep(1, dim), -1)
  
  ## lorentz inner product
  lorentz = function(x1, x2=x1) {
    return (sum (lorentz.coef * x1 * x2))
  }
  
  ## the pseudo range
  rho = - v * times
  
  ## add the pseudorange column to get B
  B = cbind(positions, rho)
  
  ## the vector of ones
  e = rep(1, nrow(B))
  
  ## the vector of squared lorentz norms 
  a = 0.5 * apply(B, 1, lorentz)
  
  ## use the pseudo-inverse of B to get B+ e, B+ a
  ## via QR decomposition of B
  
  qrB = qr(B, tol = tol)
  Binv.e = solve(qrB, e, tol = tol)
  Binv.a = solve(qrB, a, tol = tol)
  
  ## the coefficients for the quadratic equation in lambda
  
  cA = lorentz(Binv.e)
  cB = 2 * (lorentz(Binv.e, Binv.a) - 1)
  cC = lorentz(Binv.a)
  
  desc = cB^2-4*cA*cC
  
  ## if the descriminant is negative, we cheat and
  ## set it to zero, so that we still get a solution,
  ## albeit a probably not very good one!
  
  if (desc < 0)
    desc = 0
  
  ## solve the quadratic
  lambda = (-cB + c(-1, 1) * sqrt(desc)) / (2*cA)
  
  ## get the solution for each possible lambda
  
  u1 = solve(qrB, (a + lambda[1] * e), tol = tol)
  u2 = solve(qrB, (a + lambda[2] * e), tol = tol)
  
  ## return the solution with the lower sum of squares
  ## discrepancy
  
  s1 = sum( (B %*% u1 - (a + lambda[1] * e))^2)
  s2 = sum( (B %*% u2 - (a + lambda[2] * e))^2)
  
  u = if (s1 < s2) u1 else u2
  
  err.rms = sqrt(min(s1, s2) / nrow(B))
  
  ## rms space error 
  err.space =  sqrt(mean(apply(B, 1,
                               function(x) {
                                 (sqrt(sum((u[1:dim]-x[1:dim])^2)) + x[1+dim] + u[1+dim])^2
                               })))
  ## rms time error
  err.time = err.space / v
  
  u = as.list(u)
  names(u) = c("east", "north", if(dim == 3) "z", "time")
  
  ## convert distance offset to time offset
  u$time = u$time / v
  
  ## return as a list to allow simpler indexing of components, and append errors
  return(c(u, err.rms=err.rms, err.space=err.space, err.time=err.time))
}


# Remove NAs (there should be 3 or more non NA's )
sounds <- sounds[rowSums(is.na(sounds))< ncol(sounds)-4,]

sounds$temp <- as.numeric(sounds$temp)
localize_df <- localize(mics = mics, sounds = sounds, temps = NULL, tol=1e-10)

saveRDS(localize_df, paste0("E:/Triangulation 2023/cormatrices/", TRgroup, "/localizedfSNR_", TR.group, ".rData"))
saveRDS(mics, paste0("E:/Triangulation 2023/cormatrices/", TRgroup, "/micsSNR_", TR.group, ".rData"))





###################################################################################################
distance <- function(X_A, Y_A, X_B, Y_B){
  sqrt((X_B - X_A)^2 + (Y_B - Y_A)^2)
  
}

TOATestPoints$timestamp[7] <- "0230405_0950021"
sounds$name[7] <- "0230405_0950021"


TOATestPointss <- TOATestPoints %>% dplyr::rename("name" = "timestamp")
combined <- left_join(localize_df, TOATestPointss, by = "name")


#select columns needed
combined2 <- combined %>% select("east", "north", UTM_X, UTM_Y)
RealError <- apply(combined2, 1 , function(x) distance(x[1], x[2], x[3], x[4]))
combined$RealError <- RealError
combinedy <- combined %>% select(name, err.metres, RealError,)


mean(combinedc$RealError)


saveRDS(combined, "E:/test_points/TestPointsTRA_localization.rData")


TestPoints.. <- readRDS("E:/test_points/TestPointsTRA_localization.rData")
write.csv(TestPoints.., "E:/test_points/TestPointsTRA_localization_final.csv")

mean(combinedy$RealError[1:7])
mean(combinedy$RealError[ rownames(combinedy) %in% c(8, 12:18)])




#sounds[1,4] <- NA
#sounds[1,6] <- NA

#substracting drift from the test points, take first column, should be the same for start offset
drift_mat.df <- as.data.frame(drift_mat)
drift <- drift_mat.df[,1] #drift ordered alphabetically
#so substract drift from TOA's and scale for time

timestamp_name <- c()
sounds <- data.frame()

#View(TOATestPoints)
TOATestPoints[9,11] <- NA #error 114 (116 met de andere weg)


for (i in 1:nrow(TOATestPoints)){

  
  TOA.i <- TOATestPoints[i,]
  
  
  # Get timestamp
  timestamp_name[i]  <- sub("^[^_]*_(.*)\\..*$", "\\1", TOA.i$timestamp)
  timestamp.i <- parse_date_time(timestamp_name[i], orders =c("%Y%m%d_%H%M%S"))
  
  # Assuming linear clock drift, estimate drift that has occurred since the start signal
  time_since_signal <- seconds(timestamp.i)-seconds(start_file_dt)
  
  # Drift since the start signal
  relative_drift <- drift * as.numeric(time_since_signal)/as.numeric(time_passed)
  
  #startfile subtraction (recorders started on different times)
  starttimes <- start_file[,1]
  
  # get TOA's 
  recorders <- colnames(TOA.i)[9:16]
  TOAs.i <- as.numeric(c(TOA.i[,9:16]))
  
  #should be -!
  TOAs.i <- TOAs.i - relative_drift - starttimes 
  TOAs.i <- as.numeric(TOAs.i - min(na.omit(TOAs.i)))
  
  #set NA's to 100
  TOAs.i[is.na(TOAs.i)] <- 100
  
  #set everything 2 & above to NA
  TOAs.i[TOAs.i>=2] <- NA
  
  
  sub_md <- audio_md[audio_md$Timestamp == timestamp_name[i],]
  temp = mean(sub_md$Temperature.Int)
  
  row.i <- c(TOAs.i, temp, timestamp_name[i])
  sounds <- rbind(sounds, row.i)
   
}




#View(sounds)
colnames(sounds) <- c("t1","t2","t3","t4","t5","t6","t7","t8","temp", "name")
sounds <- sounds[,colnames(sounds) %in% c("t1","t2","t3","t4","t5","t6","t7","t8","temp", "name")]

# Add call timestamp
sounds$name <- timestamp_name








########## Run the function ##############

# 'temps' = NULL because we supply temperatures in the 'sounds' data frame
# 'tol' = the tolerance required for matrix inversion via QR decomposition. The default value appears to work well.

##########################################

localize <- function(mics, sounds, temps=NULL, tol=1e-10) {
  ## maybe read mic positions from a file
  if (is.character(mics))
    mics = read.csv(mics, as.is=TRUE)
  
  ## maybe read sound times from a file
  ## and if so, remember the name
  sounds.from.file = is.character(sounds)
  if (sounds.from.file) {
    sounds.filename = sounds
    sounds = read.csv(sounds, as.is=TRUE)
  }
  
  ## the required names of TOADS / timestamps columns, given the number of mics
  mic.cols = paste("t", 1:nrow(mics), sep="")
  
  ## ensure the correct TOADS / timestamps columns are present in sounds
  if (! all ( mic.cols %in% names(sounds)))
    stop("the sounds object must have a column for TOADS or timestamp at each mic,\nand these must be named t1, t2, ... ", tail(mic.cols, 1))
  
  ## ensure the temps are specified for each sound, if no temps parameter is given
  if (is.null(temps) && is.null(sounds$temp))
    stop("you must either specify a number, data.frame, or filename for temps,\nor the sounds object must have a column named 'temp'")
  
  ## maybe read temps from a file
  if (is.character(temps))
    temps = read.csv(temps, as.is=TRUE)
  
  ## get the temperature for each sound
  if (!is.null(sounds$temp)) {
    ## temperatures are specified for each sound
    temps = sounds$temp
  } else if (is.data.frame(temps)) {
    ## temperatures are given as a time series, so apply an interpolation to the mean TOADS / timestamp
    ## for each sound
    temps = spline(temps$time, temps$temp, xout=apply(as.matrix(sounds[, mic.cols]), 1, mean, na.rm=TRUE), method="natural")$y
  } else {
    ## re-use the value in temps for each sound
    temps = rep(temps, length=nrow(sounds))
  }
  
  ## the number of sounds to localize
  n = nrow(sounds)
  
  ## we're doing 3d if "z" is specified for microphone locations
  is.3d = ! is.null(mics$z)
  
  ## names of coordinate columns for microphones
  mic.coords = cbind(mics$east, mics$north, mics$z)
  
  ## add columns to the input dataframe for the estimates
  sounds$east = sounds$north = sounds$v = numeric(n)
  if (is.3d)
    sounds$z = numeric(n)
  sounds$err.millisecs = sounds$err.metres = sounds$err.rms = sounds$time = numeric(n)
  
  ## for improved numerical stability, translate coordinates
  ## so the origin is at their mean; we'll convert back
  ## after doing the estimation
  
  origin = apply(mic.coords, 2, mean)
  
  mic.coords = sweep(mic.coords, 2, origin)
  
  ## keep track of whether we've warned the user about reversed timestamps
  have.warned = FALSE
  
  ## loop over each sound
  for (i in 1:nrow(sounds)) {
    
    ## keep only mics for which the sound was detected
    keep <- ! is.na(sounds[i, mic.cols])
    used.mics = mic.coords[keep,]
    t = as.numeric(sounds[i, mic.cols[keep]])
    
    ## if all signs are non-positive, assume we need to negate
    ## them (we want increasing time to represent "later", as usual)
    
    if (all(t <= 0)) {
      t = -t
      if (! have.warned) {
        
        warning("localizF: times in the sounds matrix should be oriented so that larger
 is further in the future; I'm negating the values you supplied, as they are all non-positive,
which suggests their orientation is reversed")
        
        have.warned = TRUE
      }
    }
    
    ## calculate the speed of sound, given the interpolated
    ## temperature at the time of the first microphone's detection of
    ## this sound
    
    v = 331.3 * sqrt(1 + temps[i] / 273.15)
    
    ## get "mean" time for translating below (we kludge this and always use 64
    ## as the denominator, to match SoundFinder.xls)
    mean.t = sum(t) / 64
    t = t - mean.t
    
    ## invoke the GPS solver
    
    soln = gps(used.mics, t, v, tol)
    
    ## fill in the output row
    
    sounds$east[i]          = soln$east + origin[1]
    sounds$north[i]         = soln$north + origin[2]
    if (is.3d)
      sounds$z[i]           = soln$z + origin[3]
    sounds$time[i]          = soln$time + mean.t
    sounds$v[i]             = v
    sounds$err.rms[i]       = soln$err.rms
    sounds$err.metres[i]    = soln$err.space
    sounds$err.millisecs[i] = soln$err.time * 1000
  }
  
  if (sounds.from.file) {
    ## write solution-augmented input back to the file it came from
    write.csv(sounds, file=sounds.filename, row.names=FALSE)
  }
  
  return (sounds)
}

gps = function(positions, times, v, tol=1e-10)  {
  ## convert the dataframe to a matrix
  ## we use the notation of reference [2]
  ## The "z" column need not be present.
  
  ## either 2 or 3 dimensions
  dim = ncol(positions)
  
  ## lorentz coefficients are 1 for spatial, -1 for time
  lorentz.coef = c(rep(1, dim), -1)
  
  ## lorentz inner product
  lorentz = function(x1, x2=x1) {
    return (sum (lorentz.coef * x1 * x2))
  }
  
  ## the pseudo range
  rho = - v * times
  
  ## add the pseudorange column to get B
  B = cbind(positions, rho)
  
  ## the vector of ones
  e = rep(1, nrow(B))
  
  ## the vector of squared lorentz norms 
  a = 0.5 * apply(B, 1, lorentz)
  
  ## use the pseudo-inverse of B to get B+ e, B+ a
  ## via QR decomposition of B
  
  qrB = qr(B, tol = tol)
  Binv.e = solve(qrB, e, tol = tol)
  Binv.a = solve(qrB, a, tol = tol)
  
  ## the coefficients for the quadratic equation in lambda
  
  cA = lorentz(Binv.e)
  cB = 2 * (lorentz(Binv.e, Binv.a) - 1)
  cC = lorentz(Binv.a)
  
  desc = cB^2-4*cA*cC
  
  ## if the descriminant is negative, we cheat and
  ## set it to zero, so that we still get a solution,
  ## albeit a probably not very good one!
  
  if (desc < 0)
    desc = 0
  
  ## solve the quadratic
  lambda = (-cB + c(-1, 1) * sqrt(desc)) / (2*cA)
  
  ## get the solution for each possible lambda
  
  u1 = solve(qrB, (a + lambda[1] * e), tol = tol)
  u2 = solve(qrB, (a + lambda[2] * e), tol = tol)
  
  ## return the solution with the lower sum of squares
  ## discrepancy
  
  s1 = sum( (B %*% u1 - (a + lambda[1] * e))^2)
  s2 = sum( (B %*% u2 - (a + lambda[2] * e))^2)
  
  u = if (s1 < s2) u1 else u2
  
  err.rms = sqrt(min(s1, s2) / nrow(B))
  
  ## rms space error 
  err.space =  sqrt(mean(apply(B, 1,
                               function(x) {
                                 (sqrt(sum((u[1:dim]-x[1:dim])^2)) + x[1+dim] + u[1+dim])^2
                               })))
  ## rms time error
  err.time = err.space / v
  
  u = as.list(u)
  names(u) = c("east", "north", if(dim == 3) "z", "time")
  
  ## convert distance offset to time offset
  u$time = u$time / v
  
  ## return as a list to allow simpler indexing of components, and append errors
  return(c(u, err.rms=err.rms, err.space=err.space, err.time=err.time))
}

sounds <- sounds[rowSums(is.na(sounds))<7,]



sounds$temp <- as.numeric(sounds$temp)
localize_df <- localize(mics = mics, sounds = sounds, temps = NULL, tol=1e-10)
print(localize_df)

localize_df$name <- sounds$name

LocCor_woBoot <- localize_df  

#localize_df <- localize_df[localize_df$err.metres<10,] #zie mijn word bestand 



saveRDS(localize_df, "C:/Users/Beheerder/Documents/UU/A GSLS/MinPr/R/Audio2023/TestPointsA_localize.rData")
localize_df2 <- readRDS( "C:/Users/Beheerder/Documents/UU/A GSLS/MinPr/R/Audio2023/TestPointsA_localize.rData")

write.csv(localize_df, "C:/Users/Beheerder/Documents/UU/A GSLS/MinPr/R/Audio2023/TestPointsA_localizecsv.csv")

######################################################################################

#bootstrap die shit
localize_df <- localize(mics = mics, sounds = sounds, temps = NULL, tol=1e-10)

#select columns needed
combined2 <- combined %>% select("east", "north", UTM_X, UTM_Y)
RealError <- apply(combined2, 1 , function(x) distance(x[1], x[2], x[3], x[4]))
combined$RealError <- RealError
combined <- left_join(localize_df, TOATestPointss, by = "name")


#####################################################################################







df <- sounds
# Create an empty vector to store the results
name <- c()
Results <- c()
RESULTS <- data.frame()

#set the minimumvalue of the error
minvalue <- 1




# Iterate through each row of the dataframe
for (i in 1:nrow(df)) {
  

  print(paste0("row", i))
  results.j <- c()
  
  # Iterate through different combinations of numerical columns
  #later kan je hier iets bij schrijven dat als er uberhaupt maar 3 getallen zijn 
  #dat localize dan met die 3 getallen wordt uitgevoerd
    
  for (j in 1:5) { #we can set max 5 values to NA because otherwise there won't be enough TOAs left (we need min. 3)
   
     
  print(j)
   results.k <- c()
  
   combinations <- combn(ncol(df) - 2, j) #ncol -2 because the last two columns are not a TOA
    
    #i guess hier een lijn zonder dat er NAs worden toegevoegd en dan eerste resultaat
    #hiermee vergelijken?
    
    # Iterate through each combination of columns
    for (k in 1:ncol(combinations)) {
      
      
      row <- df[i, ]  # Get the current row
      
      
      #in this row specify which numerical columns should be set to NA
      selected_cols <- c(combinations[, k]) 
      
      #set selected columns to NA
      row[selected_cols] <- NA
      
      #only perform function if you have at least 3 columns with TOAs
      if (sum(is.na(row)) <6){
      
      
      
        localize_res <- localize(mics = mics, sounds = row, temps = NULL, tol=1e-10)
        results.k[k] <- localize_res$err.metres
      
      
      
      
        # Check if the previous error is smaller and update if necessary
        if (k >= 2 && results.k[k] > results.k[k-1] && results.k[k-1] > minvalue|
            k>=2 && results.k[k] < minvalue && results.k[k-1] > minvalue ) {
          
          results.k[k] <- results.k[k-1]
          localize_res <- prev_locres
        
        } 
        prev_locres <- localize_res #dit dan ook herhalen voor j... 
      
    } else { #in case we cannot perform localization, we set the error very high
        results.k[k] <- 1e6
       if (k >= 2 && results.k[k] > results.k[k-1]) { #this way, the error of localized combinations will be lower
          
          #set the resultant error in metres to previous error (localize_res & prev_locres
          #stay the same because we didn't create new ones)
          results.k[k] <- results.k[k-1] }
        
        
      }
      print(results.k[k])
   }
    
    results.j[j] <- results.k[k] #set results.j as last results.k (which is automatically the lowest >1)
    
    #we still have localize_res from the previous loop which is the correct result
    
    
    #if the minimum error of this set of n NAs is larger than the previous trial,
    #update the error to the previous lowest error and accompanying localization
    if (j >= 2 && results.j[j] > results.j[j-1] && results.j[j-1] > minvalue) {
      
      results.j[j] <- results.j[j-1] 
      localize_res <- previous_localization
      }
    
    print(results.j[j])
    previous_localization <- localize_res

    
  }  
  
  #for every row, save the localization with lowest error
  RESULTS <- rbind(RESULTS, localize_res)
  

}  


LocCor_wBoot0 <- RESULTS

#saving the results
saveRDS(RESULTS, "C:/Users/Beheerder/Documents/UU/A GSLS/MinPr/R/Audio2023/TestPointsA_bootstrap.rData")
RESULTS <- readRDS( "C:/Users/Beheerder/Documents/UU/A GSLS/MinPr/R/Audio2023/TestPointsA_localize.rData")

write.csv(RESULTS, "C:/Users/Beheerder/Documents/UU/A GSLS/MinPr/R/Audio2023/TestPointsA_bootstrapcsv.csv")

####################################################################################################

#misschien wil je een filter die zegt groter dan 1 voor je error... ik
#weet niet of miniscule errors kloppen.. we'll see
#dus het is niet slechter dan eerst. piecie beter. maar niet heel veel





#piece of code from chatGPT -> aangepast zodat het werkt
############################
# Assuming you have a dataframe called 'df' with 8 numerical columns (col1, col2, ..., col8) and one character column (char_col)

######################################################################################
#calculating real error and comparing it to the error of the function

#calculate mean error between localize points and compare to error of the function
distance <- function(X_A, Y_A, X_B, Y_B){
  sqrt((X_B - X_A)^2 + (Y_B - Y_A)^2)
  
}

RESULTS$name
RESULTS$name[4] <- "20230411_0845021"
localize_df$name[4] <- "20230411_0845021"


TOATestPointss <- TOATestPoints %>% dplyr::rename("name" = "timestamp")



combined <- left_join(RESULTS, TOATestPointss, by = "name")
combined <- left_join(LocCor_wBoot0, TOATestPointss, by = "name")
combined <- left_join(localize_df, TOATestPointss, by = "name")

#select columns needed
combined2 <- combined %>% select("east", "north", UTM_X, UTM_Y)
RealError <- apply(combined2, 1 , function(x) distance(x[1], x[2], x[3], x[4]))
combined$RealError <- RealError
LocCor_wBoot0$RealError <- RealError
mean(RESULTS$RealError)



distance()


localize_df$RealError <- RealError



library(ggplot2)
ggplot(RESULTS, aes(x = err.metres, y = RealError)) +
  geom_point() +             # Add scatter points
  geom_smooth(method = "lm") +  # Add a line of best fit
  geom_abline(intercept = 0, slope = 1) 






######################################################################################

#bootstrap met (batch corr) parameters


#bootstrap die shit
localize_df <- localize(mics = mics, sounds = sounds, temps = NULL, tol=1e-10)

df <- sounds
# Create an empty vector to store the results
name <- c()
Results <- c()
RESULTS <- data.frame()

#set the minimumvalue of the error
minvalue <- 0




# Iterate through each row of the dataframe
for (i in 1:nrow(df)) {
  
  
  print(paste0("row", i))
  results.j <- c()
  
  # Iterate through different combinations of numerical columns
  #later kan je hier iets bij schrijven dat als er uberhaupt maar 3 getallen zijn 
  #dat localize dan met die 3 getallen wordt uitgevoerd
  
  for (j in 1:5) { #we can set max 5 values to NA because otherwise there won't be enough TOAs left (we need min. 3)
    
    
    print(j)
    results.k <- c()
    
    combinations <- combn(ncol(df) - 2, j) #ncol -2 because the last two columns are not a TOA
    
    #i guess hier een lijn zonder dat er NAs worden toegevoegd en dan eerste resultaat
    #hiermee vergelijken?
    
    # Iterate through each combination of columns
    for (k in 1:ncol(combinations)) {
      
      
      row <- df[i, ]  # Get the current row
      
      
      #in this row specify which numerical columns should be set to NA
      selected_cols <- c(combinations[, k]) 
      
      #set selected columns to NA
      row[selected_cols] <- NA
      
      #only perform function if you have at least 3 columns with TOAs
      if (sum(is.na(row)) <6){
        
        
        
        localize_res <- localize(mics = mics, sounds = row, temps = NULL, tol=1e-10)
        
        
        #get UTM_X and UTM_Y
        X_B <- TOATestPoints$UTM_X[TOATestPoints$timestamp == localize_res$name]
        Y_B <- TOATestPoints$UTM_Y[TOATestPoints$timestamp == localize_res$name]
        
        #nu wil je de echter error hebben
        error <- distance(localize_res$east, localize_res$north, X_B, Y_B)
        
        results.k[k] <- error
        
        
        # Check if the previous error is smaller and update if necessary
        if (k >= 2 && results.k[k] > results.k[k-1] && results.k[k-1] > minvalue|
            k>=2 && results.k[k] < minvalue && results.k[k-1] > minvalue ) {
          
          results.k[k] <- results.k[k-1]
          localize_res <- prev_locres
          
        } 
        prev_locres <- localize_res #dit dan ook herhalen voor j... 
        
      } else { #in case we cannot perform localization, we set the error very high
        results.k[k] <- 1e6
        if (k >= 2 && results.k[k] > results.k[k-1]) { #this way, the error of localized combinations will be lower
          
          #set the resultant error in metres to previous error (localize_res & prev_locres
          #stay the same because we didn't create new ones)
          results.k[k] <- results.k[k-1] }
        
        
      }
      print(results.k[k])
    }
    
    results.j[j] <- results.k[k] #set results.j as last results.k (which is automatically the lowest >1)
    
    #we still have localize_res from the previous loop which is the correct result
    
    
    #if the minimum error of this set of n NAs is larger than the previous trial,
    #update the error to the previous lowest error and accompanying localization
    if (j >= 2 && results.j[j] > results.j[j-1] && results.j[j-1] > minvalue) {
      
      results.j[j] <- results.j[j-1] 
      localize_res <- previous_localization
    }
    
    print(results.j[j])
    previous_localization <- localize_res
    
    
  }  
  
  #for every row, save the localization with lowest error
  localize_res$RealError <- results.j[j]
  RESULTS <- rbind(RESULTS, localize_res)
  
  
}  

RESULTS <- as.data.frame(RESULTS)

##########################################################################################################
#Creating a dataframe in R from the results .txt file that was created by running all the files through R
Results_AudioStats<- imp_raven(path=Results_directory,all.data = TRUE, freq.cols = FALSE,
                               warbler.format=F, name.from.file = TRUE,
                               ext.case ="lower")

Results_AudioStats$Timestamp <- str_after_first(Results_AudioStats$`Begin File`, "_")
Results_AudioStats$Timestamp <- str_before_nth(Results_AudioStats$Timestamp, "_", -2)

r0825 <- subset(Results_AudioStats, Timestamp == "20230411_082502")
r0825good <- subset(GoodFile_audiostats, Timestamp == "20230411_082502")
mean(r0825$`SNR NIST Quick (dB)`)

#Summary for all files

Results_summary <- Results_AudioStats %>% group_by(Timestamp) %>%
  summarise_if(is.numeric, mean)

#do this for the files used in triangulation only

#get the names of those files
TOATestPoints2 <- TOATestPoints

#get files that are in the table
names<- c("ALOUATTA", "ATELES", "CHIROPOTES", "PITHECIA", "SAIMIRI", "SMA11381", "SMA11405", "SMA11410")
colnames(TOATestPoints2)[9:16]<- names
not_nas<- which(!is.na(TOATestPoints[,9:16]), arr.ind = TRUE)
where_are_not_nas<- data.frame(rownumber=not_nas[,1], columnname= names[not_nas[,2]])
Filenames_not_nas<- TOATestPoints$timestamp[where_are_not_nas$rownumber]
where_are_not_nas$Filenames<- Filenames_not_nas
Files_to_keep<- paste(where_are_not_nas$columnname, where_are_not_nas$Filenames, sep="_")


Files_to_keep<- paste(Files_to_keep, "_clpd_2k.wav", sep="")





#names <- RESULTS$name[RESULTS$t1 != NA ]


GoodFile_audiostats<- Results_AudioStats[Results_AudioStats$`Begin File` %in% Files_to_keep,]



#Summary for files used for triangulation

Results_tri_summary <- GoodFile_audiostats %>% group_by(Timestamp) %>%
  summarise_if(is.numeric, mean)




BadFile_audiostats <- File_audiostats[! File_audiostats$`Begin File` %in% Files_to_keep,]


min(GoodFile_audiostats$`SNR NIST Quick (dB)`)
min(BadFile_audiostats$`SNR NIST Quick (dB)`)

min(GoodFile_audiostats$`Avg Power Density (dB FS/Hz)`)
min(BadFile_audiostats$`SNR NIST Quick (dB)`)

install.packages("colorDF")
library(colorDF)




File_audiostats$Good <- File_audiostats$`Begin File` %in% Files_to_keep


maybe<- File_audiostats$`Begin File` %in% Files_to_keep
highlight(File_audiostats, maybe)








