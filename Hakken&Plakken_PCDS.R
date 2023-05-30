# This script is based on "Hakken&Plakken", but instead of outputting clips per grid (per timestamp) for 
# triangulation, the output is per grid point for using Point Count Distance Sampling

# Input: 

# - a 'scores.csv' file indicating the probability of howler presence for each recording 
# - the recordings of each grid point, stored in the directory named after the grid point


# Output:

# - clipped, downsampled recordings of howler calls with buffer per grid point
# - a df with the path of the clips, their length and temperature

##################################

# clean environment
rm(list = ls())
gc()

# load libraries
library(readr)
library(lubridate)
library(tidyverse)
library(dplyr)
library(activity)
library(reshape2)
library(ggplot2)
library(scales)
library(seewave)
library(tuneR)
library(audio)
library(R.filesets)

# here we set some basic parameters for the clipping

# value of certainty each sequence should at least have one record of
prob.max <- 0.99

# minimum probability every 10 second record in a sequence should have
prob.min <- 0.80

# amount of seconds to be added in front & after a clip, in multiples of 10
buffer <- 2

############### read in temperature meta data and probability scores file  #######################


# read in the metadata from the original recordings for adding temperature

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

#temp <- audio_md.rData
#temp <- readRDS("C:/Users/Beheerder/Documents/UU/A GSLS/MinPr/R/Audio2023/audio_md.rData")
temp <- readRDS("E:/Triangulation 2023/audio_md_ABCDE.rData")



temp$file_timestamp <- sub("^[^_]*_(.*)\\..*$", "\\1", temp$Original.Filename) # add timestamps
temp <- temp %>% select(file_timestamp, Temperature.Int, WA.Song.Meter.Prefix)


# load scores file with probability of howler per time fragment for each recording
scores <- read_csv("E:/Triangulation 2023/scores_ABCDE.csv")


############# adding variables to scores #######################


#add variables
scores$filename <- sapply(strsplit(scores$file, "/\\s*"), tail, 1) # Removes everything before and including the last slash
scores$datetime <- gsub(".WAV|.wav|.mp3", "", scores$filename) # Remove file extension
scores$datetime <- gsub(".*_([^_]*_[^_]*)$", "\\1", scores$datetime)
scores$datetime <- ymd_hms(scores$datetime)
scores$datetime <- scores$datetime + scores$start_time # Add the seconds
scores$time <- as.POSIXct(hms::as_hms(scores$datetime))
scores$hour <- hour(scores$datetime)
scores$station <- gsub("^(.*?)_.*$", "\\1", scores$filename)
scores$file_timestamp <- str_extract(scores$filename, "(?<=_)(.*?)\\.[^.]*$")
scores$file_timestamp <- gsub(".WAV|.wav|.mp3", "", scores$file_timestamp) # Remove file extension
scores$date <- substr(scores$datetime, 1, 10)
scores$point <- substr(scores$file, 39, 43) 
scores$group <- substr(scores$file, 39, 42)

# getting vectors with the points and groups
points <- (unique(scores$point));points <- points[!is.na(points)]
groups <- (unique(scores$group));groups <- groups[!is.na(groups)]

# Convert the detection probabilities to presence/absence
scores$presence <- scores$Alouatta_macconnelli>=0.99

# throw away everything after 07:00
scores <- scores[scores$hour < 7, ]

# we want to add seconds to our timestamps, e.g. for when we need to save new audiofiles
scores$time2 <- format(scores$datetime, "%H%M%S") # vector with new time part
scores$file_timestampsec <- c() # creating new timestamp vector including seconds

for (i in seq_along(scores$time2)) { # adding seconds for each timestamp
  scores$file_timestampsec[i] <- gsub("_\\d+", paste0("_", scores$time2[i]), scores$file_timestamp[i])
}

# we want the total amount of seconds since hour 1 for each grid point

# this is for determining whether howler calls are on multiple recordings (see the
# loop at the end of the script where we load audiofiles and bind them together if
# howler calls are spread across multiple recorders)

scores <- scores %>%
  dplyr::group_by(point) %>% arrange(file_timestampsec) %>%
  dplyr::mutate(totalsec = seq(from = 0, by = 10, length.out = length(point))) %>%
  ungroup()


saveRDS(scores, "E:/Triangulation 2023/scores_allpoints.rData")


##################  selecting start and end points of calls ##################

scores <- readRDS(scores,  "E:/Triangulation 2023/scores_allpoints.rData")

#for each grid point

for (i in 1:length(points)){
  
  #get grid point
  point.i <- points[i]
  
  #get scores of grid point
  scores.i <- subset(scores, point == point.i)
  
  # create dataframe with definite howler calls
  Howlers99 <- subset(scores.i, presence == T)
  
  # Get timestamps of those calls
  timestamps <- unique(Howlers99$file_timestamp)
  
  # Get all files with those timestamps
  file_df <- scores[scores$file_timestamp %in% timestamps, ]
  
  # convert filenames from python to R syntax
  file_df$filenames <- gsub("/mnt/g/", "D:/", file_df$file)
  
  
  #######################################################################################
  
  # for each timestamp, we want a df with a column for filetimestamp_sec values and
  # a column with the probabilities from scores$Alouatta_macconnelli
  
  # We'll do this by conditionally transforming the df, the end result is a df with for
  # each timestamp sequences of howler calls marked with TRUE and the time fragments in between
  # the sequences marked with FALSE. This is achieved by:
  
  # 1. Selecting sequences for which all fragments of a recording have a howler probability
  #   of >0.80 and for which at least one fragment has a probability of >=0.99
  
  # 2. Adding 20 seconds before and after a sequence to make sure the beginning and end of the
  # call is on the recording
  
  ############################################################################################
  
  # vector with the unique timestamps
  timestamps <- unique(file_df$file_timestamp)
  
  # vector for saving start & end times
  StartEnd <- c()
  
  # loop with which we'll add the start & end times to the vector
  for (i in 1:length(timestamps)) { # loop over the timestamps
    
    
    timestamp.i <- timestamps[i]
    print(timestamp.i)
    print(i)
    
    # create a df with only the selected timestamp
    scores.stamp <- subset(file_df, file_timestamp == timestamp.i)
    
    
    # Reshape the dataframe to wide format and change it to a matrix
    df_wide <- pivot_wider(scores.stamp, id_cols = point, names_from = file_timestampsec, values_from = Alouatta_macconnelli)
    df_wide2 <- df_wide[, -1]
    rownames(df_wide2) <- df_wide$point
    mat <- as.matrix(df_wide2)
    
    
    # If a sequence of values >0.8 contains at least one >0.99 value, set all values to 2
    
      # Loop through the columns
      for (j in 1:ncol(mat)) {
        if (mat[i, j] >= prob.max && mat[i, j] < 2) { # if the element is >= 0.99 and <2
          
          # Loop through the left elements
          for (k in j:1) {
            if (mat[i, k] < prob.min) { # If the element is <0.80, stop looking to the left
              break
            }
            if (mat[i, k] >= prob.min) { # If the element is >0.80, change it to 2
              mat[i, k] <- 2
            }
          }
          
          # Also Loop through elements to the right
          for (k in (j + 1):ncol(mat)) {
            if (k <= ncol(mat) && mat[i, k] < prob.min) { # If the element is <0.80, stop looking to the right
              break
            }
            if (k <= ncol(mat) && mat[i, k] >= prob.min) { # If the element is >0.80, change it to 2
              mat[i, k] <- 2
            }
          }
        }
      }
    
    
    
    
    # adding 20 seconds before and 20 seconds after a call
    
      # loop over the columns
      for (j in 1:ncol(mat)) {
        if (mat[i, j] == 2) { # if the element is 2
          
          # select the two left columns (elements)
          for (k in (j - 1):(j - buffer)) {
            if (k >= 1 && mat[i, k] < 2) { # if the element to the left is <2 (and the column exists)
              
              mat[i, k] <- 1.5
            } # change the elements to the left to 1.5
          }
          
          
          # select the two right right columns
          for (k in (j + 1):(j + buffer)) {
            if (k <= ncol(mat) && mat[i, k] < 2) { # if the element to the right is <2 (and the column exists)
              
              mat[i, k] <- 1.5 # change the element to 1.5
            }
          }
        }
      }
    
    
    # set howler seqs & their 20 sec buffers to TRUE
    mat_TF <- mat > 1.4 #>1.4 since our 20 sec buffer is 1.5 and howler containing seqs are 2
    
    # we want to select starting & end points for sequences with howler calls
    
    # We do this by looking which ten second periods have no registered howler call
    # and taking the first point before and after these empty sequences as respectively
    # end and starting points. 
    
    # Turn TRUEs to 1 and FALSEs to 0 
    Rows_F <- colSums(mat_TF)
    
    # defining the rows to split
    
    # find the positions of the zero values
    Rows_F_index <- which(colSums(mat_TF) == 0) # position of columns with no howlers
    
    # extract the names of the first value if it is not a zero
    first_name <- ifelse(Rows_F[1] > 0, names(Rows_F[1]), NA)
    
    # extract the names of the values to the right of each zero
    names_to_right <- lapply(Rows_F_index, function(x) {
      if (x + 1 <= length(Rows_F) && Rows_F[(x + 1)] != 0) { # if there is a right element and it is not 0
        names(Rows_F[x + 1]) # save the name of the element
      }
    })
    
    # unlist the result to get a vector of names
    names_to_right <- unlist(names_to_right)
    
    
    # extract the names of the values to the left of each zero
    names_to_left <- lapply(Rows_F_index, function(x) {
      if (x - 1 >= 1 && Rows_F[(x - 1)] != 0) { # if there is a left element and it is not 0
        names(Rows_F[x - 1]) # save the name of the element
      }
    })
    
    names_to_left <- unlist(names_to_left)
    
    # extract the name of the last value if it is not a zero
    last_nonzero_name <- ifelse(Rows_F[length(Rows_F)] > 0, names(Rows_F[length(Rows_F)]), NA)
    
    # combine these names
    SE <- unique(na.omit(c(first_name, names_to_left, names_to_right, last_nonzero_name)))
    
    # save the ten second fragments that are start & end points of a call
    StartEnd <- c(StartEnd, SE)
  }
  
  
  # select the start & end timestamps from the original df
  StartEnd_df <- subset(file_df, file_timestampsec %in% StartEnd)
  
  # by subsetting from the original df, we keep all the information of the clips and we
  # also make sure that timestamps are in chronological order, which is important for clipping & merging!
  
  # select the columns needed
  StartEnd_df <- StartEnd_df %>%
    select(file_timestampsec, totalsec, file_timestamp, station, start_time, end_time)
  
  # check if every start & end point is in the df
  length(unique(StartEnd_df$file_timestampsec)) == length(StartEnd)
  
  # add "start" for the start of a call & "end" for the end
  StartEnd_df$SE <- rep(c("start", "end"), times = length(StartEnd) / 2)
  
  # add sequence id's for the start & end points
  StartEnd_df$seq <- rep(1:(length(StartEnd) / 2), each = 2)
  
  
  ###################### clipping and merging of clips ########################################

  #set station name and grid point name for in the loop
  station.j = unique(StartEnd_df$station)
  TR <- point.i 
 
  # get df with temperatures of each timestamp for this station
  temp.j <- subset(temp, WA.Song.Meter.Prefix == station.j)
  
  #vector with unique sequences
  seqs <- unique(StartEnd_df$seq)
  
  # vector for in the loop
  Timestamps <- c()
  PATH <- c()
  LENGTH <- c()
  TEMP <- c()
  
  
  # loop clipping every file for this grid point
    for (i in 1:length(seqs)) {
      
      
      gc() #garbage collection, forces R to return free memory immediately before each iteration
    
      # sequence ids
      seq.i <- as.numeric(seqs[i]) # the sequence id
      seq.prev <- as.numeric(seqs[i - 1]) # previous sequence id
      seq.next <- as.numeric(seqs[i + 1]) # next sequence id
      
      # totalsec ids
      sub <- subset(StartEnd_df, seq == seq.i)
      secs_start.i <- StartEnd_df$totalsec[StartEnd_df$seq == seq.i][1] # totalsecs at the start of the clip
      secs_end.i <- StartEnd_df$totalsec[StartEnd_df$seq == seq.i][2] # totalsecs at the end of the clip
      
      secs_end.prev <- StartEnd_df$totalsec[StartEnd_df$seq == seq.prev][2] # totalsec at the end of the previous clip
      
      
      secs_start.next <- StartEnd_df$totalsec[StartEnd_df$seq == seq.next][1] # totalsec at the start of the next clip
      
      
      # timestamp of the clip wo seconds
      filetimestamp.i <- StartEnd_df$file_timestamp[StartEnd_df$seq == seq.i][1]
      
      # corresponding temperature
      temperature <- temp.j$Temperature.Int[temp.j$file_timestamp == filetimestamp.i]
      
      # timestamp of our clip with seconds
      timestamp.i <- StartEnd_df$file_timestampsec[StartEnd_df$seq == seq.i][1] # 1 because we want to name it after the starting point
      
      print(timestamp.i)
      
      
      # get the path of the audiofile 
      path <- paste0("E:/Triangulation 2023/Deployments/", TR, "/", station.j, "_", filetimestamp.i, ".wav")
     
      if (file.exists(path)) {
        
        # Set the start and end times in seconds
        start.time <- StartEnd_df$start_time[StartEnd_df$seq == seq.i][1]
        end.time <- StartEnd_df$end_time[StartEnd_df$seq == seq.i][2]
        length <- end.time - start.time
        
        # get the wav file
        wav_file <- readWave(path, from = start.time, to = end.time, units = "seconds")
        
        
        # if this clip is at the beginning of an audiofile and the previous clip at the end, merge the clips
        if (i > 1 && secs_start.i - secs_end.prev <= 10) { #difference between seconds will be 10 
         
          # merge audio files
          wav_file <- bind(previous_clip, wav_file)
          length <- previous_length + length
          
          # select timestamp of the first file, for saving the clip
          timestamp.i <- Timestamps[1]
          
          # if the previous clip does not differ <= 10 sec set Timestamps NULL
        } else {
          Timestamps <- c()
        }
        
        # if the next clip is at the start of an audiofile and this clip at the end, do not save this clip
        if (i + 1 <= length(seqs) && secs_start.next - secs_end.i <= 10) {
          
          Timestamps <- c(Timestamps, timestamp.i) #add the timestamp of this clip to a vector, used in previous loop
          previous_clip <- wav_file #temporarily store clip as previous file, for merging with next clip
          previous_length <- length #store length of this clip, will be added to next clip's length
        
          } else {
          
          # create new directory for the clipped files, with timestamp including seconds
          dir <- paste0("E:/Triangulation 2023/", TR, "_clipped_fullwv") # dir if everything is stored in one file
          if (!dir.exists(dir)) dir.create(dir, recursive = T)
          
          #get path of the to be saved audio file
          path2 <- paste0(dir, "/", station.j, "_", timestamp.i, ".wav")
          
          # vectors for df with paths, corresponding durations & temperatures
          PATH <- c(PATH, path2)
          LENGTH <- c(LENGTH, length)
          TEMP <- c(TEMP, temperature)
          
          print(path2)
          
          # Write Wave objects to file
          if (!exists(path2)) {
            writeWave(wav_file, filename = path2)
          }
          
          # downsample the file and store as downsampled
          dir <- gsub("clipped_fullwv", "clipped_ds", dir)
          
          if (!dir.exists(dir)) dir.create(dir, recursive = T)
          
          output_name <- path2
          output_name <- gsub(".wav", "_2k.wav", output_name)
          output_name <- gsub("clipped_fullwv", "clipped_ds", output_name)
          
          comnd <- paste0("sox \"", path2, "\" \"", output_name, "\" rate 2k")
          
          out <- system(
            command = comnd, ignore.stderr = TRUE,
            intern = TRUE
          )
        }
      }
    }
  
  
  
  
  # dataframe with path name of the clips, duration (s) and temperature (dg)
  df_durations <- as.data.frame(cbind(PATH, LENGTH, TEMP))
  
  
  saveRDS(df_durations, paste0("E:/Triangulation 2023/temp_lengths", TR, ".rData"))
  write_csv(df_durations, paste0("E:/Triangulation 2023/temp_lengths", TR, ".csv"))

}  
  

