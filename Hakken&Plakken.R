# This script takes predictions of howler roars in the recordings
# and then groups the recordings from different recorders taken at
# the same time and places them in separate directories.

# Raven needs this for the batch correlation function.

# Input: a 'scores.csv' file indicating the probability of howler presence for each recording

# Output: the recordings grouped neatly per timestamp in directories

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

##################################

# here we set some basic parameters for the clipping

# value of certainty each sequence should at least have one record of
prob.max <- 0.99

# minimum probability every 10 second record in a sequence should have
prob.min <- 0.80

# amount of seconds to be added in front & after a clip, in multiples of 10
buffer <- 2

##################################


# read in the metadata from the original recordings for adding temperature

# temp <- readRDS("C:/Users/Beheerder/Documents/UU/A GSLS/MinPr/R/Audio2023/audio_md.rData")
temp <- readRDS("E:/Triangulation 2023/audio_md_ABCDE.rData")

temp$file_timestamp <- sub("^[^_]*_(.*)\\..*$", "\\1", temp$Original.Filename) # add timestamps
temp <- temp %>% select(file_timestamp, Temperature.Int, WA.Song.Meter.Prefix)


# load scores file with probability of howler per time fragment for each recording

scores <- read_csv("E:/Triangulation 2023/scores_CDE.csv")
# scores <- read_csv("C:/Users/Beheerder/Documents/UU/A GSLS/MinPr/R/Audio2023/scoresTRA.csv")
# scores <- read_csv("C:/Users/Beheerder/Documents/UU/A GSLS/MinPr/R/Audio2023/scores0205.csv")
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
scores$point <- substr(scores$file, 39, 43) # als je internet hebt naar gsub omschrijven
scores$group <- substr(scores$file, 39, 42)

# adding points and groups to each score
points <- (unique(scores$point))
points <- points[!is.na(points)]
groups <- (unique(scores$group))
groups <- groups[!is.na(groups)]

# for now only clipping for group A
scores <- subset(scores, group == "TR-E")


# get a vector with all stations
stations <- (unique(scores$station))
stations <- stations[!is.na(stations)]

# correct stations for TR-B
# stations <- c("SAIMIRI",    "ATELES" ,    "CHIROPOTES", "SAGUINUS" , "PITHECIA",   "SAPAJUS",
#              "ALOUATTA",   "CEBUS"     )

scores <- subset(scores, station %in% stations)

# Convert the detection probabilities to presence/absence
scores$presence <- scores$Alouatta_macconnelli >= 0.99

# throw away everything after 07:00
scores <- scores[scores$hour < 7, ]


# we want to add seconds to our timestamps, e.g. for when we need to save new audiofiles
scores$time2 <- format(scores$datetime, "%H%M%S") # vector with new time part
scores$file_timestampsec <- c() # creating new timestamp vector including seconds

for (i in seq_along(scores$time2)) { # adding seconds for each timestamp
  scores$file_timestampsec[i] <- gsub("_\\d+", paste0("_", scores$time2[i]), scores$file_timestamp[i])
}


# we want the total amount of seconds since hour 1 for each station

# this is for determining whether howler calls are on multiple recordings (see the
# loop at the end of the script where we load audiofiles and bind them together if
# howler calls are spread across multiple recorders)

scores <- scores %>%
  dplyr::group_by(station) %>%
  arrange(file_timestampsec) %>%
  dplyr::mutate(totalsec = seq(from = 0, by = 10, length.out = length(station))) %>%
  ungroup()







############################################################################################################

# some recorders start later than others with recording on the first day, this causes a mismatch in the
# totalsec value for the same timestamp_sec. We'll fix this by comparing all stations to the earliest station
# and adding the seconds they missed (e.g. if station a starts at 01:00:00 and b at 01:00:40 we'll add
# 40 seconds to station b)

# timestamps differ in names (010000 vs 010002) and some start later (011000), so to be safe
# we look at the time differences at the 600th timestamp_sec (so 20th 5min rec) which should have
# the same name for each recorder

###########################################################################################################

# get the 600th timestamp of the first station
timestampsec.s <- unique(scores$file_timestampsec)[600]

# check if every recorder was on during that timestamp
check <- scores[scores$file_timestampsec %in% timestampsec.s, ]
print("Were all recorders present in 'check' and were the totalsecs reasonably close?")





# see which rec has the highest totalsec for this timestamp

x <- scores %>% filter(file_timestampsec == timestampsec.s) # df with all totalsecs for the 600th timestampsec
y <- x %>% slice_max(totalsec) # df with the max value(s)
totalsec.s <- y$totalsec[1] # the max value, just use the first row, as all totalsecs are the max value


# for each station, add the seconds they lag
for (i in 1:length(stations)) {
  station.i <- stations[i]

  # calculate the time a stations lags
  dif <- totalsec.s - x$totalsec[x$station == station.i] # seconds to be added

  # loop through all totalsec rows and change only the rows of the current station.i
  scores$totalsec <- ifelse(scores$station == station.i, scores$totalsec + dif, scores$totalsec)
}

# check if every recorder has the same totalsec for an identical timestamp
check <- scores[scores$file_timestampsec %in% timestampsec.s, ] # 600th timestamp for convenience
print("Do all recorders have the same value for totalsec?")


saveRDS(scores, "E:/Triangulation 2023/scoresTRE.rData")

################################################################################################
################################################################################################


scoresB <- readRDS("E:/Triangulation 2023/scoresTRB.rData")
scoresD <- readRDS("E:/Triangulation 2023/scoresTRD.rData")
scoresE <- readRDS("E:/Triangulation 2023/scoresTRE.rData")

scores2 <- rbind(scoresC, scoresD, scoresE)
scores2 <- scoresB

# value of certainty each sequence should at least have one record of
prob.max <- 0.99

# minimum probability every 10 second record in a sequence should have
prob.min <- 0.80

# amount of seconds to be added in front & after a clip, in multiples of 10
buffer <- 2

groups <- (unique(scores2$group))
groups <- groups[!is.na(groups)]



for (i in 1:length(groups)) {
  i <- 1
  # select the TR group
  group.i <- groups[i]

  # subset scores for this group
  scores <- subset(scores2, group == group.i)

  # get the stations
  stations <- (unique(scores$station))
  stations <- stations[!is.na(stations)]


  # Convert the detection probabilities to presence/absence
  scores$presence <- scores$Alouatta_macconnelli >= 0.99

  # create dataframe with definite howler calls
  Howlers99 <- subset(scores, presence == T)

  # Get timestamps of those calls
  timestamps <- unique(Howlers99$file_timestamp)

  # Get all files with those timestamps
  file_df <- scores[scores$file_timestamp %in% timestamps, ]

  # convert filenames from python to R syntax
  file_df$filenames <- gsub("/mnt/g/", "D:/", file_df$file)


  #######################################################################################

  # for each timestamp we want a matrix with as column names filetimestamp_sec values and
  # as rownames the recorders, with the probabilities from scores$Alouatta_macconnelli

  # We'll do this by conditionally transforming the matrix, the end result is a matrix with for
  # each recorder sequences of howler calls marked with TRUE and the time fragments in between
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
    df_wide <- pivot_wider(scores.stamp, id_cols = station, names_from = file_timestampsec, values_from = Alouatta_macconnelli)
    df_wide2 <- df_wide[, -1]
    rownames(df_wide2) <- df_wide$station
    mat <- as.matrix(df_wide2)


    # If a sequence of values >0.8 contains at least one >0.99 value, set all values to 2

    # For each row of the matrix
    for (i in 1:nrow(mat)) {
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
    }



    # adding 20 seconds before and 20 seconds after a call

    # for each row
    for (i in 1:nrow(mat)) {
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
    }

    # set howler seqs & their 20 sec buffers to TRUE
    mat_TF <- mat > 1.4 #>1.4 since our 20 sec buffer is 1.5 and howler containing seqs are 2

    # we want to select starting & end points for sequences with howler calls

    # We do this by looking which ten second periods have no Howlers on all recorders
    # and taking the first point before and after these empty sequences as respectively
    # end and starting points. This way, we select for the starting point the earliest onset
    # of the howler call (so based on the recorder that receives the call first) and for the
    # end point we select the latest point in time that the call is registered (so based on
    # the recorder that registers the call longest)

    # create a named vector with the amount of recorders with howlers for each timestampsec
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


  # by subsetting from the original df, we also make sure that timestamps are in
  # chronological order, wwhich is important for clipping & merging!

  # select the columns needed
  StartEnd_df <- StartEnd_df %>%
    select(file_timestampsec, totalsec, file_timestamp, station, start_time, end_time)


  # check if totalsec matches for all recorders
  length(unique(StartEnd_df$file_timestampsec)) == length(unique(StartEnd_df$totalsec))

  # check if every start & end point is in the df
  length(unique(StartEnd_df$file_timestampsec)) == length(StartEnd)


  # You'll notice that the first check returns FALSE when 1 recorder was not recording
  # during a fragment with a howler in it. This can be because it started 10 minutes
  # later and did not register the first howler call. When a recorder starts later,
  # typically the value for 'totalsec' is not empty during the fragments it did not
  # record, but is extremely high (because the seconds are added after the last timestamp
  # it did record. E.g. timestamps + secs: (2 - 10, 3 - 20, 4 - 30, 5 - 40, 1- 50)

  # you can check this by viewing the unique timestamps and checking if the last
  # timestamps are still chronologically ordered
  (timestamps.u <- unique(StartEnd_df$file_timestampsec))

  # subset dataframe for these timestamps to see if they gave the error
  last.timestamp <- timestamps.u[length(timestamps.u)]
  check <- subset(StartEnd_df, file_timestampsec == last.timestamp)

  # probably the recorders with errors have totalsecs that are too large, so we take the
  # minimum totalsecs for each timestamp from StartEnd_df
  StartEnd_df <- StartEnd_df %>%
    dplyr::group_by(file_timestampsec) %>%
    dplyr::summarize(file_timestamp = file_timestamp, start_time = start_time, end_time = end_time, totalsec = (min(totalsec)))

  # if all went well we should have 8 duplicates of each timestamp (8 recorders)
  # so we take unique rows
  StartEnd_df <- unique(StartEnd_df)

  # check if totalsec matches for all recorders (to be sure)
  length(unique(StartEnd_df$file_timestampsec)) == length(unique(StartEnd_df$totalsec))


  # add "start" for the start of a call & "end" for the end
  StartEnd_df$SE <- rep(c("start", "end"), times = length(StartEnd) / 2)

  # add sequence id's for the start & end points
  StartEnd_df$seq <- rep(1:(length(StartEnd) / 2), each = 2)
  seqs <- unique(StartEnd_df$seq)





  #############################################################################################

  # vector for in the loop
  Timestamps <- c()
  PATH <- c()
  LENGTH <- c()
  TEMP <- c()


  # loop clipping every file for every recorder
  for (j in 1:length(stations)) {
    station.j <- stations[j]
    print(j)
    print(station.j)

    # get grid point (e.g. TR-A1)
    TR <- file_df$point[file_df$station == station.j][1]

    # get df with temperatures of each timestamp for this station
    temp.j <- subset(temp, WA.Song.Meter.Prefix == station.j)


    for (i in 1:length(seqs)) {
      gc()
      print(i)


      # sequence ids
      seq.i <- as.numeric(seqs[i]) # the sequence id
      seq.prev <- as.numeric(seqs[i - 1]) # previous sequence id
      seq.next <- as.numeric(seqs[i + 1]) # next sequence id

      # totalsec ids
      sub <- subset(StartEnd_df, seq == seq.i)
      secs_start.i <- StartEnd_df$totalsec[StartEnd_df$seq == seq.i][1] # totalsecs at the start of the clip
      secs_end.i <- StartEnd_df$totalsec[StartEnd_df$seq == seq.i][2] # totalsecs at the end of the clip

      secs_end.prev <- StartEnd_df$totalsec[StartEnd_df$seq == seq.prev][2] # totalsec at the end of the previous clip


      secs_start.next <- StartEnd_df$totalsec[StartEnd_df$seq == seq.next][1] # totalsec of at the start of the next clip


      # timestamp of the clip wo seconds
      filetimestamp.i <- StartEnd_df$file_timestamp[StartEnd_df$seq == seq.i][1]

      # corresponding temperature
      temperature <- temp.j$Temperature.Int[temp.j$file_timestamp == filetimestamp.i]

      # timestamp of our clip with seconds
      timestamp.i <- StartEnd_df$file_timestampsec[StartEnd_df$seq == seq.i][1] # 1 because we want to name it after the starting point

      print(timestamp.i)


      # Read the .WAV file into R
      path <- paste0("E:/Triangulation 2023/Deployments/", TR, "/", station.j, "_", filetimestamp.i, ".wav")
      

      if (file.exists(path)) {
        # Set the start and end times in seconds
        start.time <- StartEnd_df$start_time[StartEnd_df$seq == seq.i][1]
        end.time <- StartEnd_df$end_time[StartEnd_df$seq == seq.i][2]
        length <- end.time - start.time

        wav_file <- readWave(path, from = start.time, to = end.time, units = "seconds")


        # If the previous clip was at the end of an audiofile and this one at the beginning, merge the files

        if (i > 1 && secs_start.i - secs_end.prev <= 10) {
          # merge audio files
          wav_file <- bind(previous_clip, wav_file)
          length <- previous_length + length

          # select timestamp of the first file (see the vector in the next step)
          timestamp.i <- Timestamps[1]

          # If the previous clip does not differ <= 10 sec do not merge and set Timestamps -> NULL
          } else {
          Timestamps <- c()
        }

        # if this clip is at the end of a recording and the next one at the beginning, do not save this clip and merge later on
        if (i + 1 <= length(seqs) && secs_start.next - secs_end.i <= 10) {
          Timestamps <- c(Timestamps, timestamp.i) # add this timestamp to a vector to select it for the file name if it is the first timestamp
          previous_clip <- wav_file # change te name of the clip so it can be merged upon the next iteration
          previous_length <- length # change name of length so it can be added to the next length
        
          
          #if that is not the case, save the clip
          } else { 
          # create new directory for the clipped files, with timestamp including seconds
          TR.group <- substr(TR, 1, 4)
          dir <- paste0("E:/Triangulation 2023/", TR.group, "_clipped_fullwv") # dir if everything is stored in one directory
          # dir <- paste0("D:/Triangulation 2023/", TR.group, "_clipped/subset0405", timestamp.i) #dir if clips are stored per timestamp
           if (!dir.exists(dir)) dir.create(dir, recursive = T)

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

          # downsample file and store as downsampled
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
  }



  # dataframe with path name of the clips, duration (s) and temperature (dg)
  df_durations2 <- as.data.frame(cbind(PATH, LENGTH, TEMP))

  # saveRDS(df_durations2, "D:/Triangulation 2023/lengthsTRA.rData")
  saveRDS(df_durations2, paste0("E:/Triangulation 2023/lengths", TR.group, ".rData"))
  write_csv(df_durations2, paste0("E:/Triangulation 2023/lengths", TR.group, ".csv"))
}



################################################################################################

# for only the df (path names, durations & temperatures) withouth actually clipping files:

# vector for in the loop
Timestamps <- c()
PATH <- c()
LENGTH <- c()
TEMP <- c()

# repeat previous loop but without loading, clipping & saving files
for (j in 1:length(stations)) {
  station.j <- stations[j]
  TR <- file_df$point[file_df$station == station.j][1]
  temp.j <- subset(temp, WA.Song.Meter.Prefix == station.j)

  for (i in 1:length(seqs)) {
    # sequence ids
    seq.i <- as.numeric(seqs[i]) # the sequence id
    seq.prev <- as.numeric(seqs[i - 1]) # previous sequence id
    seq.next <- as.numeric(seqs[i + 1]) # next sequence id

    # totalsec ids
    secs_start.i <- StartEnd_df$totalsec[StartEnd_df$seq == seq.i][1] # totalsecs at the start of the clip
    secs_end.i <- StartEnd_df$totalsec[StartEnd_df$seq == seq.i][2] # totalsecs at the end of the clip
    secs_end.prev <- StartEnd_df$totalsec[StartEnd_df$seq == seq.prev][2] # totalsec at the end of the previous clip
    secs_start.next <- StartEnd_df$totalsec[StartEnd_df$seq == seq.next][1] # totalsec of at the start of the next clip

    # timestamp of the clip wo seconds
    filetimestamp.i <- StartEnd_df$file_timestamp[StartEnd_df$seq == seq.i][1]

    # corresponding temperature
    temperature <- temp.j$Temperature.Int[temp.j$file_timestamp == filetimestamp.i]

    # timestamp of our clip with seconds
    timestamp.i <- StartEnd_df$file_timestampsec[StartEnd_df$seq == seq.i][1] # 1 because we want to name it after the starting point

    # path of old file
    path <- paste0("E:/Triangulation 2023/Deployments/", TR, "/", station.j, "_", filetimestamp.i, ".wav")

    if (file.exists(path)) { # if there is indeed a file

      # Set the start and end times in seconds
      start.time <- StartEnd_df$start_time[StartEnd_df$seq == seq.i][1]
      end.time <- StartEnd_df$end_time[StartEnd_df$seq == seq.i][2]
      length <- end.time - start.time

     
      if (i > 1 && secs_start.i - secs_end.prev <= 10) {
        length <- previous_length + length
        timestamp.i <- Timestamps[1]

      } else {
        Timestamps <- c()
      }

     if (i + 1 <= length(seqs) && secs_start.next - secs_end.i <= 10) {
        Timestamps <- c(Timestamps, timestamp.i)
        previous_length <- length
      } else {

        print("else")
        # create new directory for the clipped files, with timestamp including seconds
        TR.group <- substr(TR, 1, 4)
        dir <- paste0("E:/Triangulation 2023/", TR.group, "_clipped_ds")

        path2 <- paste0(dir, "/", station.j, "_", timestamp.i, ".wav")

        PATH <- c(PATH, path2)
        LENGTH <- c(LENGTH, length)
        TEMP <- c(TEMP, temperature)
      }
    }
  }
}


# dataframe with names paths of the clips and duration in seconds
df_durations2 <- as.data.frame(cbind(PATH, LENGTH, TEMP))

# save dataframe
saveRDS(df_durations2, paste0("E:/Triangulation 2023/templengths", TR.group, ".rData"))
write_csv(df_durations2, paste0("E:/Triangulation 2023/templengths", TR.group, ".csv"))





