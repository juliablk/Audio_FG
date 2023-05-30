# This script is based on acoustic triangulation (a script using the function 'localize'
# to calculate GPS coordinates by triangulation), but takes a csv with
# single row time of arrivals as input instead of matrices for each timestamp.  


# Additionally, this script

# 1. Uses bootstrapping to find the estimated gps coordinates with lowest error in metres
#    (parameter of the localization function) 
# 2. Calculates the real error of the estimated GPS points by comparing them to the known field locations
# 3. Plots the relationship between the error provided by the localize function and the real error,
#    to see if we can use the error provided by the localize function to improve the accuracy of our
#    estimation of howler localizations


# Input needed for the localize function:

# The localize function used for calculating GPS points by triangulation takes as input
# a 'mics' df with information concerning the recorders (X,Y and Z coordinates) and a 'sounds' df.
# 'sounds' =  a data frame with columns 't1', 't2', ..., 'tn', indicating the relative time of arrival (TOA)
# of a signal between recorders (same order as 'mics'). Missing values are allowed. There is also a column
# 'Temp' which should yield the temperature registered by the recorder when the sound was detected.


##########################################################################################################

#remove list and load relevant libraries
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


#set wd and load TestPoints df
wd <- "E:/test_points/Grouped/_clipped_ds"

TOATestPoints <- read.csv("E:/test_points/test points2.csv", sep=";")


############### make 'mics' #########################

#get metadata
ARU_metadata <- read.csv("C:/Users/Beheerder/Documents/UU/A GSLS/MinPr/R/Audio2023/deployment_tr.csv", sep = ";")

#get metadata of the A grid (testpoint grid)
TR.group = "TR-A"
TRgroup = "TRA"

ARU_metadata <- read.csv("C:/Users/Beheerder/Documents/UU/A GSLS/MinPr/R/Audio2023/deployment_tr.csv", sep = ";")
mics <- ARU_metadata[,colnames(ARU_metadata) %in% c("UTMX.AVG","UTMY.AVG","elevation","deployment_id","sensor_id")]

#select mics from the current grid
mics <- mics[grepl(TR.group, mics$deployment_id),]

#remove audiomods
Audiomods <- c("A23", "A21", "A02", "A27", "A200", "A26", "A18", "A13")
mics <- mics[!mics$sensor_id %in% Audiomods,]

#set colnames for localize function (later in script)
colnames(mics) <- c("deployment_ID","sensor_id","z","east","north")
mics <- with (mics, mics [order (sensor_id) , ])

# sensors were installed at 2 m height 
mics$z <- mics$z+2



############### Make 'sounds' ##############


# Import recording metadata of recordings (mostly for temperature)
audio_md <- readRDS("E:/Triangulation 2023/audio_md_ABCDE.rData")
audio_md$Timestamp <- sub("^[^_]*_(.*)\\..*$", "\\1", audio_md$Original.Filename)


# First estimate clock drift for each recorder 

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

#for substracting drift from the test points, we take the first column, should be the same for start offset
drift <- drift_mat.df[,1] 

#for substracting start times (recorders started on different times)
starttimes <- start_file[,1] 

#vectors for in the loop
timestamp_name <- c()
sounds <- data.frame()


#for each test point, get the relative time of arrivals

for (i in 1:nrow(TOATestPoints)){
  
  TOA.i <- TOATestPoints[i,] #test points are ordered by row
  
  # Get timestamp
  timestamp_name[i]  <- sub("^[^_]*_(.*)\\..*$", "\\1", TOA.i$timestamp)
  timestamp.i <- parse_date_time(timestamp_name[i], orders =c("%Y%m%d_%H%M%S"))
  
  # Assuming linear clock drift, estimate drift that has occurred since the start signal
  time_since_signal <- seconds(timestamp.i)-seconds(start_file_dt)
  
  # Drift since the start signal
  relative_drift <- drift * as.numeric(time_since_signal)/as.numeric(time_passed)
  
  # get TOAs of the timestamp 
  recorders <- colnames(TOA.i)[9:16]
  TOAs.i <- as.numeric(c(TOA.i[,9:16]))
  
  #get relative TOAs
  TOAs.i <- TOAs.i - relative_drift - starttimes 
  TOAs.i <- as.numeric(TOAs.i - min(na.omit(TOAs.i)))
  
  #set NA's to 100
  TOAs.i[is.na(TOAs.i)] <- 100
  
  #set everything 2 & above to NA
  TOAs.i[TOAs.i>=2] <- NA
  
  #get mean temperature across the sensors
  sub_md <- audio_md[audio_md$Timestamp == timestamp_name[i],]
  temp = mean(sub_md$Temperature.Int)
  
  #store relative TOAs, temperature and timestamp in a vector 
  row.i <- c(TOAs.i, temp, timestamp_name[i])
  
  #add the vector to the df
  sounds <- rbind(sounds, row.i)
  
}

#set column names for in the localize function
colnames(sounds) <- c("t1","t2","t3","t4","t5","t6","t7","t8","temp", "name")
sounds <- sounds[,colnames(sounds) %in% c("t1","t2","t3","t4","t5","t6","t7","t8","temp", "name")]

#convert temperature to numeric
sounds$temp <- as.numeric(sounds$temp)

#remove rows with too many NAs
sounds <- sounds[rowSums(is.na(sounds))<7,]



################# calculate GPS coordinates using triangulation ########################

#load the functions
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


#run the function to predict GPS coordinates of the test points
localize_df <- localize(mics = mics, sounds = sounds, temps = NULL, tol=1e-10)


#alternatively, if you want to run the function and bootstrap for the lowest
#error in metres by removing single or multiple TOAs (because some TOAs might
#be unreliable) try the following loop:

#rename the df for in the loop
df <- sounds

# Create an empty vector to store the results
name <- c()
Results <- c()
RESULTS <- data.frame()

#set the minimumvalue of the error
# (we bootstrap for lowest error but errors below 1m seem unreliable)
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



################  calculate the real error of the test points  #######################################

#combine test point df with real coordinates and localize_df/RESULTS with calculated coordinates
TOATestPointss <- TOATestPoints %>% dplyr::rename("name" = "timestamp")
combined <- left_join(RESULTS, TOATestPointss, by = "name")

#function for calculate distance between localize points and real points 
distance <- function(X_A, Y_A, X_B, Y_B){
  sqrt((X_B - X_A)^2 + (Y_B - Y_A)^2)
  
}

#calculate the distance between predicted gps coordinates and real gps coordinates of Test Points
coordinates <- combined %>% select("east", "north", UTM_X, UTM_Y) #select columns needed
RealError <- apply(coordinates, 1 , function(x) distance(x[1], x[2], x[3], x[4]))
combined$RealError <- RealError 

#df with all information on test points
TestPoints_localization_bootstrap <- left_join(localize_df, TOATestPointss, by = "name")



###########   plot the relation between the error of the localize function and the real error ##########


library(ggplot2)
ggplot(TestPoints_localization_bootstrap,
  aes(x = err.metres, y = RealError)) +
  geom_point() +             # Add scatter points
  geom_smooth(method = "lm") +  # Add a line of best fit
  geom_abline(intercept = 0, slope = 1) # add a y = x line to see if the errors scale linearly

# As you can see low errors provided by the function do not imply low real errors at all. Therefore,
# we can not use them as metric for selecting howler localizations. 

