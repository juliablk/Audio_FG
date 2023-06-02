#This script copies audiofiles that are in one single folder and pastes them in 
#folders according to their timestamp, the output will be stored in TR-A_grouped_ds

#list files from TRA_clipped

#setwd("C:/Users/Beheerder/Documents/UU/A GSLS/MinPr/R/Audio2023")
setwd("E:/Triangulation 2023")
#wd <- "C:/Users/Beheerder/Documents/UU/A GSLS/MinPr/R/Audio2023"
wd <- "E:/Triangulation 2023"


#create df with filename and timestampsec
#for each timestamps sec 
#       1. subset file list
#       2. create dir 
#       3. copy files to new dir (file.copy)

#audiofiles <- list.files(path = "E:/Triangulation 2023/TR-B_clipped_ds", recursive = F, pattern = ".wav", full.names = T )


dirs <- c("TRB1-20","TRB21-40", "TRB41-60", "TRB61-98" )
#lapply(dirs, function (x) {dir.create(path = paste0(wd, "/TRB_grouped_ds/", x))})

for (i in 1:length(dirs)){ 


x <- dirs[i]


audiofiles <- list.files(path=paste0(wd, "/TRB_grouped_ds/", x),recursive=T,pattern=".wav",full.names = T)



audio.df <- data.frame(audiofiles)


#audio.df$filename <- gsub(".*/", "", audio.df$audiofiles)
#audio.new <-  paste0(wd, "/TRA0406_clipped/", audio.df$filename)

#file.copy(audiofiles, audio.new)


#is vast een soepelere manier maar chatgpt werkte niet mee
filenameswo2k <- gsub("_2k.wav", "",  audio.df$audiofiles)

timestamp_day <-  sub(".*_", "", sub("_[^_]*$", "", filenameswo2k))
timestamp_time <- sub(".*_", "", filenameswo2k)
audio.df$timestamp.sec <- paste0(timestamp_day, "_", timestamp_time)

audio.df$filename <- gsub(".*/", "", audio.df$audiofiles)




file.copy(audio.df$audiofiles, paste0(wd, "/TRB_grouped_ds/", x, "/", audio.df$filename ))

} 





warnings()

timestampsecs <- unique(audio.df$timestamp.sec)



for (i in 1:length(timestampsecs)){
  
  
  
  timestamp.i = timestampsecs[i]
  
  #subset files
  audio.timestamp <- subset(audio.df, timestamp.sec == timestamp.i)
  
  #create directory
  dir <- paste0(wd, "/TRB_grouped_ds/", timestamp.i)
  if (!dir.exists(dir))  dir.create(dir, recursive = T) 
  
  #copy files into directory
  newfile <- paste0(dir, "/", audio.timestamp$filename)[1] 
  
  if(!file.exists(newfile)){
  file.copy(audio.timestamp$audiofiles, paste0(dir, "/", audio.timestamp$filename ))
  
  }

  
  
}







