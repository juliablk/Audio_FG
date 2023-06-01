rm(list=ls())
library("Rraven")
library("dplyr")
library(strex)

#for each group, there needs to be a directory with all clipped downsampled files

TR.group <- "TRB" 
TR_.group <- "TR-B"

#Setting a directory for the audio-files, and for the one selection table file on which
#the "fake" selection tables will be based
audio_directory<-  paste0("E:/Triangulation 2023/clips/", TR.group, "/", TR_.group, "_clipped_ds")
seltable_directory<- "E:/Triangulation 2023/AudioStats_Faketable"

#Importing that one selection table into R 
sel_tab <- imp_raven(path=seltable_directory,all.data = TRUE, freq.cols = FALSE,
                     warbler.format=F, name.from.file = TRUE,
                     ext.case ="lower")

#Renaming some of the columns of the selection table so that Raven won't hate it.
colnames(sel_tab)[which(colnames(sel_tab)=="Selection")] <- "selec"
colnames(sel_tab)[which(colnames(sel_tab)=="Begin Time (s)")] <- "start"
colnames(sel_tab)[which(colnames(sel_tab)=="End Time (s)")] <- "end"
sel_tab <- sel_tab %>% mutate_at(c('start', 'end'), as.numeric)

#Getting a list of the audio files within our audio file directory (might need to change around
#recursive=F/T or full.names=F/T)
audio_files<- list.files(path=audio_directory,recursive=F,pattern=".wav",full.names = F)

#getting names right 
#audio_files <- sub(".*/", "", audio_files)


#Throwing out files that are too small, and would therefore likely not work in Raven
#audio_files <- audio_files[file.size(audio_files)>=14304908]

#Editing the selection table so that it has as many rows as there are audio files in the audio directory
sel_tab<- sel_tab[rep(1:nrow(sel_tab), each=length(audio_files)),]

#Editing the last column of the selection table so that the names of the audio files in the audio directory
#are listed.
sel_tab$sound.files<- audio_files

#Setting the frequency range of the selection (might want to change/vary)
sel_tab$`High Freq (Hz)` <- 450
sel_tab$`Low Freq (Hz)` <- 250

#Setting the end times for each file
#end_times<- read.csv("E:/Triangulation 2023/lengthsTRA_csv.csv")
end_times<- readRDS(paste0("E:/Triangulation 2023/cormatrices/", TR.group, "/templengths", TR_.group, ".rData"))

end_times <- df_durations2

end_times$sound.files <- gsub(".wav", "_2k.wav", str_after_last(end_times$PATH, "/"))



sel_tab <- left_join(sel_tab, end_times, by = "sound.files")



#audio_files_2<- paste("D:/Triangulation 2023/TR-A_clipped/", audio_files, sep="") #If only using a selection of the files
#end_times<- end_times[end_times$PATH %in% audio_files_2,] #If only using a selection of the files
sel_tab$end<- as.numeric(sel_tab$LENGTH) - 20
sel_tab$start <- 20
sel_tab <- sel_tab[, !colnames(sel_tab) %in% c("LENGTH", "PATH", "TEMPERATURE")]



#Exporting the selection table from R into a .txt file. This .txt file can then be dragged into Raven using control click

#raven can't handle 1304 audio fragments....
#so we're trying half
sel_tab1 <- sel_tab[1:as.integer((nrow(sel_tab)/2)),]
sel_tab2 <- sel_tab[as.integer((nrow(sel_tab)/2+1)): nrow(sel_tab),]

output_directory<- paste0("E:/Triangulation 2023/clips/", TR.group, "/AudioStats_Table_for_Raven")
exp_raven(sel_tab1,path=output_directory ,file.name="Output_table1",sound.file.path = audio_directory, single.file=T, pb=T)
exp_raven(sel_tab2,path=output_directory ,file.name="Output_table2",sound.file.path = audio_directory, single.file=T, pb=T)


#Setting a directory where to pull the results file from
Results_directory1<- paste0("E:/Triangulation 2023/clips/", TR.group, "/AudioStats_Results1")
Results_directory2 <- paste0("E:/Triangulation 2023/clips/", TR.group, "/AudioStats_Results2")

dir.create(Results_directory1)
dir.create(Results_directory2)

#doing the same for the whole bandwidth...

#Setting the frequency range of the selection (might want to change/vary)
sel_tab$`High Freq (Hz)` <- 1000
sel_tab$`Low Freq (Hz)` <- 0
sel_tab$start <- 0
sel_tab$end <- 20

sel_tab3 <- sel_tab[1:as.integer((nrow(sel_tab)/2)),]
sel_tab4 <- sel_tab[as.integer((nrow(sel_tab)/2+1)): nrow(sel_tab),]

exp_raven(sel_tab3,path=output_directory ,file.name="Output_table3",sound.file.path = audio_directory, single.file=T, pb=T)
exp_raven(sel_tab4,path=output_directory ,file.name="Output_table4",sound.file.path = audio_directory, single.file=T, pb=T)

#Setting a directory where to pull the results file from
Results_directory3<- paste0("E:/Triangulation 2023/clips/", TR.group, "/AudioStats_Results_fullband1")
Results_directory4 <- paste0("E:/Triangulation 2023/clips/", TR.group, "/AudioStats_Results_fullband2")

dir.create(Results_directory3)
dir.create(Results_directory4)





#############################################################################################################

#this part of the script saves the previously created audiostats files into 1 audiostats rdata file per grid
# (after adding aiSNR, the avgPower density of the howler call/avgPower density of the background noise),
# and checks whether the filter values are ok

#set group
TRgroup <- "TRB"


#import filea_audiostats for selections (250-450)
pathr = paste0("E:/Triangulation 2023/clips/", TRgroup, "/AudioStats_Results1")
pathr2 = paste0("E:/Triangulation 2023/clips/", TRgroup, "/AudioStats_Results2")


Results1<- imp_raven(path=pathr,all.data = TRUE, freq.cols = FALSE,
                     warbler.format=F, name.from.file = TRUE,
                     ext.case ="lower")


 Results2<- imp_raven(path=pathr2,all.data = TRUE, freq.cols = FALSE,
                     warbler.format=F, name.from.file = TRUE,
                     ext.case ="lower")

Results <- rbind(Results1, Results2)

#import file_audiostats for whole clip
pathr3 = paste0("E:/Triangulation 2023/clips/", TRgroup, "/AudioStats_Results_fullband1")
pathr4 = paste0("E:/Triangulation 2023/clips/", TRgroup, "/AudioStats_Results_fullband2")


Results3<- imp_raven(path=pathr3,all.data = TRUE, freq.cols = FALSE,
                     warbler.format=F, name.from.file = TRUE,
                     ext.case ="lower")


Results4<- imp_raven(path=pathr4,all.data = TRUE, freq.cols = FALSE,
                     warbler.format=F, name.from.file = TRUE,
                     ext.case ="lower")

Results_fullband <- rbind(Results3, Results4)
saveRDS(Results_fullband, paste0("E:/Triangulation 2023/clips/", TRgroup, "/AudioStats_Results_fullband.rData"))



#look at which avg power density value signals should be excluded for triangulation. below -80 signals look really weak/are not there
length(Results$`Avg Power Density (dB FS/Hz)`[Results$`Avg Power Density (dB FS/Hz)`> -80])


#add artificial SNR, by dividing avg power of the selection (which should contain a howler)
#by the avg power of the whole clip
Results <- Results %>% arrange(`Begin File`)
Results_fullband <- Results_fullband %>% arrange(`Begin File`)
Results$artificialSNR <- Results$`Avg Power Density (dB FS/Hz)`/ Results_fullband$`Avg Power Density (dB FS/Hz)`

#artificial SNR should be lower than 1, as avg power density will approximate 0 for louder signals (e.g. -50), divided by file avg power density which should be much lower (e.g. -80)
length(Results$artificialSNR[ Results$artificialSNR <= 1])


saveRDS(Results, paste0("E:/Triangulation 2023/clips/", TRgroup, "/AudioStats_Results.rData"))





