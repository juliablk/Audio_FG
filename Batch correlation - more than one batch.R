library("Rraven")
library("dplyr")
library("data.table")
library(strex)

TR.group <- "TRB"



# Import all text files
wd <- "E:/Triangulation 2023"
corr_mats <- list.files(paste0(wd),pattern="*.txt",recursive=F)


# Get the start signal file

corr_mats <- corr_mats[which(corr_mats %like% TR.group)]

wd = "E:/Triangulation 2023/"


for (i in 2:length(corr_mats)){
  

cormatrix <- corr_mats[i]


#Importing a dataframe containing the correlation peaks information for a Raven batch correlation between
#files of different time stamps, and then splitting that large dataframe up into smaller dataframes
#containing information only for the correlation peaks between files with the same timestamp
Mega_Batchcor_corrpeaks <- data.frame(imp_corr_mat(cormatrix, path =wd )[[1]])
#unique_codes<- unique(sub(".*_(\\d{8}_\\d{6}\\.wav)$", "\\1" , c(rownames(Mega_Batchcor_corrpeaks), colnames(Mega_Batchcor_corrpeaks)), perl=T))
codes <- str_after_first(rownames(Mega_Batchcor_corrpeaks), "_")
unique_codes <- unique(str_before_last(codes, "_"))

split_df_corrpeaks<- lapply(unique_codes, function(code){
  rows<- grepl(paste0(code), rownames(Mega_Batchcor_corrpeaks))
  cols<- grepl(paste0(code), colnames(Mega_Batchcor_corrpeaks))
  Mega_Batchcor_corrpeaks[rows, cols]
})



#Importing a dataframe containing the correlation lags information for a Raven batch correlation between
#files of different time stamps, and then splitting that large dataframe up into smaller dataframes
#containing information only for the correlation lags between files with the same timestamp
Mega_Batchcor_lags <- data.frame(imp_corr_mat(cormatrix, wd)[[2]])
split_df_lags<- lapply(unique_codes, function(code){
  rows<- grepl(paste0(code), rownames(Mega_Batchcor_lags))
  cols<- grepl(paste0(code), colnames(Mega_Batchcor_lags))
  Mega_Batchcor_lags[rows, cols]
})

#Creating a .txt file for the correlation lags and peaks of each group of files that share the same timestamp
for (i in seq_along(unique_codes)){
  code<-unique_codes[i]
  code_df_peaks<- split_df_corrpeaks[[i]]
  code_df_lags<-split_df_lags[[i]]
  file_name_peaks<- paste0(wd, "cormatrices/", TR.group, "/", code, "_peaks.txt")
  file_name_lags<- paste0(wd, "cormatrices/",TR.group, "/", code, "_lags.txt")
  write.table(code_df_peaks, file_name_peaks, sep=",", row.names=T, col.names=T, quote=F)
  write.table(code_df_lags, file_name_lags, sep=",", row.names=T, col.names=T, quote=F)
}

}
# #listing all corr_lags and all corr_peaks files if needed
# call_corr_allfiles <- list.files(paste0(wd, "cormatrices"),pattern="*.txt",recursive=T)
# call_corr_lags<- call_corr_allfiles[call_corr_allfiles %like% "lags"]
# call_corr_peaks<- call_corr_allfiles[call_corr_allfiles %like% "peaks"]

#Example for importing the created .txt files into R as a dataframe with the same format as the
#ones created as input in the triangulation script
CorrLags<-read.csv("D:/SNR trial/20230405_012722_peaks.wav.txt", row.names=1, quote="")
CorrPeaks<-read.csv("D:/SNR trial/20230405_012722_lags.wav.txt", row.names=1, quote="")

