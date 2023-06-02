# a script for converting coordinates (triangulation exp.)
#Julia

rm(list=ls())
setwd("C:/Users/Beheerder/Documents/UU/A GSLS/MinPr/R/Audio2023")



#load excel file with in long format stations, a column with the name of the point, a time column,
#a column with the first 4 digits of UTM X (named Xa) a column with the first 
#4 digits of UTM (named Ya), a column named XYb with for each time point the last two digits
# of X and the last two digits of Y, seperated by a white space, e.g. '92 16'

library(readr)
#GPS  <- read.csv("LocTest.csv", sep = ";")  #load the csv file
GPS  <- read.csv("TriangulationE_recorders.csv", sep = ";")  #load the csv file

library(dplyr)
#GPS <- GPS %>% subset(Day %in% c("04/11/2023", "04/05/2023" )) #select the rows needed 
#GPS <- GPS %>% subset(Day %in% c("04/11/2023" ))
#GPS <- GPS[1:127,]


#library(tidyr)
#GPS <- separate(GPS, XYb, into = c("Xb", "Yb"), sep = " ") #seperate the XYb column into two new columns
GPS$Xb <- substr(GPS$XYb, 1,2)
GPS$Yb <- substr(GPS$XYb, 4,5)

GPS$UTM_X = paste0(GPS$Xa, GPS$Xb) #create UTM X by pasting the 4 X digits and the last 2 X digits
GPS$UTM_Y = paste0(GPS$Ya, GPS$Yb) #same for UTM Y

#write.csv(GPS, )

##############################################################################

#calculating average UTM X and UTM Y per point
#Location2 <- GPS %>%  dplyr::group_by(Day, Point, Time)  %>% dplyr::summarize(UTM_X=round(mean(as.numeric(UTM_X)),0), 
#                                                                              UTM_Y = round(mean(as.numeric(UTM_Y)),0))
#Location2 <- GPS %>%  dplyr::group_by(Point, Time)  %>% dplyr::summarize(UTM_X=mean(as.numeric(UTM_X)), UTM_Y = mean(as.numeric(UTM_Y)))

Location2 <- GPS %>%  dplyr::group_by(Point)  %>% dplyr::summarize(UTM_X=round(mean(as.numeric(UTM_X)),0), 
UTM_Y = round(mean(as.numeric(UTM_Y)),0))
Location2


write.csv(Location2, "C:/Users/Beheerder/Documents/UU/stage/stage_fgr/R/myrepo/TestSoundsLocs" )




