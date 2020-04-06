setwd("C:/Users/Aastha/Desktop/datasciencecoursera/RepData_PeerAssessment1")

library(dplyr)

## Preparing the data
uzData<- unzip("activity.zip")
activity<-read.csv("activity.csv",na.strings = NA, header = TRUE)
head(activity)
