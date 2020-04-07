setwd("C:/Users/Aastha/Desktop/datasciencecoursera/RepData_PeerAssessment1")

library(dplyr)

## Preparing the data
uzData<- unzip("activity.zip")
activity<-read.csv("activity.csv",na.strings = NA, header = TRUE)
head(activity)


## What is mean total number of steps taken per day?
grouppbyDay<-activity %>% group_by(date) %>%
        summarise(total_steps = sum(steps, na.rm = TRUE))
png("Total_steps_per_day.png")
hist(grouppbyDay$total_steps, xlab = "Total steps", col = "light blue",
     main = " Total number of steps per day")
dev.off()
mean(grouppbyDay$total_steps)
median(grouppbyDay$total_steps)
