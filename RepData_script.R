setwd("C:/Users/Aastha/Desktop/datasciencecoursera/RepData_PeerAssessment1")

library(dplyr)
library(ggplot2)

## Preparing the data
uzData<- unzip("activity.zip")
activity<-read.csv("activity.csv",na.strings = NA, header = TRUE)
tbl_df(activity)
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

## What is the average daily activity pattern?
groupbyInterval<-activity %>% group_by(interval) %>%
    summarise(average_steps = mean(steps, na.rm = TRUE))
png("Average_number_of_steps.png")
g<-ggplot(groupbyInterval, aes(interval, average_steps))
g + geom_line(color = "blue") + labs(x = "Minutes", 
                                     y = "Average number of steps")+
    labs(title = "Average number of steps along a day") + theme_bw()
dev.off()


# Interval with maximum steps
groupbyInterval[which.max(groupbyInterval$average_steps),1] 

## Imputing missing values
missingNr<-sum(is.na(activity$steps))   #Total missing values
activityImp<-activity %>% group_by(interval) %>%
    mutate(step_new = ifelse(is.na(steps),as.numeric(groupbyInterval[
        which(groupbyInterval$interval==interval),2]),steps))
anyNA(activityImp$step_new)   #Check for NA values



t<-as.POSIXlt(as.Date(activity$date),format = "%Y%m%d %H:%M")
names(unclass(t))