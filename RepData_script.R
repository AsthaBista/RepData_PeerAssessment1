setwd("C:/Users/Aastha/Desktop/datasciencecoursera/RepData_PeerAssessment1")

library(dplyr)
library(ggplot2)

## Preparing the data
uzData<- unzip("activity.zip")
activity<-read.csv("activity.csv",na.strings = NA, header = TRUE)
tbl_df(activity)
View(activity)

## What is mean total number of steps taken per day?
grouppbyDay<-activity %>% group_by(date) %>%
    summarise(total_steps = sum(steps, na.rm = TRUE))
png("Total_steps_per_day.png")
hist(grouppbyDay$total_steps, breaks = 30, xlab = "Total steps", col = "light blue",
     main = " Total number of steps per day")
abline(v=mean(grouppbyDay$total_steps),col = "red", lty=2)
abline(v=median(grouppbyDay$total_steps),col = "red", lty=2)
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


# Copy the original dataset into a new dataframe
activity2<-activity

# Loop around the new dataframe rows and substitute the NA places with mean of 
# the steps per 5-minute interval across all days
for(i in 1:nrow(activity2)){
    if(is.na(activity2[i,1])){
        activity2[i,4]<-as.numeric(groupbyInterval[which(groupbyInterval$interval
                                                        ==activity2[i,3]),2])
    }else{
        activity2[i,4]<-activity2[i,1]
    }
}
names(activity2)<-c("steps", "Date","Interval", "Steps_new")
activity2<-select(activity2, Date:Steps_new)
tbl_df(activity2)

# Now fnding the total number of steps in a day
grouppbyDay2<-activity2 %>% group_by(Date) %>%
    summarise(total_steps = sum(Steps_new, na.rm = TRUE))

# Plot histogram
png("Total_steps_per_day_imputedData.png")
hist(grouppbyDay2$total_steps, breaks = 30, xlab = "Total steps", col = "light blue",
     main = " Total number of steps per day")
abline(v=mean(grouppbyDay2$total_steps),col = "red", lty=2)
abline(v=median(grouppbyDay2$total_steps),col = "red", lty=2)
dev.off()



t<-as.POSIXlt(as.Date(activity$date),format = "%Y%m%d %H:%M")
names(unclass(t))