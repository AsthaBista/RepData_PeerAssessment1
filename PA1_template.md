Reproducible Research: Peer Assessment 1
================================================================================ 
## Introduction
It is now possible to collect a large amount of data about personal movement 
using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone 
Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. 
This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

### Loading and preprocessing the data
The data for this assignment were downloaded from the course web site.


```r
uzData<- unzip("activity.zip")   # unzip the zipped file
activity<-read.csv("activity.csv",na.strings = NA, header = TRUE)
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
Dataset: Activity monitoring data
The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded
as NA
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken

## What is mean total number of steps taken per day?
The total number of steps was grouped daily and total steps per day was calculated using the dplyr package.


```r
library(dplyr)
library(ggplot2)
```

```r
grouppbyDay<-activity %>% group_by(date) %>%
        summarise(total_steps = sum(steps, na.rm = TRUE))
```

```r
hist(grouppbyDay$total_steps, breaks = 30, xlab = "Total steps", col = "light blue",
     main = " Total number of steps per day")
abline(v=mean(grouppbyDay$total_steps),col = "red", lty=2)
abline(v=median(grouppbyDay$total_steps),col = "red", lty=2)
```

![plot of chunk plot](figure/plot-1.png)

Here, the mean of the total number of steps taken is 9354.23 and the median is 10395.

## What is the average daily activity pattern?
The time series of steps in a day can be determined by taking an average of the steps at each 5-minute interval in all number of days.

```r
groupbyInterval<-activity %>% group_by(interval) %>%
        summarise(average_steps = mean(steps, na.rm = TRUE))
```

```r
g<-ggplot(groupbyInterval, aes(interval, average_steps))
g + geom_line(color = "blue") + labs(x = "Minutes", y = "Average number of steps")+
        labs(title = "Average number of steps along a day") + theme_bw()
```

![plot of chunk plot2](figure/plot2-1.png)

Here, the number of steps reaches maximum at 835 minutes after taking an average across days.

## Imputing missing values


In the original datasets, there a lot of values in steps column that are missing. The total number of rows missing values of steps are 2304. The missing values can be imputed by filling the NAs with the mean steps in that 5-minute interval across all the days. 


```r
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
```

```
## # A tibble: 17,568 x 3
##    Date       Interval Steps_new
##    <fct>         <int>     <dbl>
##  1 2012-10-01        0    1.72  
##  2 2012-10-01        5    0.340 
##  3 2012-10-01       10    0.132 
##  4 2012-10-01       15    0.151 
##  5 2012-10-01       20    0.0755
##  6 2012-10-01       25    2.09  
##  7 2012-10-01       30    0.528 
##  8 2012-10-01       35    0.868 
##  9 2012-10-01       40    0     
## 10 2012-10-01       45    1.47  
## # ... with 17,558 more rows
```
The above dataframe consists of all As imputed by the average number of steps for each interval. Now, to check if there are missing values,

```r
anyNA(activity2$Steps_new)   #Check for NA values
```

```
## [1] FALSE
```
The new dataframe has no missing values


```r
# Now fnding the total number of steps in a day
grouppbyDay2<-activity2 %>% group_by(Date) %>%
    summarise(total_steps = sum(Steps_new, na.rm = TRUE))

# Plot histogram
hist(grouppbyDay2$total_steps, breaks = 30, xlab = "Total steps", col = "light blue",
     main = " Total number of steps per day")
abline(v=mean(grouppbyDay2$total_steps),col = "red", lty=2)
abline(v=median(grouppbyDay2$total_steps),col = "red", lty=2)
```

![plot of chunk hist2](figure/hist2-1.png)

Here, we can see that after removing the NAs from the dataframe, the mean at 10766.189 coincides with median 10766.189. It seems like the weight of the values are balanced by imputing the NAs as the average of all days, making the mean coincide with the median.

## Are there differences in activity patterns between weekdays and weekends?
A new factor column was added that separated out the weekends from the weekdays.

```r
# Create a new factor column separating weekdays and weekend
activity3<-activity2 %>% 
    mutate(Day = ifelse(weekdays(as.Date(activity2$Date)) == c("Sunday", "Saturday"),
                        "weekend","weekday")) %>%
    group_by(Day)  
```
Now, an average of all weekdays were taken for each interval, and the same process was carried out for weekdays.

```r
groupbyweek<-activity3 %>% group_by(Interval,Day) %>%
    summarise(average_steps = mean(Steps_new, na.rm = TRUE))
```
These average number of steps were plotted separately for weekdays and weekends.

```r
g<-ggplot(groupbyweek, aes(Interval, average_steps))
g + geom_line(color = "blue") + labs(x = "Minutes", 
                                     y = "Average number of steps")+
    labs(title = "Average number of steps per interval for weekend and weekday") + theme_bw()+
    facet_grid(.~Day)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

It looks like the number steps is more unsteady with lots of higher and lower values during weekends. It may mean that during weekends people are more likely to do more activities or just rest, in contrast to weekdays when the number of steps seem to be fairly consistent.
    
