---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Here we are going to load the csv file in RStudio.


```r
MyData <- read.csv(file="activity.csv", header=TRUE, sep=",")
library(lubridate) 
```

```
## Warning: package 'lubridate' was built under R version 3.6.1
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
MyData$date <- ymd(MyData$date)
```

## What is mean total number of steps taken per day?


```r
total.steps <- tapply(MyData$steps, MyData$date, FUN=sum, na.rm=TRUE)
hist(total.steps, xlab = "steps per day", main = "Total number of steps taken on each day", breaks = 30, col = "pink")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
summary(total.steps)[3:4]
```

```
##   Median     Mean 
## 10395.00  9354.23
```
## What is the average daily activity pattern?


```r
library(ggplot2)
```

```
## Registered S3 methods overwritten by 'ggplot2':
##   method         from 
##   [.quosures     rlang
##   c.quosures     rlang
##   print.quosures rlang
```

```r
interval.steps <- aggregate(x=list(steps = MyData$steps), by=list(interval = MyData$interval), FUN=mean, na.rm=TRUE)

ggplot(data=interval.steps, aes(x=interval, y = steps)) +geom_line(col = "red") + xlab("5-minute Interval")+ylab("Avg No of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

On average across all the days in the dataset, this interval contains the maximum number of steps:


```r
interval.steps[which.max(interval.steps$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```


## Imputing missing values

There are a number of days/intervals where there are missing values (coded as 'NA'. The presence of missing days may introduce bias into some calculations or summaries of the data.

Total number of intervals missing the steps are:


```r
summary(MyData$steps)[7]
```

```
## NA's 
## 2304
```

All of the missing values are substituted with mean value for that 5-minute
interval.


```r
## Replace missing value with the mean value of that interval

mean.steps <- function(steps, interval){
  new_step <- NA
  if(!is.na(steps))
    new_step <- steps
  else
    new_step <- interval.steps[interval.steps$interval == interval, "steps"]
  return(new_step)
  
}

filled.data <- MyData
filled.data$steps <- mapply(mean.steps, filled.data$steps, filled.data$interval)
```
Now, using the filled data set, let's make a histogram of the total number of steps taken each day and calculate the mean and median total number of steps.


```r
full.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
hist(full.steps, xlab = "steps per day", main = "Total number of steps taken on each day", breaks = 30, col = "pink")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
mean(full.steps)
```

```
## [1] 10766.19
```

```r
median(full.steps)
```

```
## [1] 10766.19
```

Now the mean and median are higher than before because previously there were many values with NA, now they have been replaced with mean of that interval.

## Are there differences in activity patterns between weekdays and weekends?

Lets find out if a its a weekday or weekend. To do that creating a function for segregating weekend and weekdays using the inbuild weekdays() function of R


```r
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)
```

Now, let's make a panel plot containing plots of average number of steps taken
on weekdays and weekends.

```r
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

