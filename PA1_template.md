---
title: "Reproducible Research: Peer Assessment 1"
output:
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
steps_dataset <- read.csv("./activity.csv", header = TRUE, na.strings = 'NA')
```

## What is mean total number of steps taken per day?


```r
date_summary <- aggregate(steps~date, steps_dataset, sum)
```
Plot Histogram of steps taken each day:


```r
library(ggplot2)
ggplot(date_summary, aes(x=as.Date(date), y=steps))+
geom_col(fill='black')+
xlab('Date (Year = 2012)')+
ylab('Steps')+
ggtitle('Steps Taken per Day')
```

![](PA1_template_files/figure-html/hist-1.png)<!-- -->

Calculate mean and median number of steps taken each day:


```r
summary(date_summary$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

The mean number of steps taken per day is 10,766 steps.

The median number of steps taken per day is 10,765 steps.

## What is the average daily activity pattern?

Calculate average number of steps per 5-minute interval:


```r
steps_interval <- aggregate(steps~interval, steps_dataset, mean)
```
Plot time-series plot of average number of steps per 5-minute interval:


```r
plot(steps_interval$interval, steps_interval$steps, type = 'l', xlab = '5-minute Interval', 
ylab = 'Steps', main = 'Average Number of Steps per 5-minute Interval')
```

![](PA1_template_files/figure-html/time_series_plot-1.png)<!-- -->

Calculate 5-minute interval with highest average number of steps:


```r
print(steps_interval[which.max(steps_interval$steps),1])
```

```
## [1] 835
```

Interval 835 has the highest average number of steps.

## Imputing missing values

Calculate number of NAs in dataset:


```r
summary(steps_dataset$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```

There are 2,304 NAs in the dataset.

Imputation performed based on rounded 5-minute interval averages:


```r
library(plyr)
NA_dataset <- subset(steps_dataset, is.na(steps_dataset$steps))
NA_dataset <- join(NA_dataset, steps_interval, by = 'interval')
NA_dataset <- NA_dataset[,c(4,2,3)]
NA_dataset$steps <- round(NA_dataset$steps, digits = 0)
Imputed_dataset <- subset(steps_dataset, steps != 'NA')
Imputed_dataset <- rbind(NA_dataset, Imputed_dataset)
```

Determine total steps per day with imputation:


```r
Imputed_summary <- aggregate(steps~date, Imputed_dataset, sum)
```

Plot Histogram of steps taken each day with Imputed Dataset:


```r
ggplot(Imputed_summary, aes(x=as.Date(date), y=steps))+
geom_col(fill='black')+
xlab('Date (Year = 2012)')+
ylab('Steps')+
ggtitle('Steps Taken per Day with Imputation')
```

![](PA1_template_files/figure-html/imputed_hist-1.png)<!-- -->

Calculate mean and median number of steps taken each day with Imputed Dataset:


```r
summary(Imputed_summary$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10762   10766   12811   21194
```

The mean number of steps taken per day is 10,766 steps.

The median number of steps taken per day is 10,762 steps.


## Are there differences in activity patterns between weekdays and weekends?

Subset weekday and weekend datasets from imputed dataset:


```r
Imputed_dataset$day <- weekdays(as.Date(Imputed_dataset$date))
weekday_averages <- aggregate(steps~interval, subset(Imputed_dataset, day != 'Saturday' & day != 'Sunday'), FUN=mean)
weekend_averages <- aggregate(steps~interval, subset(Imputed_dataset, day == 'Saturday' | day == 'Sunday'), FUN=mean)
```

Plot weekend and weekday average steps per 5-minute interval:


```r
library(lattice)
weekend_averages$category <- c('Weekend')
weekday_averages$category <- c('Weekday')
imputed_averages <- rbind(weekday_averages, weekend_averages)
xyplot(steps~interval | category, imputed_averages, type='l', xlab = 'Interval', ylab = 'Number of Steps', layout=c(1,2))
```

![](PA1_template_files/figure-html/xyplot-1.png)<!-- -->
