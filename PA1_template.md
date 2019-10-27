---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
- Load in the data

```r
activity <- read.csv("activity.csv")
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


## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```r
total_steps <- aggregate(steps~date,
                         data = activity, 
                         FUN = sum, 
                         na.rm = T)
head(total_steps)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

2. Make a histogram of the total number of steps taken each day

```r
hist(total_steps$steps, 
     main = "Total Number of Steps Taken per Day",
     xlab = "Number of Steps", 
     col = "green")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(total_steps$steps)
```

```
## [1] 10766.19
```

```r
median(total_steps$steps)
```

```
## [1] 10765
```
- The mean number of steps taken per day is 1.0766189\times 10^{4} steps. 
- The median number of steps taken per day is 10765 steps. 


## What is the average daily activity pattern?
1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) 

- First, a new data set with the mean steps for each time interval should be made. This can then be plotted to create the time series in question. 

```r
interval_steps <- aggregate(steps~interval, 
                            data = activity, 
                            FUN = mean, 
                            na.rm = T)
head(interval_steps)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```


```r
plot(steps~interval, 
     interval_steps, 
     type = "l", 
     xlab = "Interval", 
     ylab = "Steps"
     )
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
interval_steps$interval[which.max(interval_steps$steps)]
```

```
## [1] 835
```
- Interval 835 contains the maximum number of steps. 

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset

```r
sum(is.na(activity))
```

```
## [1] 2304
```

- There are 2304 rows with missing data. 

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

- One strategy that could account for missing values of steps would be to use the mean number of steps for the giving time interval to replace the NA in the data. This can be accomplished by merging the data set containing the mean steps for each time interval with the original 'activity' data set.  
 
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity_noNA <- merge(activity, interval_steps, by = "interval", suffixes = c("", ".x"))
head(activity_noNA)
```

```
##   interval steps       date  steps.x
## 1        0    NA 2012-10-01 1.716981
## 2        0     0 2012-11-23 1.716981
## 3        0     0 2012-10-28 1.716981
## 4        0     0 2012-11-06 1.716981
## 5        0     0 2012-11-24 1.716981
## 6        0     0 2012-11-15 1.716981
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
total_steps_new <- aggregate(steps~date, data = activity_noNA, sum)
hist(total_steps_new$steps, 
     main = "Total Number of Steps Taken per Day: NAs Accounted For", 
     xlab = "Number of Steps", 
     col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean(total_steps_new$steps)
```

```
## [1] 10766.19
```

```r
median(total_steps_new$steps)
```

```
## [1] 10765
```

When missing values are accounted for, the mean number of steps taken per day is 1.0766189\times 10^{4} steps and the the median is 10765 steps. 

This mean and median are the same as before the missing data was imputed because the mean value for each interval was used. 

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
days <- weekdays(as.Date(activity$date))
day_sorting <- function(day){
  weekends <- c("Saturday", "Sunday")
  if (day %in% weekends){
    category <- "Weekend"
  }
  else {
    category <- "Weekday"
  }
  return(category)
}
activity$weekday <- factor(sapply(days, day_sorting))
head(activity)
```

```
##   steps       date interval weekday
## 1    NA 2012-10-01        0 Weekday
## 2    NA 2012-10-01        5 Weekday
## 3    NA 2012-10-01       10 Weekday
## 4    NA 2012-10-01       15 Weekday
## 5    NA 2012-10-01       20 Weekday
## 6    NA 2012-10-01       25 Weekday
```

2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
interval_steps_by_day <- aggregate(steps ~ interval + weekday, 
                                   data = activity, 
                                   mean)
head(interval_steps_by_day)
```

```
##   interval weekday     steps
## 1        0 Weekday 2.3333333
## 2        5 Weekday 0.4615385
## 3       10 Weekday 0.1794872
## 4       15 Weekday 0.2051282
## 5       20 Weekday 0.1025641
## 6       25 Weekday 1.5128205
```

```r
library(lattice)
xyplot(steps ~ interval | weekday, 
       interval_steps_by_day, 
       type = "l", 
       xlab = "Interval", 
       ylab = "# of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


