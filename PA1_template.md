---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

1. Load the data (i.e. `read.csv()`)


```r
data <- read.csv("activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

The first few lines of the data file:


```r
head(data)
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

For this part of the assignment, missing values in the dataset have been ignored (removed).

Adding up the total number of steps per day, w/ NAs included:


```r
day.steps.NAs <- sapply(split(data$steps, data$date), sum)
```

Remove the NAs:


```r
day.steps <- day.steps.NAs[!is.na(day.steps.NAs)]
```



1. Make a histogram of the total number of steps taken each day


```r
hist(day.steps, main="Total Number of Steps Taken Per Day", xlab="Number of Steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

2. Calculate and report the **mean** and **median** total number of steps taken per day


```r
mean.day.steps <- format(mean(day.steps), width=5)
median.day.steps <- median(day.steps)
```


The **mean** total number of steps taken per day is 10766.19.

The **median** total number of steps taken per day is 10765.

## What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
interval.steps <- sapply(split(data$steps, data$interval), mean, na.rm=TRUE)

plot(interval.steps, type="l", main="Average Number of Steps Per 5-Min Workout Interval", xlab="5-Min Workout Interval", ylab="Average Numebr of Steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max.interval.index <- which.max(interval.steps)
max.interval.name <- names(which.max(interval.steps))
```

The 104th 5-minute interval (labeled "835") contains the maximum number of steps.

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)


```r
num.missing.values <- sum(is.na(data$steps))
```

The total number of rows with `NA`s is 2304.


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


The strategy I used is to fill a particular NA with that interval's (rounded) mean value across all days with non-NA values for that interval.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

Here is the code to implement the above strategy:


```r
data.filled <- data
intervals <- as.numeric(names(interval.steps))
for (interval in intervals) {
    interval.index <- which(intervals == interval)
    data.filled[is.na(data$steps) & data$interval == interval,1] <- round(interval.steps[interval.index])
}
```

4. Make a histogram of the total number of steps taken each day and calculate and report the **mean** and **median** total number of steps taken per day. 

Calculating the total number of steps each day using the imputed dataset:


```r
day.steps.filled <- sapply(split(data.filled$steps, data.filled$date), sum)
```

Histogram:


```r
hist(day.steps.filled, main="Total Number of Steps Taken Per Day Using Imputed Data", xlab="Number of Steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 


```r
mean.imputed <- format(mean(day.steps.filled), width=5)
median.imputed <- format(median(day.steps.filled), width=5)
```

The **mean** steps per day using imputed data is 10765.64.

The **median** steps per day using imputed data is 10762.

*Do these values differ from the estimates from the first part of the assignment?*

Yes, the **mean** and **median** values for imputed data are different than non-imputed data, though the values are close. Adding the mean to a vector of numbers will not change the mean of the new vector. Example:


```r
raw.mean <- mean(c(3,8,3,NA,6), na.rm=TRUE)
imputed.mean <- mean(c(3,8,3,raw.mean,6))
```

Raw mean = 5.

Imputed mean = 5.

There is a slight difference in the result for this problem because my strategy called for adding the **rounded** mean to replace `NA`s rather than the actual mean.

*What is the impact of imputing missing data on the estimates of the total daily number of steps?*

For estimates of the **total daily** number of steps, as detailed in the answer to the previous question, imputing (rounded) interval means for NA interval values will result in approximately similar estimates -- again, the difference being because of my strategy's use of **rounded** interval means rather than exact interval means. Note that this assignment specified that: *the strategy does not need to be sophisticated.*


## Are there differences in activity patterns between weekdays and weekends?

*For this part the `weekdays()` function may be of some help here. Use the dataset with the filled-in missing values for this part.*

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
weekdays.filled <- weekdays(as.Date(data.filled$date))
weekdays.filled[weekdays.filled=="Saturday" | weekdays.filled=="Sunday"] <- "weekend"
weekdays.filled[!(weekdays.filled=="weekend")] <- "weekday"
data.filled.factor <- cbind(data.filled, weekdays.filled)
```

1. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using **simulated data**:


```r
weekday.data <- data.filled.factor[data.filled.factor$weekdays.filled == "weekday",]
weekend.data <- data.filled.factor[data.filled.factor$weekdays.filled == "weekend",]

weekday.mean.steps <- sapply(split(weekday.data$steps, weekday.data$interval), mean)
weekend.mean.steps <- sapply(split(weekend.data$steps, weekend.data$interval), mean)

par(mfrow = c(2,1))
plot(weekday.mean.steps, type="l", main="Weekday", ylim=c(0,250), xlab="5-Min Workout Interval", ylab="Average Numebr of Steps")
plot(weekend.mean.steps, type="l", main="Weekend", ylim=c(0,250),  xlab="5-Min Workout Interval", ylab="Average Numebr of Steps")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png) 

```r
par(mfrow=c(1,1))
```
