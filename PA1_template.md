# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

Steps taken:

1. The file is downloaded and save in local
2. Set working directory to the folder where the dataset located
3. Unzip file
4. Load data into data frame by using read.csv() 
5. Transform the date column into 

```r
df <- read.csv("activity.csv", colClasses = c("numeric", "character", "numeric"))

df$date <- as.Date(df$date, format = "%Y-%m-%d")
summary(df)
```

```
##      steps            date               interval   
##  Min.   :  0.0   Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0.0   Median :2012-10-31   Median :1178  
##  Mean   : 37.4   Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.: 12.0   3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806.0   Max.   :2012-11-30   Max.   :2355  
##  NA's   :2304
```

## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day


```r
steps_by_day <- aggregate(steps ~ date, df, sum, na.rm = TRUE)
hist(steps_by_day$steps)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

2. Calculate and report the mean and median total number of steps taken per day


```r
mean(steps_by_day$steps)
```

```
## [1] 10766
```

```r
median(steps_by_day$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
steps_by_interval <- aggregate(steps ~ interval, df, mean, na.rm = TRUE)
plot(steps_by_interval, type = "l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps_by_interval$interval[which.max(steps_by_interval$steps)]
```

```
## [1] 835
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(df$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset.The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
In order to fill in the NA value, I will use the mean of steps for that particular 5-minute interval
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
newData <- df
totalRows = nrow(newData)
for (i in 1:totalRows) {
    if (is.na(newData[i, ]$steps)) {
        currentInterval <- newData[i, ]$interval
        newData[i, ]$steps <- steps_by_interval[steps_by_interval$interval == 
            currentInterval, ]$steps
    }
}
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
steps_by_day_NEW <- aggregate(steps ~ date, newData, sum, na.rm = TRUE)
hist(steps_by_day_NEW$steps)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r
mean(steps_by_day_NEW$steps)
```

```
## [1] 10766
```

```r
median(steps_by_day_NEW$steps)
```

```
## [1] 10766
```

5. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
The result are the same. Only slight difference in median.
## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
newData$TypeOfDay <- factor(ifelse(weekdays(as.Date(newData$date)) %in% c("Saturday", 
    "Sunday"), "weekend", "weekday"))
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
par(mfrow = c(2, 1))
StepsByWeekday <- aggregate(steps ~ interval, newData, subset = newData$TypeOfDay == 
    "weekday", FUN = mean)
StepsByWeekend <- aggregate(steps ~ interval, newData, subset = newData$TypeOfDay == 
    "weekend", FUN = mean)
plot(StepsByWeekday, type = "l")
plot(StepsByWeekend, type = "l")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

