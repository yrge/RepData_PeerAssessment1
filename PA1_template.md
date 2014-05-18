# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv", header = T)
data$newdate <- strptime(data$date, "%Y-%m-%d")
```


## What is mean total number of steps taken per day?

```r
total_num <- tapply(data$steps, data$date, sum, na.rm = T)
hist(total_num)
```

![plot of chunk totalSteps](figure/totalSteps.png) 

```r

mean_d <- mean(total_num)
median_d <- median(total_num)
```

The mean total number of steps taken per day is 9354.2295. The median total number of steps taken per day is 10395.

## What is the average daily activity pattern?

```r
aver_interval <- tapply(data$steps, data$interval, mean, na.rm = T)
plot(as.numeric(names(aver_interval)), aver_interval, type = "l", xlab = "intervals", 
    ylab = "average steps")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

```r

x <- names(aver_interval)[which(aver_interval == max(aver_interval))]
```

The 835 interval contains the maximum number of steps.


## Imputing missing values

```r
# total number of missing values
num_na <- sum(is.na(data$steps))

sub_data <- subset(data, is.na(data$steps))
# replace NAs with the average value across 5-interval
new_data <- data  # create a new dataset
new_data$steps[is.na(new_data$steps)] = aver_interval[as.character(sub_data$interval)]

# Make a histogram of the total number of steps taken each day
new_total_num <- tapply(new_data$steps, new_data$date, sum)
hist(new_total_num)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
new_mean_d <- mean(new_total_num)
new_median_d <- median(new_total_num)
```

Total number of missing values in the dataset is 2304.
After replacing NA values to average value per 5-interval, the new mean is 1.0766 &times; 10<sup>4</sup> and the new median is 1.0766 &times; 10<sup>4</sup>.The values are different from the ones from first part of the assignment. The values of mean and median improve by replacing the NAs.

## Are there differences in activity patterns between weekdays and weekends?

```r
Sys.setlocale("LC_TIME", "en_US")
```

```
## [1] "en_US"
```

```r
# create a new factor variable in the dataset
wd <- as.character(weekdays(strptime(data$date, "%Y-%m-%d")))
l <- wd == "Saturday" | wd == "Sunday"
wd[l] = "weekend"
wd[!l] = "weekday"
wd <- factor(wd)
data$wd <- wd

# make a plot about the average number of steps with 5-min interval.
str(data)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ newdate : POSIXlt, format: "2012-10-01" "2012-10-01" ...
##  $ wd      : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

```r
data_weekend <- subset(data, wd == "weekend")
data_weekday <- subset(data, wd == "weekday")
weekend_aver_interval <- tapply(data_weekend$steps, data_weekend$interval, mean, 
    na.rm = T)
weekday_aver_interval <- tapply(data_weekday$steps, data_weekday$interval, mean, 
    na.rm = T)

par(mfcol = c(2, 1))
par(mar = c(4, 4, 2, 2))
plot(as.numeric(names(weekday_aver_interval)), weekday_aver_interval, type = "l", 
    xlab = "intervals", ylab = "average steps", main = "weekend")
plot(as.numeric(names(weekend_aver_interval)), weekend_aver_interval, type = "l", 
    xlab = "intervals", ylab = "average steps", main = "weekday")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

