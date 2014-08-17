### Loading and preprocessing the data
1. Load the data (i.e. read.csv())

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
data <- read.csv("activity.csv")
```

### What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day

```r
temp1 = tapply(data$steps, data$date, sum, na.rm = TRUE)
hist(temp1, breaks = 20, main = "mean total number of steps", xlab = "")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

2. Calculate and report the mean and median total number of steps taken per day

```r
mean(temp1)
```

```
## [1] 9354
```

```r
median(temp1)
```

```
## [1] 10395
```



### What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken,

averaged across all days (y-axis)

```r
temp2 = tapply(data$steps, as.factor(data$interval),mean, na.rm = TRUE)
daily_activity = data.frame(average = temp2, interval = unique(data$interval), row.names = NULL)
with(daily_activity,  plot(interval,average, type="l" ))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
daily_activity[which.max(daily_activity$average),2]
```

```
## [1] 835
```



### Inputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
NA_rows <- is.na(data$steps)
sum(NA_rows)
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data2 = data




for (i in 1:nrow(data2)) {
        if(NA_rows[i] == TRUE){  # check NA values
                temp <- match(data2[i,3], daily_activity$interval) # substitute 5-minute interval mean for NAs
                data2[i,1] <- daily_activity[temp,1]   
        }
        
}
```
  
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
temp3 = tapply(data2$steps, data$date, sum, na.rm = TRUE)
hist(temp3, breaks = 20, main = "mean total number of steps", xlab = "")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r
mean(temp3)
```

```
## [1] 10766
```

```r
median(temp3)
```

```
## [1] 10766
```
All NA values has been disappeared. As a result, mean and median are increased. 



### Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels ¡°weekday¡± and ¡°weekend¡± indicating whether a given date is a weekday or weekend day.


```r
Sys.setlocale("LC_TIME", "English")
weekday_info = weekdays(as.Date(data2$date))
weekday_info <- gsub("Sunday|Saturday","weekend", weekday_info)
weekday_info <- gsub("Monday|Tuesday|Wednesday|Thursday|Friday","weekday", weekday_info)
data2$weekday <- as.factor(weekday_info)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
data2_weekday <- data2[data2$weekday == "weekday",]
data2_weekend <- data2[data2$weekday == "weekend",]

temp4 = tapply(data2_weekday$steps, as.factor(data2_weekday$interval),mean, na.rm = TRUE)
temp5 = tapply(data2_weekend$steps, as.factor(data2_weekend$interval),mean, na.rm = TRUE)

weekday_activity = data.frame(average = temp4,interval = unique(data2_weekday$interval), weekday = "weekday", row.names = NULL)
weekend_activity = data.frame(average = temp5,interval = unique(data2_weekend$interval), weekday = "weekend", row.names = NULL) 

activity2 <- rbind( weekday_activity, weekend_activity)
library(lattice)
xyplot(average ~ interval | weekday, data = activity2, type = "l", layout = c(1,2) )
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

