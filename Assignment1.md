---
output: word_document
---


```{r, echo=FALSE, results='hide'}
setwd("C:/Users/JigsawAM003/Documents/Reproducible Research/repdata_data_activity")
```


## Loading and preprocessing the data

* Loading the data.

```{r}
Data.in <- read.csv("activity.csv")
```

* Loading the required packages

```{r}
library(plyr)
library(ggplot2)
```

* Changing the date to the right format

```{r}
Data.in$date <- as.Date(Data.in$date, "%Y-%m-%d")
```


## Mean Total number of steps taken per day.

* Calculate the total number of steps taken per day

```{r}
StepsPerDay <- ddply(Data.in, .(date), summarize, StepsDay = sum(steps, na.rm=T))
print(StepsPerDay)
```

* Histogram of the Total Steps taken per day

```{r}
with(StepsPerDay, hist(StepsDay))
```

* Calculate and report the mean and median of the total number of steps taken per day

```{r}
mean(StepsPerDay$StepsDay, na.rm=T)
median(StepsPerDay$StepsDay, na.rm = T)
```


## What is the average daily activity pattern?

* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
AveStpInt <- ddply(Data.in, .(interval), summarise, AveSteps = mean(steps, na.rm=T))

with(AveStpInt, plot(interval, AveSteps, type = "l" ))

```

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
AveStpInt$interval[which.max(AveStpInt$AveSteps)]
```


## Imputing missing values

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
sapply(as.data.frame(sapply(Data.in, is.na)), sum)
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```{r}
ImputMisssing <- function(x){
  Index <-  which(is.na(x$steps))
  for(i in 1:length(Index)){
    x$steps[Index[i]]<- AveStpInt$AveSteps[AveStpInt$interval == x$interval[Index[i]]]
  }
  return(x)
}
```

* Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
Data.cleaned <- ImputMisssing(Data.in)
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

1) Calculate the total number of steps taken per day

```{r}
StepsPerDayCleaned <- ddply(Data.cleaned, .(date), summarize, StepsDay = sum(steps, na.rm=T))
print(StepsPerDayCleaned)
```

2) Histogram of the Total Steps taken per day

```{r}
with(StepsPerDayCleaned, hist(StepsDay))
```

3) Calculate and report the mean and median of the total number of steps taken per day

```{r}
mean(StepsPerDayCleaned$StepsDay, na.rm=T)
median(StepsPerDayCleaned$StepsDay, na.rm = T)
```

4) The Difference between the Mean and the medians compared with the previous ones are

```{r}
mean(StepsPerDayCleaned$StepsDay, na.rm=T) - mean(StepsPerDay$StepsDay, na.rm=T)
median(StepsPerDayCleaned$StepsDay, na.rm = T) - median(StepsPerDay$StepsDay, na.rm = T)
```

## Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r}
Data.cleaned$WeekDayOrEnd <- weekdays(Data.cleaned$date)
WeekendIndex <- (Data.cleaned$WeekDayOrEnd=="Saturday"|Data.cleaned$WeekDayOrEnd=="Sunday")
Data.cleaned$WeekDayOrEnd[WeekendIndex] <- "weekend"
Data.cleaned$WeekDayOrEnd[!WeekendIndex] <- "weekday"
Data.cleaned$WeekDayOrEnd <- as.factor(Data.cleaned$WeekDayOrEnd)
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r}
PlotData <- ddply(Data.cleaned, .(WeekDayOrEnd, interval), summarise, AvgSteps = mean(steps))
qplot(interval, AvgSteps, data = PlotData, facets = .~WeekDayOrEnd, geom = "line")
```

