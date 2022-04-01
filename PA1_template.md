---
title: "Reproducible Research"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
df <- read.csv("activity.csv", header=T, quote="\"", sep=",")


## What is mean total number of steps taken per day?
totalStepsByDate <- aggregate(steps ~ date, df, sum)
hist(totalStepsByDate$steps, main = paste("TOTAL STEPS EACH DAY"), col="red", xlab="Number of Steps")
dailyMean <- mean(totalStepsByDate$steps)
print(paste("Daily Mean",dailyMean))
dailyMedian <- median(totalStepsByDate$steps)
print(paste("Daily Median",dailyMedian))

## What is the average daily activity pattern?
stepsByInterval <- aggregate(steps ~ interval, df, mean)
plot(stepsByInterval$interval,stepsByInterval$steps, type="l", xlab="Daily Interval", ylab="Steps",main="AVERAGE STEPS /DAY /INTERVAL")

max_interval <- stepsByInterval[which.max(stepsByInterval$steps),1]
print(max_interval)

## Imputing missing values
naCount <- sum(!complete.cases(df))
print(naCount)

imputedData <- transform(df, steps = ifelse(is.na(df$steps), stepsByInterval$steps[match(df$interval, stepsByInterval$interval)], df$steps))
imputedData[as.character(imputedData$date) == "2012-10-01", 1] <- 0
imputedStepsByDate <- aggregate(steps ~ date, imputedData, sum)
hist(imputedStepsByDate$steps, main = paste("TOTAL STEPS EACH DAY"), col=rgb(0,1,0), xlab="Number of Steps")
hist(totalStepsByDate$steps, main = paste("TOTAL STEPS EACH DAY"), col=rgb(1,1,0), xlab="Number of Steps", add=TRUE)
legend("topright", c("Imputed", "Original"), col=c("green", "red"), lwd=8)

imputedMean <- mean(totalStepsByDate$steps)
print(imputedMean)

imputedMedian <- median(totalStepsByDate$steps)
print(imputedMedian)

## Are there differences in activity patterns between weekdays and weekends?
library(lattice)
imputedData$dayFactor <- as.factor(ifelse(weekdays(as.Date(imputedData$date))=="Saturday" | weekdays(as.Date(imputedData$date))=="Sunday", "weekend", "weekday"))
aggregateImputedData <- aggregate(steps~interval+dayFactor, imputedData, mean)
xyplot(steps~interval|dayFactor, data=aggregateImputedData, main="INTERVAL AVERAGEs(WEEKDAY v/s WEEKEND)", xlab="Interval", layout=c(1,2), type="l")
