
setwd("C:/Users/Mike/Documents/testdir/Repdata")
StepData <- read.csv(file="activity.csv")
StepData$date <- as.Date(StepData$date)
StepsDaily <- aggregate(steps ~date, StepData, sum)
plot(StepsDaily$date, StepsDaily$steps, type="h", col="green", lwd=10)
StepMean <- mean(StepsDaily$steps)
paste("Mean Steps per day is ",StepMean)
StepMedian <- median(StepsDaily$steps)
paste("Median Steps per day is ",StepMedian)


StepsInterval <- aggregate(steps ~interval, StepData, mean)
StepIntMean <- mean(StepsInterval$steps)
plot(StepsInterval$interval, StepsInterval$steps, type="b", col="green",
     main="Steps per Interval", xlab="Interval", ylab="Steps")
abline(h=StepIntMean, col="blue")

StepDataNoNA <- StepData
NAlist=which(is.na(StepData$steps))
StepDataNoNA[NAlist,"steps"] <- StepIntMean

StepsDaily <- aggregate(steps ~date, StepDataNoNA, sum)
plot(StepsDaily$date, StepsDaily$steps, type="h", col="green", lwd=10)
StepMean <- mean(StepsDaily$steps)
paste("Mean Steps per day is ",StepMean)
StepMedian <- median(StepsDaily$steps)
paste("Median Steps per day is ",StepMedian)
abline(h=StepMedian, col="blue")

for (i in 1:nrow(StepDataNoNA)) {
  if (weekdays(StepDataNoNA$date[i]) =="Saturday"  | weekdays(StepDataNoNA$date[i]) =="Sunday" ){
    StepDataNoNA$DayType[i] = "weekend"
  } else {
    StepDataNoNA$DayType[i] = "weekday"
  }
}
StepDataNoNAWeekDay<-subset(StepDataNoNA, DayType == "weekday")
StepDataNoNAWeekEnd<-subset(StepDataNoNA, DayType == "weekend")

StepsInterval <- aggregate(steps ~interval, StepDataNoNAWeekDay, mean)
StepIntMean <- mean(StepsInterval$steps)
plot(StepsInterval$interval, StepsInterval$steps, type="b", col="green",
     main="Steps per Interval WeekDay", xlab="Interval", ylab="Steps")
abline(h=StepIntMean, col="blue")

StepsInterval <- aggregate(steps ~interval, StepDataNoNAWeekEnd, mean)
StepIntMean <- mean(StepsInterval$steps)
plot(StepsInterval$interval, StepsInterval$steps, type="b", col="green",
     main="Steps per Interval WeekEnd", xlab="Interval", ylab="Steps")
abline(h=StepIntMean, col="blue")

