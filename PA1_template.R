## ----loaddata------------------------------------------------------------
data <- read.csv("activity.csv")


## ------------------------------------------------------------------------
total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
hist(total.steps,breaks = 20,col = "black",xlab="total number of steps taken each day")
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)


## ------------------------------------------------------------------------
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
png(filename = "image-2.png")
with(averages, plot(
  x = interval,
  y = steps,
  type = "l",
  pch = 20,
  xlab = "5 minute interval",
  ylab = "average number of steps taken"
))

## ------------------------------------------------------------------------
averages[which.max(averages$steps),]


## ----how_many_missing Value----------------------------------------------------
missing <- is.na(data$steps)
# How many missing
table(missing)


## ------------------------------------------------------------------------
# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (averages[averages$interval==interval, "steps"])
  return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)


## ------------------------------------------------------------------------
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)

png(filename = "image-3.png")
hist(total.steps,breaks = 20,col = "black",xlab="total number of steps taken each day")
mean(total.steps)
median(total.steps)


## ------------------------------------------------------------------------
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


## ------------------------------------------------------------------------
library(ggplot2)
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
png(filename = "image-4.png")
ggplot(averages, aes(interval, steps, color = "red")) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
