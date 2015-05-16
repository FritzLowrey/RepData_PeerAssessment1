library(ggplot2)
library(knitr)
library(data.table)
library(dplyr)

weekdays_steps <- function(data) {
  weekdays_steps <- aggregate(data$steps, by=list(interval = data$interval), FUN=mean, na.rm=T)
  # convert to integers for plotting
  colnames(weekdays_steps) <- c("interval", "steps")
  weekdays_steps
}

activity <- read.csv("activity.csv", header=TRUE)
## massage the date column into a numeric value
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
activity$interval <- as.numeric(activity$interval)

intervals <- aggregate(activity$steps, by=list(interval = activity$interval), FUN=mean, na.rm=TRUE)
colnames(intervals) <- c("interval", "steps") # useful to keep naming conventions straight

new_activity <- activity
for(v in 1:length(new_activity$steps))
{
  if(is.na(new_activity[v,]$steps))
  {
    new_activity[v,]$steps <- intervals[intervals$interval == new_activity[v,]$interval, ]$steps
  }
}


new_totals <- aggregate(steps ~ date, new_activity, sum)
colnames(new_totals) <- c("date","steps")

ggplot(new_totals, aes(x = steps)) + 
  geom_histogram(fill = "blue", binwidth = 1000) + 
  labs(title="Histogram of Steps Taken per Day", 
       x = "Number of Steps per Day", y = "Number of times in a day(Count)") + theme_bw() 

new_activity$weekday <- as.factor(weekdays(new_activity$date))

weekend_vals <- subset(new_activity, weekday %in% c("Saturday","Sunday"))
weekday_vals <- subset(new_activity, !weekday %in% c("Saturday","Sunday"))

weekend_steps <- weekdays_steps(weekend_vals)
weekday_steps <- weekdays_steps(weekday_vals)

weekend_steps$dayofweek <- rep("weekend", nrow(weekend_steps))
weekday_steps$dayofweek <- rep("weekday", nrow(weekday_steps))

data_by_weekdays <- rbind(weekend_steps, weekday_steps)
data_by_weekdays$dayofweek <- as.factor(data_by_weekdays$dayofweek)
data_by_weekdays

ggplot(data_by_weekdays, aes(x=interval, y=steps)) + 
  geom_line(color="violet") + 
  facet_wrap(~ dayofweek, nrow=2, ncol=1) +
  labs(x="Interval", y="Number of steps") +
  theme_bw()