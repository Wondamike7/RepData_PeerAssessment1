echo = TRUE
setwd("./Repositories/RepData_PeerAssessment1")
acty <- read.csv("activity.csv",header=TRUE,sep=",",colClasses=c("integer","Date","integer"))
str(acty)
dim(acty)
## 17568    3
## comment on read.csv that I bring in the date field as "Date"
library(dplyr)
by_day <- group_by(acty, date)
tot_by_day <- summarise(by_day, tot_steps = sum(steps, na.rm=TRUE))
hist(tot_by_day$tot_steps)
summary(tot_by_day)
## total steps per day: median is 10,395, mean is 9,354
## 61 days of observations
## need to clean up the histogram, with titles. Maybe add vertical line at median and mean?
## Time series plot is next
avg_by_int <- acty %>% group_by(interval) %>% summarise(mean_steps = mean(steps,na.rm=TRUE))
plot(avg_by_int$interval, avg_by_int$mean_steps, type="l")
## need to determine how to save the figure for knitr
## also need to clean up plot with title, maybe color the line
## do I want to note on graph where the max value is?
subset(avg_by_int, mean_steps==max(mean_steps))
## interval 835 has max average steps of 206.1698
## next step is imputing
sum(is.na(acty$steps))
## 2304 (can also get from summary(acty))
## strategy is to use the mean for that time interval. The strategies that use day will fail because some days (like the first) have exactly zero observations
acty_impute <- acty %>% group_by(interval) %>% mutate(mean_steps=mean(steps,na.rm=TRUE),imputed_steps = steps)
acty_impute$imputed_steps[is.na(acty_impute$steps)] <- acty_impute$mean_steps[is.na(acty_impute$steps)]
## this imputed the missing values using the mean for the given interval
summary(acty_impute)
sum(is.na(acty_impute$imputed_steps))
## 0
tot_by_day_imp <- acty_impute %>% group_by(date) %>% summarise(tot_steps = sum(imputed_steps))
hist(tot_by_day_imp$tot_steps)
summary(tot_by_day_imp)
## median and mean are both 10766 now
## weekend activity vs. weekday activity
## messed up, should've been using filled in missing values data
acty_impute$weekend <- factor(weekdays(acty$date)=="Saturday" | weekdays(acty$date)=="Sunday")
table(acty_impute$weekend)
new_avg <- acty_impute %>% group_by(interval, weekend) %>% summarise(mean_steps = mean(imputed_steps))
par(mfrow=c(2,1))
with(subset(new_avg,weekend=="TRUE"),plot(interval,mean_steps, type="l"))
with(subset(new_avg,weekend=="FALSE"),plot(interval,mean_steps, type="l"))
weekday <- subset(new_avg, weekend=="FALSE")
weekend <- subset(new_avg, weekend=="TRUE")
subset(weekday, mean_steps==max(mean_steps))
## still 835, mean now 230.3782
subset(weekend, mean_steps==max(mean_steps))
## now 915 max interval, with mean of 166.6392
## consider using lattice or ggplot to make nicer panel plots
savehistory("~/Coursera/Repositories/RepData_PeerAssessment1/project_complete(need to clean).R")
