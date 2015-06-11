echo = TRUE
setwd("./Repositories/RepData_PeerAssessment1")
getwd()
acty <- read.csv("activity.csv",header=TRUE,sep=",")
View(acty)
head(acty,3)
tail(acty,3)
summary(acty)
dim(acty)
library(dplyr)
by_day <- group_by(acty, steps)
by_day <- group_by(acty, date)
tot_by_day <- summarise(by_day, tot_steps = sum(steps, na.rm=TRUE))
hist(tot_by_day$tot_steps)
summary_stats <- summarise(by_day, tot_steps = sum(steps, na.rm=TRUE),mean_steps = mean(steps, na.rm=TRUE), median_steps = median(steps,na.rm=TRUE))
by_interval <- group_by(acty,interval)
average_by_interval <- summarise(by_interval, mean_steps = mean(steps,na.rm=TRUE))
plot(average_by_interval$interval, average_by_interval$mean_steps, type="l")
average_by_interval[mean_steps==max(mean_steps),]
average_by_interval[mean_steps==max(average_by_interval$mean_steps),]
average_by_interval[mean_steps==max(average_by_interval$mean_steps, na.rm=TRUE),]
## error in lapply for each of prior three
max(average_by_interval$mean_steps)
str(average_by_interval)
subset(average_by_interval, mean_steps==max(mean_steps))
## bingo
sum(is.na(acty$steps)
)
## 2304
sum(is.na(acty$interval))
sum(is.na(acty$date))
## only missing values are in steps
## strategy is to use the mean for that time interval. The strategies that use day will fail because some days (like the first) have exactly zero observations
str(acty$date)
table(acty[date=="2012-10-01",]$interval)
table(acty[acty$date=="2012-10-01",]$interval)
table(acty[acty$date=="2012-10-01",]$steps)
table(acty[acty$date=="2012-10-02",]$steps)
table(acty[acty$date=="2012-10-02",]$steps, acty[acty$date=="2012-10-02",]$interval)
## ignore those tables
acty_impute <- acty
test <- acty_impute %>% group_by(interval) %>% mutate(median_steps = median(steps, na.rm=TRUE), mean_steps= mean(steps, na.rm=TRUE))
View(test)
str(test)
test$imputed_steps <- test$steps
test$imputed_steps[is.na(test$steps)] <- test$mean_steps
test$imputed_steps[is.na(test$steps)] <- test$mean_steps[is.na(test$steps)]
View(test)
test2<-test[is.na(test$steps),]
View(test2)
## test has what I want
hist(test$imputed_steps)
summary(test$imputed_steps, test$steps)
summary(test$imputed_steps)
summary(test$steps)
hist(test$steps)
?weekdays
weekdays(test$date)
## need to bring in date variable as date in the beginning, rather than converting it here
str(actcy)
str(acty)
new_acty <- read.csv("activity.csv",header=TRUE,sep=",",colclasses=c("int","Date","int"))
new_acty <- read.csv("activity.csv",header=TRUE,sep=",",colClasses=c("int","Date","int"))
new_acty <- read.csv("activity.csv",header=TRUE,sep=",",colClasses=c("integer","Date","integer"))
View(new_acty)
str(new_acty)
weekdays(new_acty$date)
new_acty$weekend <- factor(weekdays(new_acty$date)=="Sunday" | weekdays(new_acty$date)=="Saturday")
View(new_acty)
summary(new_acty)
str(new_acty)
levels(new_acty$weekend)[levels(new_acty$weekend=="TRUE")] <- "weekend"
levels(new_acty$weekend)[levels(new_acty$weekend=="FALSE")] <- "weekday"
View(new_acty)
str(new_acty)
levels(new_acty$weekend)
levels(new_acty$weekend)["FALSE"]<-"weekday"
levels(new_acty$weekend)
levels(new_acty$weekend)[levels(new_acty$weekend=="FALSE"]<-"weekday"
levels(new_acty$weekend)[levels(new_acty$weekend)=="FALSE"]<-"weekday"
levels(new_acty$weekend)
levels(new_acty$weekend)[levels(new_acty$weekend=="TRUE"]<-"weekend"
levels(new_acty$weekend)[levels(new_acty$weekend)=="TRUE"]<-"weekend"
levels(new_acty$weekend)
View(new_acty)
par(mfrow=c(2,1))
new_avg <- new_acty %>% group_by(interval, weekend) %>% summarise(mean_steps = mean(steps, na.rm=TRUE))
View(new_avg)
with(subset(new_avg,weekend=="weekend"),plot(interval,mean_steps, type="l"))
with(subset(new_avg,weekend=="weekday"),plot(interval,mean_steps, type="l"))
View(new_avg)
subset(new_avg, weekend=="weekend")
subset(new_avg, weekend=="weekday")
weekday <- subset(new_avg, weekend=="weekday")
weekend <- subset(new_avg, weekend=="weekend")
par(mfrow=c(2,1))
plot(weekday$interval, weekday$mean_steps, type="l")
plot(weekday$interval, weekday$mean_steps, type="l")
View(average_by_interval)
plot(average_by_interval$interval, average_by_interval$mean_steps, type="l")
with(subset(new_avg,weekend=="weekend"),plot(interval,mean_steps, type="l"))
with(subset(new_avg,weekend=="weekday"),plot(interval,mean_steps, type="l"))
par(mfrow=c(2,1))
with(subset(new_avg,weekend=="weekend"),plot(interval,mean_steps, type="l"))
with(subset(new_avg,weekend=="weekday"),plot(interval,mean_steps, type="l"))
savehistory("~/Coursera/Repositories/RepData_PeerAssessment1/project_scrap2.R")
