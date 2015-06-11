echo = TRUE
setwd("./Repositories/RepData_PeerAssessment1")
getwd()
list.files()
unzip(activity.zip)
unzip("activity.zip")
list.files()
acty <- read.csv("activity.csv",header=TRUE,sep=",")
head(acty,3)
tail(acty,3)
summary(acty)
## want to sum the steps by day - dplyr?
library(dplyr)
?ddply
?plyr
?dplyr
browseVignettes(package="dplyr")
by_day <- group_by(acty, steps)
head(by_day,3)
str(by_day)
tot_by_day <- summarise(by_day, tot_steps = sum(steps, na.rm=TRUE))
tot_by_day
by_day <- group_by(acty, date)
tot_by_day <- summarise(by_day, tot_steps = sum(steps, na.rm=TRUE))
tot_by_day
hist(tot_by_day)
hist(tot_by_day$tot_steps)
summary_stats <- summarise(by_day, tot_steps = sum(steps, na.rm=TRUE),mean_steps = mean(steps, na.rm=TRUE), median_steps = median(steps,na.rm=TRUE))
summary_stats
by_interval <- group_by(acty,interval)
average_by_interval <- summarise(by_interval, mean_steps = mean(steps,na.rm=TRUE))
average_by_interval
plot(average_by_interval$interval, average_by_interval$mean_steps, type="l")
max_int <- summarise(average_by_interval, max = max(mean_steps, na.rm=TRUE))
max_int
average_by_interval[mean_steps==206.1698,]
average_by_interval[,mean_steps==206.1698]
average_by_interval[,mean_steps=206.1698]
average_by_interval[mean_steps=206.1698,]
average_by_interval[which(average_by_interval$mean_steps == 206.1698),]
average_by_interval[which(average_by_interval$mean_steps >= 206),]
subset(average_by_interval,mean_steps>=206)
acty_impute <- acty
acty_impute$imputed_steps = acty_impute$steps
str(acty_impute)
?replace
acty_impute$imputed_steps <- replace(acty_impute$steps, is.na(acty_impute$steps), average_by_interval$mean_steps)
str(acty_impute)
table(acty$steps, acty$interval)
table(acty$interval, sum(acty$steps, na.rm=TRUE))
summary(acty$steps)
summary(acty_impute$imputed_steps)
## maybe first add a mutated column of median by interval, then use that to impute
acty_impute2 <- acty
str(acty_impute2)
acty_impute2 %>% group_by(interval) %>% mutate(median_steps = median(steps, na.rm=TRUE))
summary(acty_impute2$median_steps)
table(acty_impute2$median_steps)
acty_impute2[445,]
test <- acty_impute2 %>% group_by(interval) %>% mutate(median_steps = median(steps, na.rm=TRUE))
test
str(test)
summary(test)
test <- acty_impute2 %>% group_by(interval) %>% mutate(median_steps = median(steps, na.rm=TRUE, mean_steps= mean(steps, na.rm=TRUE))
)
test <- acty_impute2 %>% group_by(interval) %>% mutate(median_steps = median(steps, na.rm=TRUE), mean_steps= mean(steps, na.rm=TRUE))
summary(test)
table(interval, mean_steps)
table(test$interval, test$mean_steps)
with(test, tapply(steps, list(interval), sum))
with(test, tapply(steps, list(interval), sum, na.rm=TRUE))
with(test, tapply(steps, list(interval), mean, na.rm=TRUE))
## that last tapply gave me what I created earlier with group_by
with(test, tapply(mean_steps, list(interval), mean))
head(test)
test <- mutate(test, imputed_steps = steps)
head(test)
?replace
test <- with(test, replace(imputed_steps, is.na(imputed_steps), mean_steps))
## warning that number of items to replace was not a multiple of replacement length...
head(test)
## result is just a vector now???
## don't think that's what I want
## maybe a function that looks for missing steps and replaces it with the mean
test2 <- test
head(test2)
