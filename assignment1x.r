#read in dataset
data_all <- read.csv('activity.csv')

#remove all NA in data

data <- data_all[ with (data_all, { !(is.na(steps)) } ), ]

#histogram of the total number of steps per day
library(dplyr)
each_day<- group_by(data, date)
steps_per_day <- summarise(each_day, total = sum(steps))
hist(steps_per_day$total, main="Histogram of total number of steps taken per day", 
     xlab="Total steps in a day")

#get mean and median steps per day
summary(steps_per_day)

#answer:

#date        total      
#2012-10-02: 1   Min.   :   41  
#2012-10-03: 1   1st Qu.: 8841  
#2012-10-04: 1   Median :10765  
#2012-10-05: 1   Mean   :10766  
#2012-10-06: 1   3rd Qu.:13294  
#2012-10-07: 1   Max.   :21194  
#(Other)   :47  

#time series plot of the average number of steps taken
#First we must preprocess the data
steps_per_interval <- aggregate(steps ~ interval, data, mean)
#Then we create the time series plot
plot(steps_per_interval$interval, steps_per_interval$steps, type='l', 
     main="Average number of steps over all days", xlab="Interval", 
     ylab="Average number of steps")

#find row with maximum number of steps
maximum_steps_row <- which.max(steps_per_interval$steps)

#find the five minute interval containing the maximum number of steps
steps_per_interval[maximum_steps_row,] 

#answer
#interval    steps
#104      835 206.1698

#find the number of rows including NA's
sum(is.na(data_all))
#answer
#[1] 2304

#I chose to use the imputatiom strategy of replacing NA with the mean
#for the five minute interval
data_imputed <- data_all
for (i in 1:nrow(data_imputed)) {
  if (is.na(data_imputed$steps[i])) {
    interval_amount <- data_imputed$interval[i]
    steps_amount <- steps_per_interval[
      steps_per_interval$interval == interval_amount,]
    data_imputed$steps[i] <- steps_amount$steps
  }
}


#compute total number of steps taken each day
data_imputed_steps_per_day <- aggregate(steps ~ date, data_imputed, sum)
head(data_imputed_steps_per_day)

#answer
#date    steps
#1 2012-10-01 10766.19
#2 2012-10-02   126.00
#3 2012-10-03 11352.00
#4 2012-10-04 12116.00
#5 2012-10-05 13294.00
#6 2012-10-06 15420.00

#Make histogram of total number of steps each day with NA's filled in.
hist(data_imputed_steps_per_day$steps, main="Histogram of total number of steps taken per day-imputed", 
     xlab="Total steps in a day")

#calculate the mean and median of the imputed data
summary(data_imputed_steps_per_day$steps)

#answer:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#41    9819   10770   10770   12810   21190 

#Do the imputed mean and median differ from the previous earlier computations?
mean(steps_per_day$total)
# answer: 1] 10766.19
median(steps_per_day$total)
# answer:[1] 10765

# answer: the mean is the same, the median differs.

#create a new factor variable in the dataset with two levels-weekday
#and weekend

data_imputed_steps_per_day$day = ifelse(as.POSIXlt(as.Date(data_imputed_steps_per_day$date))$wday%%6 == 
    0, "weekend", "weekday")

#make a panel plot containing a time series plot of the 5-min interval (x-axis)
#and the average number of steps taken, averaged across all weekday days
#or weekend days (y-axis)

steps_new_interval = aggregate(steps ~ interval + day, steps_per_
interval, mean)
library(lattice)
xyplot(steps ~ interval | factor(day), data = steps_new_interval, aspect = 1/2, 
    type = "l")

data_imputed_steps_per_day$day = factor(data_imputed_steps_per_day$day, levels = c("weekday", "weekend"))