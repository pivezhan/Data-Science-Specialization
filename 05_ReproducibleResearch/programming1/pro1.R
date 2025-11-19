setwd(file.path("D:", "sbu", "RLearning", "reproducible research","programming1"))

# Loading and preprocessing the data
#### 1- Load the data (i.e. read.csv())
activity<-read.csv("activity.csv")
#### 2- Process/transform the data (if necessary) into a format suitable for your analysis
library(dplyr)
Ractivity <-tbl_df(activity)
rm(activity)

# What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset

#### 1- Calculate the total number of steps taken per day
by_date<-group_by(Ractivity[!is.na(Ractivity$steps),],date)
total_steps<-summarize(by_date, TotalStepsPerDay=sum(steps))

#### 2- If you do not understand the difference between a histogram and a barplot, 
#### research the difference between them. 
#### Make a histogram of the total number of steps taken each day
barplot(total_steps$TotalStepsPerDay,xlab="Dates",ylab="Total Steps in Each Dates")
dev.copy(png, file = "barplot.png" ) ## Copy my plot to a PNG file
dev.off() ## closing the PNG device!

hist(total_steps$TotalStepsPerDay,30,col = "blue",
     xlab="Total Steps Per One Day",ylab="Number of Dates",main="Number of Steps Per Dates")
dev.copy(png, file = "hist.png" ) ## Copy my plot to a PNG file
dev.off() ## closing the PNG device!

#### 3- Calculate and report the mean and median of the total number of steps taken per day
MeanMedianSteps<-summarize(by_date,MeanStepsPerDay=mean(steps),
                           MedianStepsPerDay=median(steps))

# What is the average daily activity pattern?
#### 1- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#### and the average number of steps taken, averaged across all days (y-axis)
by_interval<-group_by(Ractivity[!is.na(Ractivity$steps),],interval)
intervalsteps<-summarize(by_interval,AverageSteps=mean(steps))
plot(intervalsteps$interval,intervalsteps$AverageSteps,type="l",xlab="intervals",
     ylab="steps")
dev.copy(png, file = "intervalsteps.png" ) ## Copy my plot to a PNG file
dev.off() ## closing the PNG device!

#### 2- Which 5-minute interval, on average across all the days in the dataset, 
#### contains the maximum number of steps?
intervalsteps[max(intervalsteps$AverageSteps)==intervalsteps$AverageSteps,]

# Imputing missing values
# Note that there are a number of days/intervals where there are missing values 
# (coded as NA). The presence of missing days may introduce bias into some 
# calculations or summaries of the data.
#### 1- Calculate and report the total number of missing values in the 
#### dataset (i.e. the total number of rows with NAs)
sum(is.na(Ractivity))

#### 2- Devise a strategy for filling in all of the missing values in the dataset. 
#### The strategy does not need to be sophisticated. For example, you could use the 
#### mean/median for that day, or the mean for that 5-minute interval, etc.

#### Imputing by Average
# average by date
menactivity<- Ractivity;
mean_by_date<-group_by(Ractivity[!is.na(Ractivity$steps),],date)
mean_DateSteps<-summarize(mean_by_date,MeanStepsPerDay=mean(steps))
# average by interval
mean_by_interval<-group_by(Ractivity[!is.na(Ractivity$steps),],interval)
mean_IntervalSteps<-summarize(mean_by_interval,MeanStepsPerInterval=mean(steps))
for (i in 1:nrow(Ractivity)){
        if (is.na(Ractivity$steps[i])){
                if (sum(Ractivity$date[i]==mean_DateSteps$date)!=0){
                        menactivity$steps[i] <- mean_DateSteps$MeanStepsPerDay[(Ractivity$date[i]==mean_DateSteps$date)]     
                        #### Imputing Strategy Base On Date Steps         
                }else{
                        menactivity$steps[i] <- mean_IntervalSteps$MeanStepsPerInterval[(Ractivity$interval[i]==mean_IntervalSteps$interval)]     
                        #### Imputing Strategy Base On 5 Min Intervals
                }
        }else{
                menactivity$steps[i] <-  Ractivity$steps[i] 
        }
}
head(menactivity,10)

#### Imputing by median
# median by date
medactivity<- Ractivity;
median_by_date <- group_by(Ractivity[!is.na(Ractivity$steps),],date)
median_DateSteps <- summarize(median_by_date,MedianStepsPerDay=median(steps))
# median by interval
median_by_interval<-group_by(Ractivity[!is.na(Ractivity$steps),],interval)
median_IntervalSteps<-summarize(median_by_interval,MedianStepsPerInterval=median(steps))
for (i in 1:nrow(Ractivity)){
        if (is.na(Ractivity$steps[i])){
                if (sum(Ractivity$date[i]==median_DateSteps$date)!=0){
                        medactivity$steps[i] <- median_DateSteps$MedianStepsPerDay[(Ractivity$date[i]==median_DateSteps$date)]     
                        #### Imputing Strategy Base On Date Steps         
                }else{
                        medactivity$steps[i] <-median_IntervalSteps$MedianStepsPerInterval[(Ractivity$interval[i]==median_IntervalSteps$interval)]     
                        #### Imputing Strategy Base On 5 Min Intervals
                }
        }else{
                medactivity$steps[i] <-  Ractivity$steps[i] 
        }
}
head(medactivity,10)

#### 3- Create a new dataset that is equal to the original 
#### dataset but with the missing data filled in.
write.table(menactivity,file="menactivity.csv", row.name=F,col.names=T)
write.table(medactivity,file="medactivity.csv", row.name=F,col.names=T)

#### 4- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

#### Do these values differ from the estimates from the first part of the assignment? 

#### What is the impact of imputing missing data on the estimates of the total daily number of steps?

#### Survey By Mean
mean_by_date<-group_by(menactivity,date)
mean_total_steps<-summarize(mean_by_date, TotalStepsPerDay=sum(steps))
mean_by_interval<-group_by(menactivity[!is.na(menactivity$steps),],interval)
mean_intervalsteps<-summarize(mean_by_interval,AverageSteps=mean(steps))

#### Survey By Median
median_by_date<-group_by(medactivity,date)
median_total_steps<-summarize(median_by_date, TotalStepsPerDay=sum(steps))
median_by_interval<-group_by(medactivity[!is.na(medactivity$steps),],interval)
median_intervalsteps<-summarize(median_by_interval,AverageSteps=mean(steps))

##### Histogram Plot
par(mfrow=c(3,1),mar=c(4,4,2,1),oma=c(0,0,2,0))
with(Ractivity,{
        hist(total_steps$TotalStepsPerDay,30,col = "blue",
             xlab="Total Steps Per One Day",ylab="Number of Dates",
             main="Number of Steps Per Dates Without NA VAlues")
        hist(mean_total_steps$TotalStepsPerDay,30,col = "blue",
        xlab="Total Steps Per One Day",ylab="Number of Dates",
        main="Number of Steps Per Dates with Mean Imputing")
        hist(median_total_steps$TotalStepsPerDay,30,col = "blue",
        xlab="Total Steps Per One Day",ylab="Number of Dates", 
        main="Number of Steps Per Dates with Median Imputing")
})
dev.copy(png, file = "HistogramwithPlot.png" ) ## Copy my plot to a PNG file
dev.off() ## closing the PNG device!

# time series plot 
par(mfrow=c(3,1),mar=c(4,4,2,1),oma=c(0,0,2,0))
with(Ractivity,{
        plot(intervalsteps$interval,intervalsteps$AverageSteps,type="l",
             main="Time Series Plot Without NA Values",
             xlab="intervals",ylab="steps")
        plot(mean_intervalsteps$interval,mean_intervalsteps$AverageSteps,type="l",
             main="Time Series Plot With Mean Imputing 5 min Intervals",
             xlab="intervals",ylab="steps")
        plot(median_intervalsteps$interval,median_intervalsteps$AverageSteps,type="l",
             xlab="intervals", ylab="steps", 
             main="Time Series Plot With Median Imputing 5 min Intervals")
})
dev.copy(png, file = "intervalstepswithPlot.png" ) ## Copy my plot to a PNG file
dev.off() ## closing the PNG device!

## Are there differences in activity patterns between weekdays and weekends? 

## For this part the weekdays() function may be of some help here. 

## Use the dataset with the filled-in missing values for this part.

#### 1- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
library(dplyr);library(lubridate)
weekend<-c("Saturday","Sunday")
week<-rep(0,nrow(menactivity))
for (i in 1:nrow(menactivity)){
        if (sum(weekdays(ymd(menactivity$date[i]))==weekend)!=0){
                week[i] <- "weekend"
        }else{
                week[i] <- "weekday"
        }
}                
newactivity<-mutate(menactivity,weekdays=factor(week))

#### 2- Make a panel plot containing a time series plot (i.e. type = "l") of 
#### the 5-minute interval (x-axis) and the average number of steps taken, 
#### averaged across all weekday days or weekend days (y-axis). 
#### See the README file in the GitHub repository to see an example of what
#### this plot should look like using simulated data.
weekendactivity<-newactivity[newactivity$weekdays=="weekend",]
weekdayactivity<-newactivity[newactivity$weekdays=="weekday",]

weekendgroup<-group_by(weekendactivity,interval)
WeekendMean<-summarize(weekendgroup,MeanPerInterval=mean(steps))

weekdaygroup<-group_by(weekdayactivity,interval)
WeekdayMean<-summarize(weekdaygroup,MeanPerInterval=mean(steps))

par(mfrow=c(2,1),mar=c(4,4,2,1),oma=c(0,0,2,0))
with(newactivity,{
        plot(WeekendMean$interval,WeekendMean$MeanPerInterval,type="l",
             xlab="Intervals",ylab="Steps",main="Weekend")
        plot(WeekdayMean$interval,WeekdayMean$MeanPerInterval,type="l",
             xlab="Intervals",ylab="Steps",main="Weekday")        
})
dev.copy(png, file = "Weekday.png" ) ## Copy my plot to a PNG file
dev.off() ## closing the PNG device!

library(knitr)
knit2html("PA1_template.Rmd")
browseURL("PA1_template.html")
C:\Users\lenovo\AppData\Local\Temp\RtmpGueUWD