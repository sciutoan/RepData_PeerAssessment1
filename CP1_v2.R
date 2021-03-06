#load ggplot
library(ggplot2)

#Read the data
data <- read.csv("activity.csv")

#Calculate the total number of steps taken per day
totalstepsperday <- tapply(data$steps, data$date, sum, na.rm=T)

#Make a histogram of the total number of steps taken each day
hist(totalstepsperday, main = "Frequency of Steps Per Day", xlab= "Steps Per Day", ylab="Frequency")

#Calculate and report the mean and median of the total number of steps taken per day
print(mean(totalstepsperday))
print(median(totalstepsperday))

#Make a time series plot (type="l") of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all days (y-axis)
data1 <- data[!is.na(data$steps),]
meansteps <- tapply(data1$steps, data1$interval, mean)


interval <- levels(as.factor(data1$interval))
plot(interval, meansteps, type = "l", main = "Average Number of Steps Taken Over Time", xlab = "Interval", ylab = "Mean Steps")

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
table <- data.frame(meansteps, interval)
table[table$meansteps==max(table$meansteps),][2]

#Calculate and report the total number of missing values in the dataset 
#(i.e. the total number of rows NAs)
missingsteps <- data[is.na(data$steps),]
print(nrow(missingsteps))

#Devise a strategy for filling in all of the missing values in the dataset
#Create a new dataset that is equal to the original dataset but with the missing data filled in.
missingsteps$steps <- meansteps
imputeddata <- rbind(data1, missingsteps)
imputeddata <- imputeddata[order(imputeddata$date),]

#Make a histogram of the total number of steps taken each day and Calculate 
#and report the mean and median total number of steps taken per day. 
#Do these values differ from the estimates from the first part of the assignment? 
#What is the impact of imputing missing data on the estimates of the total daily number of steps?
totalstepsperdayimputed <- tapply(imputeddata$steps, imputeddata$date, sum)
hist(totalstepsperdayimputed, main= "Frequency of Steps Per Day (Imputed)", xlab= "Steps Per Day", ylab="Frequency")
print(mean(totalstepsperdayimputed))
print(median(totalstepsperdayimputed))
summary(totalstepsperday)
summary(totalstepsperdayimputed)

#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
#indicating whether a given date is a weekday or weekend day
imputeddata$day <- weekdays(as.Date(imputeddata$date))
weekend_feature <- grep("Saturday|Sunday", imputeddata$day, ignore.case = T)
weekenddata <- imputeddata[weekend_feature,]
weekenddata$weekday <- "weekend"
weekdaydata <- subset(imputeddata,imputeddata$day!=weekend_feature)
weekdaydata$weekday <- "weekday"
mergeddata <- rbind(weekdaydata, weekenddata)

#Make a panel plot containing a time series plot (type="l") 
#of the 5-minute interval (x-axis) and the average number of steps taken, 
#averaged across all weekday days or weekend days (y-axis)

meansteps <- aggregate(steps~ interval+weekday, mergeddata, mean)
graph <- qplot(interval, steps, data = meansteps, facets = weekday~.)
graph + geom_line(size = 0.75) + ylab("Mean Steps") + ggtitle("Average Number of Steps Taken")

