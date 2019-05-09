#Load the data

activity <- read.csv("activity.csv")
View(activity)

#What is mean total number of steps taken per day?
library(dplyr)
library(ggplot2)
summary(activity)


#Cleaning and preparing the data for plotting
activitybydate <- select(activity, date, steps)
activitybydate <-group_by(activitybydate, date)
activitybydate <- na.omit(activitybydate)
activitybydate <- summarize(activitybydate, totalsteps = sum(steps))
View(activitybydate)

hist1<-hist(activitybydate$totalsteps, xlab = "Total daily Steps", 
     main = "Histogram of Total Steps by Date", breaks = 20)

#Calculating the mean and median
mean(activitybydate$totalsteps)
median(activitybydate$totalsteps)

#What is the average daily activity pattern?
fivemin <- aggregate(steps ~interval, data = activity, FUN = mean)
View(fivemin)
timeseries1 <- ggplot(data = fivemin, aes(x = interval, y = steps)) +
  geom_line() + 
  xlab("Time Intervals") +
  ylab("Total Number of Steps") +
  ggtitle("Average Number of Steps Taken of the 5-Minute Interval")

print(timeseries1)

#The 5-minute interval that, on average, contains the maximum number of steps
fivemin[which(fivemin$steps == max(fivemin$steps)),]

#Imputing missing Values

#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sapply(X = activity, FUN = function(x) sum(is.na(x)))

sum(is.na(activity))

#2. Devise a strategy for filling in all of the missing
#values in the data set. The strategy does not need to be sophisticated.
#For example. you could use the mean/median for that day,
#or the mean for that 5-minute interval, etc.

#Using the mean of the 5-minute interval
replacemean <- function(num) replace(num, is.na(num), mean(num, na.rm = TRUE))
meanday <- (activity %>% group_by(interval) %>% mutate(steps = replacemean(steps)))
head(meanday)

#check for is.na
sum(is.na(meanday))

#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
new_activity <- as.data.frame(meanday)
head(new_activity)

summary(new_activity)
summary(activity)

#4. 
# new_steps <- aggregate(new_activity$steps, by = list(new_activity$date), FUN = sum)
# names(new_steps)[names(new_steps) == "x"] <- "Total"
# names(new_steps)[names(new_steps) == "Group.1"] <- "Date"
# hist2 <- ggplot(data = new_steps, aes(Total)) + 
#   geom_histogram(binwidth = 1500, colour = "white") +
#   xlab("Total Number of Steps Taken Each Day") +
#   ylab("Count") +
#   ggtitle("Histogram of the Total Number of Steps Taken Each Day with New Version Dataset")
# print(hist2)
# 
# library(grid)
# library(gridExtra)
# 
# grid.arrange(hist1, hist2, ncol = 2)
#
newactivitybydate <- select(new_activity, date, steps)
newactivitybydate <-group_by(newactivitybydate, date)
newactivitybydate <- na.omit(newactivitybydate)
newactivitybydate <- summarize(newactivitybydate, totalsteps = sum(steps))
View(newactivitybydate)

hist2<-hist(newactivitybydate$totalsteps, xlab = "Total daily Steps", 
            main = "Histogram of Total Steps by Date", breaks = 20)

mean(activitybydate$totalsteps)
mean(newactivitybydate$totalsteps)
median(activitybydate$totalsteps)
median(newactivitybydate$totalsteps)

#Are there differences in activity patterns between weekdays and weekends?
new_activity$date <- as.Date(new_activity$date)
new_activity$weekday <- weekdays(new_activity$date)
new_activity$weekend <- ifelse(new_activity$weekday=="Saturday" | new_activity$weekday=="Sunday", "Weekend", "Weekday" )
View(new_activity)

library(ggplot2)
meandataweekendweekday <- aggregate(new_activity$steps , by= list(new_activity$weekend, new_activity$interval), na.omit(mean))
names(meandataweekendweekday) <- c("weekend", "interval", "steps")

ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
  facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
  ggtitle("Comparison of Average Number of Steps in Each Interval")