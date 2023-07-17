

# Loading and pre-processing the data

# 1. Load the data
url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zip_file = "repdata_data_activity.zip"
if(!file.exists(zip_file)) download.file(url = url, destfile = zip_file)

file_name = "activity.csv"
if(!file.exists(file_name)) unzip(zipfile = zip_file)

df <- read.csv(file_name)
head(df, 10)
tail(df)
str(df)

# 2. Process/transform the data into a format suitable for your analysis
df$date <- as.Date(df$date, "%Y-%m-%d")
str(df)
summary(df)

#-------------------------------------------------------------------------------
# What is mean total number of steps taken per day? 
# For this part of the assignment, you can ignore the missing values in the dataset.

# 1. Calculate the total number of steps taken per day
total_daily_steps <- aggregate(steps ~ date, data = df, FUN = sum, na.rm = TRUE)

# 2. Make a histogram of the total number of steps taken each day
hist(total_daily_steps$steps, col="pink",
     xlab = "Steps per day", main = "Total Number of Steps per Day")

# 3. Calculate the mean and median of the total number of steps taken per day
mean(total_daily_steps$steps)
median(total_daily_steps$steps)

#-------------------------------------------------------------------------------
# What is the average daily activity pattern?

# 1. Make a time series plot (i.e. type = "l" ) of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
ave_interval_steps <- aggregate(steps ~ interval, data = df, FUN = mean, na.rm=TRUE)
# ave_interval_steps <- aggregate(x=ave_interval_steps$steps, by=list(ave_interval_steps$interval), FUN=mean)

plot(steps ~ interval, data = ave_interval_steps, type="l", col="blue",
     xlab="Time Interval", ylab = "Average Steps")

# 2. Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
#ave_interval_steps[ave_interval_steps$steps==max(ave_interval_steps$steps), ]
ave_interval_steps$interval[which.max(ave_interval_steps$steps)]

#-------------------------------------------------------------------------------
# Imputing missing values

# 1. Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)
colSums(is.na(df))
barplot(colSums(is.na(df)), ylim = c(0, 2500), 
        xlab = "Dataframe variables", ylab = "Number of missing values")

# 2. Devise a strategy for filling in all of the missing values in the dataset.
ave_interval_steps <- aggregate(steps ~ interval, data = df, FUN = mean, na.rm=TRUE)

# 3. Create a new dataset that is equal to the original dataset but with the 
# missing data filled in.
fill_na_df <- df
fill_na_df$steps[is.na(fill_na_df$steps)] <- ave_interval_steps[, 2]


# 4. Make a histogram of the total number of steps taken each day and Calculate 
# and report the mean and median total number of steps taken per day. Do these 
# values differ from the estimates from the first part of the assignment? What 
# is the impact of imputing missing data on the estimates of the total daily 
# number of steps?
total_daily_steps2 <- aggregate(steps ~ date, data = fill_na_df, FUN = sum)

par(mfrow = c(1, 2))
hist(total_daily_steps$steps, col="pink",
     xlab = "Steps per day",
     main = "Total Steps per Day (with NAs)")

hist(total_daily_steps2$steps, 
     xlab = "Steps per day", col="green",
     main = "Total Steps per Day (without NAs)")

mean(total_daily_steps2$steps)
median(total_daily_steps2$steps)

#-------------------------------------------------------------------------------
# Are there differences in activity patterns between weekdays and weekends?

#1. Create a new factor variable in the dataset with two levels – “weekday” 
# and “weekend” indicating whether a given date is a weekday or weekend day.
fill_na_df$day <- as.factor(ifelse(weekdays(fill_na_df$date)=="Saturday" |  
                                        weekdays(fill_na_df$date)=="Sunday", 
                                        "weekend", "weekday"))

# Make a panel plot containing a time series plot of the 5-minute interval 
# (x-axis) and the average number of steps taken, averaged across all weekday 
# days or weekend days (y-axis). 
library(ggplot2)
temp <- aggregate(steps ~ interval + day, data = fill_na_df, FUN = mean)

# qplot(x=interval, y=steps, data=temp, geom = "line", color=day, facets =day~.)
ggplot(temp, aes(interval, steps, color=day)) + geom_line() + facet_grid(day~.)











