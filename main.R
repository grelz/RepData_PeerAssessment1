
# Reproducible Research - Course Project 1
# This R script content code for:
#  - Reading in the dataset and/or processing the data
#  - Histogram of the total number of steps taken each day
#  - Mean and median number of steps taken each day
#  - Time series plot of the average number of steps taken
#  - The 5-minute interval that, on average, contains the maximum number of 
#    steps
#  - Code to describe and show a strategy for imputing missing data
#  - Histogram of the total number of steps taken each day after missing values
#    are imputed
#  - Panel plot comparing the average number of steps taken per 5-minute 
#    interval across weekdays and weekends


library(ggplot2)

# Set English locale. I do it because my locale is Russian_Russia.1251. 
tmp_time_locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "English")



## Loading and preprocessing the data

# Show any code that is needed to
# 1. Load the data (i.e. read.csv())

actvt <- read.csv("activity.csv")

# 2. Process/transform the data (if necessary) into a format suitable for your 
#    analysis

actvt$date <- as.Date(actvt$date,"%Y-%m-%d")


## What is mean total number of steps taken per day?

# For this part of the assignment, you can ignore the missing values in the 
# dataset.

# 1. Calculate the total number of steps taken per day
spd <- aggregate(actvt$step, list(date = actvt$date), sum, na.rm = TRUE)

# 2. Make a histogram of the total number of steps taken each day
plot <- ggplot(spd, aes(x = date, y = x)) + 
    geom_bar(stat = "identity") +
    labs(x = "Date") +
    labs(y = "Steps") +
    labs(title = "Histogram of the total number of steps taken each day")

#plot
ggsave("plot1.png", plot = plot, device = "png")

# 3. Calculate and report the mean and median of the total number of steps taken
#    per day
spd_mean <- mean(spd$x)
spd_median <- median(spd$x)
print(paste("Mean of the total number of steps taken per day:", 
            round(spd_mean, digits = 2)))
print(paste("Median of the total number of steps taken per day:", 
            round(spd_median, digits = 2)))


## What is the average daily activity pattern?

# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
#    and the average number of steps taken, averaged across all days (y-axis)
aspi <- aggregate(actvt$step, list(interval = actvt$interval), mean, na.rm = TRUE)

plot <- ggplot(aspi, aes(x = interval, y = x)) + 
    geom_line() +
    labs(x = "Interval") +
    labs(y = "Number of steps") +
    labs(title = "Time series plot of the average number of steps taken")

#plot
ggsave("plot2.png", plot = plot, device = "png")


# 2. Which 5-minute interval, on average across all the days in the dataset, 
#    contains the maximum number of steps?
max_steps_interval <- aspi[which.max(aspi$x), "interval"]
print(paste(
    "Interval contains the maximum number of steps on average across all the days: ",
    max_steps_interval))


## Imputing missing values

# 1. Calculate and report the total number of missing values in the dataset 
#    (i.e. the total number of rows with NAs)

na_count <- sum(is.na(actvt$steps))
print(paste("The total number of missing values in the dataset:", na_count))

# 2. Devise a strategy for filling in all of the missing values in the dataset. 
#    The strategy does not need to be sophisticated. For example, you could use 
#    mean/median for that day, or the mean for that 5-minute interval, etc.

#    I use the mean for that 5-minute interval across all the days for filling 
#    the missing values.

# 3. Create a new dataset that is equal to the original dataset but with the 
#    missing data filled in.

actvt_int <- split(actvt, actvt$interval)
actvt_int_fmv <- lapply(actvt_int, function (x) { x$steps[is.na(x$steps)] = mean(x$steps, na.rm=TRUE) ; x} )
actvt_fmv <- unsplit(actvt_int_fmv, actvt$interval)

# 4. Make a histogram of the total number of steps taken each day and Calculate
#    and report the mean and median total number of steps taken per day. 
#    Do these values differ from the estimates from the first part of the 
#    assignment? What is the impact of imputing missing data on the estimates of
#    the total daily number of steps?

spd_fmv <- aggregate(actvt_fmv$step, list(date = actvt_fmv$date), sum, 
                       na.rm = TRUE)

plot <- ggplot(spd_fmv, aes(x = date, y = x)) + 
    geom_bar(stat = "identity") +
    labs(x = "Date") +
    labs(y = "Steps") +
    labs(title = "Histogram of the total number of steps taken after missing values are imputed")

#plot
ggsave("plot3.png", plot = plot, device = "png")

spd_fmv_mean <- mean(spd_fmv$x)
spd_fmv_median <- median(spd_fmv$x)
print(paste("Mean of the total number of steps taken per day after imputing missing values:", 
            round(spd_fmv_mean, digits = 2)))
print(paste("Median of the total number of steps taken per day after imputing missing values:", 
            round(spd_fmv_median, digits = 2)))


## Are there differences in activity patterns between weekdays and weekends?

# Use the dataset with the filled-in missing values for this part.

# 1. Create a new factor variable in the dataset with two levels – “weekday” and
#    “weekend” indicating whether a given date is a weekday or weekend day.

actvt_fmv$pw <- as.factor(ifelse(weekdays(actvt_fmv$date) %in% 
                                        c("Saturday", "Sunday"),
                                       "Weekend", "Weekday"))

# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 
#    5-minute interval (x-axis) and the average number of steps taken, averaged 
#    across all weekday days or weekend days (y-axis). 

aspipw <- aggregate(actvt_fmv$step, 
               list(interval = actvt_fmv$interval, pw = actvt_fmv$pw), 
               mean, na.rm = TRUE)

plot <- ggplot(aspipw, aes(x = interval, y = x)) + 
    geom_line() +
    facet_grid(pw ~ .) +
    labs(x = "Interval") +
    labs(y = "Number of steps") +
    labs(title = "Comparing the average number of steps taken across weekdays and weekends")

#plot
ggsave("plot4.png", plot = plot, device = "png")



# Restore locale.
Sys.setlocale("LC_TIME", tmp_time_locale)










