##JohnHopkins University course in 
#Reproducible Research with Dr Roger Peng

#COURSE PROJECT 1
library(dplyr)
library(ggplot2)

#Load data
path <- getwd()
download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
              , destfile = paste(path, "dataFiles.zip", sep = "/"))
unzip(zipfile = "dataFiles.zip")

dir() #Check if data present

file.remove("dataFiles.zip")

#Import dataset
activity <- read.csv("activity.csv")

#Check dataset
head(activity)
str(activity)


# Question 1: What is mean total number of steps taken per day? ------------

steps_total <- activity %>%
  group_by(date) %>%
  summarise(steps_daily = sum(steps, na.rm = TRUE))


#Create barplot
p1 <- ggplot(steps_total, aes(steps_daily)) + 
      geom_histogram(binwidth = 2500, col = "tomato3", fill = "sandybrown") +
      geom_rug(aes(steps_daily)) +
      ggtitle("Total number of steps taken each day") +
      xlab("Steps") + 
      ylab("Frequency") +
      scale_y_continuous(breaks=seq(0,18,2)) +
      scale_x_continuous(breaks=seq(0,25000,2500)) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))

p1

#Safe file in working directory
ggsave(filename = "Plot1_Total number of steps taken each day.png", p1, dpi = 600, limitsize = TRUE)

#Calculate and report sum, mean & median
sum(steps_total$steps_daily, na.rm = TRUE)
summary(steps_total$steps_daily, na.rm = TRUE)


# Question 2: What is the average daily activity pattern? ----------------

#Create line plot for average steps per 5-minute intervals
steps_interval <- activity %>% 
  group_by(interval) %>%
  summarise(steps = mean(steps, na.rm =TRUE))


p2 <- ggplot(steps_interval, aes(interval, steps)) +
      geom_line(col="blue") +
      ggtitle("Average daily activity pattern") +
      xlab("5-minute intervals") +
      ylab("Average number of steps taken") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))

p2

#Safe file in working directory
ggsave(filename = "Plot2_Average daily activity pattern.png", p2, dpi = 600, limitsize = TRUE)

#Report max 5-minute interval
which.max(steps_interval$steps)
max_interval = round(steps_interval[104,])
max_interval

# Question 3: Imputing missing values -------------------------------------

##1 Total number of missing values
#Creating table
missing <- tbl_df(activity)
#Filtering for NA
missing %>% filter(is.na(steps)) %>% summarize(missing_values = n())


##2 Replacing missing values by average 5-minute intervals used in Q2
activity$steps_complete <- ifelse(is.na(activity$steps), 
  round(steps_interval$steps[match(activity$interval, steps_interval$interval)],0), 
  activity$steps)


##3 Create new dataset adjusted for NA values
activity_complete <- data.frame(steps=activity$steps_complete, 
  interval=activity$interval, date=activity$date)

head(activity_complete)

#Check if NA values still present
any(is.na(activity_complete))


##4 Create histogram with new dataset
activity_complete_steps <- aggregate(activity_complete$steps, list(activity_complete$date), FUN=sum)
colnames(activity_complete_steps) <- c("Date", "Steps")
head(activity_complete_steps)


p3 <- ggplot(activity_complete_steps, aes(Steps)) +
      geom_histogram(binwidth = 2500, col = "blue", fill = "skyblue1") +
      geom_rug(aes(Steps)) +
      ggtitle("Adjusted total number of steps taken each day") +
      xlab("Steps") + 
      ylab("Frequency") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))

p3

#Safe file in working directory
ggsave(filename = "Plot3_Adjusted total number of steps taken each day.png", p3, dpi = 600, limitsize = TRUE)

#Calculate and report sum, mean & median
sum(activity_complete_steps$Steps, na.rm = TRUE)
summary(activity_complete_steps$Steps, na.rm = TRUE)

#Do sum/mean/median values differ from Q1
sum(activity_complete_steps$Steps, na.rm = TRUE) - sum(steps_total$steps_daily, na.rm = TRUE)
summary(activity_complete_steps$Steps, na.rm = TRUE) - summary(steps_total$steps_daily, na.rm = TRUE)


# Question 4: Are there differences in activity patterns between weekdays and weekends? ---

#Make a panel plot containing a time series plot of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

## --> Use dataset activity_complete

#Change date format (if necessary)
activity_complete$date <- as.Date(activity_complete$date, format = "%Y-%m-%d")

#Add variable with the according weekdays name
activity_complete$Weekday <- weekdays(activity_complete$date)

#Distinguish between weekdays and weekend
activity_complete$Type <- ifelse(activity_complete$Weekday=='Saturday' | 
  activity_complete$Weekday=='Sunday', 'Weekend','Weekday')

#Check dataset
head(activity_complete)


#Plot two time series for weekdays & weekend

steps_week <- activity_complete %>% 
  group_by(interval, Type) %>%
  summarise(steps = mean(steps, na.rm =TRUE))

p4 <- ggplot(steps_week, aes(interval, steps)) +
      geom_line(col="black") +
      facet_wrap(~Type, nrow=2) +
      ggtitle("Weekday vs weekend activity pattern") +
      xlab("5-minute intervals") +
      ylab("Average number of steps taken") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))

p4

#Safe file in working directory
ggsave(filename = "Plot4_Weekday vs weekend activity pattern.png", p4, dpi = 600, limitsize = TRUE)

