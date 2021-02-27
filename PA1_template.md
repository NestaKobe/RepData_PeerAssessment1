---
title: 'Reproducible Research: Peer Assessment 1'
author: "Simon Coburg"
date: "26/02/2021"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

*This is my submission for the Reproducible Research Course Project 1. To obtain more information about this project see the [ReadMe](https://github.com/NestaKobe/RepData_PeerAssessment1/blob/master/README.md) on GitHub*.

## About

This assignment makes use of data from a personal activity monitoring device. The device collected data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012, and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

* **Dataset**: [Activity monitoring data [52K]](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA).
* **date**: The date on which the measurement was taken in YYYY-MM-DD format.
* **interval**: Identifier for the 5-minute interval in which measurement was taken.

The dataset is stored in a *comma-separated-value (CSV) file* and there are a total of *17,568 observations* in this dataset.

## Loading and preprocessing the data
The following steps were applied to load, import and check the dataset.

```{r presettings, loading data}
#Libraries
library(ggplot2)
library(dplyr)


#Load data
path <- getwd()
download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
              , destfile = paste(path, "dataFiles.zip", sep = "/"))
unzip(zipfile = "dataFiles.zip")

#Check if data present
dir() 

#Remove zip file
file.remove("dataFiles.zip")

#Import dataset
activity <- read.csv("activity.csv")

#Check dataset
head(activity)
str(activity)
```


## What is mean total number of steps taken per day?
For this part of the assignment, missing values in the dataset could be ignored.

1. Calculate the total number of steps taken per day.
2. Make a histogram of the total number of steps taken each day.
3. Calculate and report the mean and median of the total number of steps taken per day.

#### 1. Number of steps taken per day
```{r}
steps_total <- activity %>%
  group_by(date) %>%
  summarise(steps_daily = sum(steps, na.rm = TRUE))
head(steps_total)

#Calculate and report sum
sum(steps_total$steps_daily, na.rm = TRUE)

```
Total number of steps taken are *570,608*.

#### 2. Create histogram of total number of steps taken each day
```{r}
#Create barplot
ggplot(steps_total, aes(steps_daily)) + 
      geom_histogram(binwidth = 2500, col = "tomato3", fill = "sandybrown") +
      geom_rug(aes(steps_daily)) +
      ggtitle("Total number of steps taken each day") +
      xlab("Steps") + 
      ylab("Frequency") +
      scale_y_continuous(breaks=seq(0,18,2)) +
      scale_x_continuous(breaks=seq(0,25000,2500)) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
```

#### 3. Calculate mean and median of total number of steps taken per day

```{r}
summary(steps_total$steps_daily, na.rm = TRUE)
```


## What is the average daily activity pattern?

1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

#### 1. Time series of the averaged number of steps taken by 5-minute intervals
```{r}

#Create line plot for average steps per 5-minute intervals
steps_interval <- activity %>% 
  group_by(interval) %>%
  summarise(steps = mean(steps, na.rm =TRUE))


ggplot(steps_interval, aes(interval, steps)) +
      geom_line(col="blue") +
      ggtitle("Average daily activity pattern") +
      xlab("5-minute intervals") +
      ylab("Average number of steps taken") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
```
#### 2. Which 5-minute interval (averaged across all days) contains max. number of steps?

```{r}
#Report max 5-minute interval
which.max(steps_interval$steps)
max_interval = round(steps_interval[104,])
max_interval
```
The 5-minute interval that contains the maximum number of steps is the *835th interval*.

## Imputing missing values
There are a number of days/intervals where there are missing values (coded as \color{red}{\verb|NA|}NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values (NA) in the dataset.
2. Replace missing values (NA) in the dataset.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day.
5. Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

#### 1. Calculate total number of NA in the dataset
```{r}
#Creating table
missing <- tbl_df(activity)
#Filtering for NA
missing %>% filter(is.na(steps)) %>% summarize(missing_values = n())
```

#### 2. Replacing NA values by the averaged 5-minute intervals
```{r}
activity$steps_complete <- ifelse(is.na(activity$steps), 
  round(steps_interval$steps[match(activity$interval, steps_interval$interval)],0), 
  activity$steps)
```

#### 3. Create new dataset adjusted for NA values
```{r}
activity_complete <- data.frame(steps=activity$steps_complete, 
  interval=activity$interval, date=activity$date)

head(activity_complete)

#Check if NA values still present
any(is.na(activity_complete))
```

#### 4. Create histogram with new dataset
```{r}
activity_complete_steps <- aggregate(activity_complete$steps, list(activity_complete$date), FUN=sum)
colnames(activity_complete_steps) <- c("Date", "Steps")
head(activity_complete_steps)


ggplot(activity_complete_steps, aes(Steps)) +
      geom_histogram(binwidth = 2500, col = "blue", fill = "skyblue1") +
      geom_rug(aes(Steps)) +
      ggtitle("Adjusted total number of steps taken each day") +
      xlab("Steps") + 
      ylab("Frequency") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
```

#### 5. Calculate and report sum, mean & median
```{r}
sum(activity_complete_steps$Steps, na.rm = TRUE)
summary(activity_complete_steps$Steps, na.rm = TRUE)

#Do sum/mean/median values differ from Q1
sum(activity_complete_steps$Steps, na.rm = TRUE) - sum(steps_total$steps_daily, na.rm = TRUE)
summary(activity_complete_steps$Steps, na.rm = TRUE) - summary(steps_total$steps_daily, na.rm = TRUE)

```
The estimate of total number of steps increases by *86,096*. Mean and median differ by the amount of *1,411* and *367* respectively compared to the previous estimates that excluded the missing values.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

#### 1. Create a new factor variable with two levels: "weekday" and "weekend"
```{r}
#Change date format (if necessary)
activity_complete$date <- as.Date(activity_complete$date, format = "%Y-%m-%d")

#Add variable with the according weekdays name
activity_complete$Weekday <- weekdays(activity_complete$date)

#Distinguish between weekdays and weekend
activity_complete$Type <- ifelse(activity_complete$Weekday=='Saturday' | 
  activity_complete$Weekday=='Sunday', 'Weekend','Weekday')

#Check dataset
head(activity_complete)
```

#### 2. Plot two time series for weekdays & weekend
```{r}
steps_week <- activity_complete %>% 
  group_by(interval, Type) %>%
  summarise(steps = mean(steps, na.rm =TRUE))

ggplot(steps_week, aes(interval, steps)) +
      geom_line(col="black") +
      facet_wrap(~Type, nrow=2) +
      ggtitle("Weekday vs weekend activity pattern") +
      xlab("5-minute intervals") +
      ylab("Average number of steps taken") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
```

##### This concludes the assignment.
*This document, the R-script and graphs can be found under my [GitHub](https://github.com/NestaKobe/RepData_PeerAssessment1) domain.*
