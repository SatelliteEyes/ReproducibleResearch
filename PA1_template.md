# Reproducible Research  
## Course Project 1  
Author: Caitlin Hart  
Date Started: February 9, 2015  
Date Submitted: February 15, 2015  

### Where to find the data  
The dataset, *Activity Monitoring Data* can be downloaded from the Course website:  
[here](https://class.coursera.org/repdata-011/human_grading/view/courses/973512/assessments/3/submissions)  
  
### Initial read-in to variable *activity*:  

```r
activity = read.csv("activity.csv")
```

### Getting a first look at the data:

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

Wow, that is a lot of NAs in the steps variable. It would be good to clean it up some.


```r
cleanact <- activity[!is.na(activity$steps),]
```


```
## 'data.frame':	15264 obs. of  3 variables:
##  $ steps   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-02:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-03:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-04:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-05:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-06:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-07:  288   Max.   :2355.0  
##                   (Other)   :13536
```

So much better!  

### Summing Steps over Days and Creating a Histogram  
Next, we'd like to know the total number of steps taken per day. Thankfully, date is already a factor variable. I'm going to use the **tapply()**  
function to get this done. I will also make a histogram using lattice graphics.


```r
totalday <- tapply(cleanact$steps,list(cleanact$date),sum)
head(totalday)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA        126      11352      12116      13294      15420
```

```r
## Now we can make a histogram with number of steps per day as bins and the count of days
## as the frequency variable.
library(lattice)
histogram(totalday, main = "Steps per Day", xlab = "Number of Steps", ylab = "Number of Days")
```

![plot of chunk tapply](figure/tapply-1.png) 

### Reporting out the mean and median

```
## [1] "Mean Total Steps per Day = 10766.19"
```

```
## [1] "Median Total Steps per Day = 10765"
```

### Ploting average steps per interval
We're going to use **tapply()** again, except this time, we'll use interval as the factor and mean as the function. We'll then use **xyplot()** from  
the lattice package to plot the frequency of steps throughout the day. In order to make the plot look more readable, we're specifying that  
the x-axis ticks will occur every 120 minutes and will be rotated 90 degrees. This is controled with the *scale* parameter in **xyplot**.  
To ensure we only edit the x-axis, *x = list(...)* is specified within the *scale* parameter.


```r
totalinterval <- tapply(cleanact$steps,list(cleanact$interval),mean)
xyplot(totalinterval ~ interval,cleanact,type="l",scale=list(x=list(tick.number = 120, at = seq(0,2355,120),rot=90)),main="Steps By Interval",xlab = "Interval",ylab = "Steps")
```

![plot of chunk daily](figure/daily-1.png) 

### Reporting out the interval with the maximum average steps

```
## [1] "Most Active Interval: 835 or 13 hours 55 minutes or 1 : 55 pm"
```

### Filling In Missing Values
So, how many missing values are there?


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

Well, that's alot! We'd like to fill those in. How many zero values are there?


```r
length(which(activity$steps == 0))
```

```
## [1] 11014
```

Those are going to cause our mean to be lower than it ought to be. First lets remove the zeros so that they don't skew the data low. This  
probably means the data will be skewed high, but who doesn't want to catch a break with their exercise routine? After removing the zeros,  
we'll use the mean of all intervals on all days to fill in the NA values. We wont remove the zeros in the full dataset, however, because  
they *are* data.


```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
nozero <- cleanact[cleanact$steps != 0,]
mean <- mean(nozero$steps)

activity["steps"][is.na(activity["steps"])] <- mean
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  134 134 134 134 134 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
### Any Change? 
How does this substitution change the histogram, mean and median?


```r
totalday <- tapply(activity$steps,list(activity$date),sum)
head(totalday)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##   38667.08     126.00   11352.00   12116.00   13294.00   15420.00
```

```r
## Remake the histogram with number of steps per day as bins and the count of days as the frequency variable.
library(lattice)
histogram(totalday, main = "Steps per Day", xlab = "Number of Steps", ylab = "Number of Days")
```

![plot of chunk tapply2](figure/tapply2-1.png) 

### Reporting out the mean and median

```
## [1] "Mean Total Steps per Day = 14425.32"
```

```
## [1] "Median Total Steps per Day = 11458"
```
Well, that changed quite a bit. We'd better know our datasets really well before trying this on real science! Substituting a number for  
NA caused the mean value to jump by over 4000 steps! The median value was skewed upwards as well, since the mean value fell in the lower  
50% of the NA-free dataset. The distribution of the data has changed quite a bit as well, as you can see in the histogram. The strange  
outlier bin around 40000 is the result of the non-zero mean, which is approximately 134.3, multiplied by the number of intervals in a  
day, 288. This reveals that there were several days when ALL intervals were missing data.

### More activity on the Weekend or Weekdays?
We'd like to see whether there is more activity on the Weekends or Weekdays. For this, we need the *date* variable to be of class "date"  
or POSIXct.


```r
## date is a factor, so we need to convert it into a character, then into a POSIXlt.
date <- activity$date
chardate <- as.character(date)
POSdate <- format(as.POSIXlt(chardate),format="%Y-%m-%d")

## When we bind POSdate to cleanact, it becomes a factor, so we need to convert it to a POSIXlt
dateact <- cbind(activity,POSdate)
dateact$POSdate <- as.POSIXlt(dateact$POSdate)

## Now we can figure out the days of the week and add those to the data.frame
week <- weekdays(dateact$POSdate)
dateact <- cbind(dateact,week)

str(dateact)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ steps   : num  134 134 134 134 134 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ POSdate : POSIXlt, format: "2012-10-01" "2012-10-01" ...
##  $ week    : Factor w/ 7 levels "Friday","Monday",..: 2 2 2 2 2 2 2 2 2 2 ...
```

For simplicity, we're going to split dateact into two data.frame. Weekend and Weekday.


```r
Weekend <- dateact[dateact$week == "Sunday" | dateact$week == "Saturday",]
Weekday <- dateact[dateact$week != "Sunday" & dateact$week != "Saturday",]
```

At this point, I am out of time. I need the last 15 minute to confirm the submission.

