Loading and preprocessing the data
----------------------------------

Show any code that is needed to

1.- Load the data (i.e. read.csv()).

2.- Process/transform the data (if necessary) into a format suitable for
your analysis.

    activity <- read.csv("activity.csv")

    base01 <- activity %>%
      mutate(date=as.Date(date,format = "%Y-%m-%d")) %>%
      filter(!is.na(steps)) %>%
      arrange(date)
    head(base01)

    ##   steps       date interval
    ## 1     0 2012-10-02        0
    ## 2     0 2012-10-02        5
    ## 3     0 2012-10-02       10
    ## 4     0 2012-10-02       15
    ## 5     0 2012-10-02       20
    ## 6     0 2012-10-02       25

Including Plots
---------------

What is mean total number of steps taken per day? For this part of the
assignment, you can ignore the missing values in the dataset.

1.- Calculate the total number of steps taken per day.

2.- If you do not understand the difference between a histogram and a
barplot, research the difference between them. Make a histogram of the
total number of steps taken each day 3.- Calculate and report the mean
and median of the total number of steps taken per day

    ## omite missing values 
    df01 <- base01 %>%
      group_by(date) %>%
      summarise(Total.steps=sum(steps)) %>%
      ungroup() %>%
      data.frame()
    head(df01)

    ##         date Total.steps
    ## 1 2012-10-02         126
    ## 2 2012-10-03       11352
    ## 3 2012-10-04       12116
    ## 4 2012-10-05       13294
    ## 5 2012-10-06       15420
    ## 6 2012-10-07       11015

    # histogram
    p1 <- ggplot(df01,aes(x=Total.steps)) +
      geom_histogram(bins =8 , color="white", fill="red")
    p1

![](README_figs/README-unnamed-chunk-5-1.png)

    # 3.- Mean and median of number of steps taken each day
    df02 <- base01 %>% 
      group_by(date) %>%
      summarise(Mean_Interval=mean(interval),
                Median_Interval = median(interval)) %>%
      ungroup() %>%
      data.frame()
      
    head(df02)

    ##         date Mean_Interval Median_Interval
    ## 1 2012-10-02        1177.5          1177.5
    ## 2 2012-10-03        1177.5          1177.5
    ## 3 2012-10-04        1177.5          1177.5
    ## 4 2012-10-05        1177.5          1177.5
    ## 5 2012-10-06        1177.5          1177.5
    ## 6 2012-10-07        1177.5          1177.5

What is the average daily activity pattern?
-------------------------------------------

1.- Make a time series plot (i.e. type = “l”) of the 5-minute interval
(x-axis) and the average number of steps taken, averaged across all days
(y-axis).

2.- Which 5-minute interval, on average across all the days in the
dataset, contains the maximum number of steps?.

    df03 <- base01 %>%
      group_by(interval) %>%
      summarise(MeanSteps = mean(steps)) %>%
      ungroup() %>%
      data.frame()

    # time series plot
    p2 <- ggplot(df03,aes(x=interval,y=MeanSteps)) +
      geom_line(col="blue") +
      ggtitle("Time series plot of the average number of steps taken") +
      xlab("Interval") +
      ylab("Average number of steps")
    p2

![](README_figs/README-unnamed-chunk-7-1.png)

Imputing missing values
-----------------------

Note that there are a number of days/intervals where there are missing
values (coded as NA). The presence of missing days may introduce bias
into some calculations or summaries of the data.

1.-Calculate and report the total number of missing values in the
dataset (i.e. the total number of rows with NAs).

2.- Devise a strategy for filling in all of the missing values in the
dataset. The strategy does not need to be sophisticated. For example,
you could use the mean/median for that day, or the mean for that
5-minute interval, etc.

3.- Create a new dataset that is equal to the original dataset but with
the missing data filled in.

4.- Make a histogram of the total number of steps taken each day and
Calculate and report the mean and median total number of steps taken per
day. Do these values differ from the estimates from the first part of
the assignment? What is the impact of imputing missing data on the
estimates of the total daily number of steps?.

    # total number of missing values
    df_missing <- activity %>%
      filter(is.na(steps)) %>%
      group_by(steps) %>%
      summarise(n=n()) %>%
      ungroup() %>%
      data.frame()
    head(df_missing)

    ##   steps    n
    ## 1    NA 2304

In order not to alter the average of each step in each interval, I
consider that a good alternative is to replace the missing values with
the average of each step in each interval.

    # mean of each step
    df04 <- base01 %>%
      group_by(interval) %>%
      summarise(MeanSteps = mean(steps)) %>%
      ungroup() %>%
      data.frame() %>%
      arrange(desc(MeanSteps)) 
    head(df04)

    ##   interval MeanSteps
    ## 1      835  206.1698
    ## 2      840  195.9245
    ## 3      850  183.3962
    ## 4      845  179.5660
    ## 5      830  177.3019
    ## 6      820  171.1509

    # Code to describe and show a strategy for imputing missing data
    activity.imp <- activity %>% filter(is.na(steps))
    for(j in 1: nrow(df04)){
      for(i in 1:nrow(activity.imp)){
        if(is.na(activity.imp[i,1]) & activity.imp[i,3]==df04[j,1]){
          activity.imp[i,1] <- df04[j,2]
        }else{
          i <- i+1
        }
      }
    }

    activity.cleaned <- rbind(activity.imp%>%mutate(date=as.Date(date)),base01)
    dim(activity)

    ## [1] 17568     3

    dim(activity.cleaned)

    ## [1] 17568     3

    df01.2 <- activity.cleaned %>%
      group_by(date) %>%
      summarise(Total.steps=sum(steps)) %>%
      ungroup() %>%
      data.frame()

    # histogram after missing values
    p3 <- ggplot(df01.2,aes(x=Total.steps)) +
      geom_histogram(bins =8 , color="white", fill="red") +
      ggtitle("Histogram after missing values are imputed ")
    p3

![](README_figs/README-unnamed-chunk-11-1.png)

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

For this part the weekdays() function may be of some help here. Use the
dataset with the filled-in missing values for this part.

1.- Create a new factor variable in the dataset with two levels –
“weekday” and “weekend” indicating whether a given date is a weekday or
weekend day.

2.- Make a panel plot containing a time series plot (i.e. type = “l”) of
the 5-minute interval (x-axis) and the average number of steps taken,
averaged across all weekday days or weekend days (y-axis). See the
README file in the GitHub repository to see an example of what this plot
should look like using simulated data.

    df05 <- activity.cleaned %>%
      mutate(weekdays=weekdays(as.Date(date)),
             type = ifelse(weekdays == "sábado" | weekdays == "domingo","weekend","weekday"))%>%
      group_by(type,interval) %>%
      summarise(avg.steps=mean(steps)) %>%
      ungroup() %>%
      data.frame()
    head(df05)

    ##      type interval  avg.steps
    ## 1 weekday        0 2.25115304
    ## 2 weekday        5 0.44528302
    ## 3 weekday       10 0.17316562
    ## 4 weekday       15 0.19790356
    ## 5 weekday       20 0.09895178
    ## 6 weekday       25 1.59035639

    # Panel plot
    p4 <- ggplot(df05,aes(x=interval,y=avg.steps)) +
      geom_line() +
      facet_wrap(facets="type",nrow=2) +
      ggtitle("Average number of steps taken per 5-minute interval across weekdays and weekends") +
      xlab("Interval") +
      ylab("Average number of steps")
    p4

![](README_figs/README-unnamed-chunk-13-1.png)
