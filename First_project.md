Reproducible_research_1st_project
================
Zubair Afzal
2025-09-18

``` r
library(dplyr)
library(ggplot2)
library(lattice)
```

# **Loading and Processing Data**

``` r
activity <- read.csv("activity.csv")
str(activity)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

``` r
summary(activity)
```

    ##      steps            date              interval     
    ##  Min.   :  0.00   Length:17568       Min.   :   0.0  
    ##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
    ##  Median :  0.00   Mode  :character   Median :1177.5  
    ##  Mean   : 37.38                      Mean   :1177.5  
    ##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
    ##  Max.   :806.00                      Max.   :2355.0  
    ##  NA's   :2304

# **Mean Total Number of Steps per Day**

``` r
steps_per_day <- activity %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm = TRUE))
hist(steps_per_day$total_steps,
     main = "Total Steps Per day",
     xlab = "Steps",
     col = "steelblue"
     )
```

![](First_project_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
mean(steps_per_day$total_steps)
```

    ## [1] 9354.23

``` r
median(steps_per_day$total_steps)
```

    ## [1] 10395

# **Daily average activity pattern**

``` r
daily_activity <- activity%>%
  group_by(interval) %>%
  summarise(mean_steps = mean(steps, na.rm = TRUE))

plot(daily_activity$interval, daily_activity$mean_steps, type = "l",
     main = "Average Daily Activity Pattern",
     xlab = "5-minute Interval", ylab = "Average Steps"
     )
```

![](First_project_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
daily_activity[which.max(daily_activity$mean_steps), ]
```

    ## # A tibble: 1 × 2
    ##   interval mean_steps
    ##      <int>      <dbl>
    ## 1      835       206.

# **Imputing Missing Values**

``` r
sum(is.na(activity$steps))
```

    ## [1] 2304

``` r
activity_filled <- activity %>%
  left_join(daily_activity, by = "interval") %>%
  mutate(steps = ifelse(is.na(steps), mean_steps, steps)) %>%
  select(steps, date, interval)

steps_per_day_filled <- activity_filled %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps))
hist(steps_per_day_filled$total_steps,
     main = "Total Steps per Day (Missing Values Imputed)",
     xlab = "Steps", col = "darkgreen")
```

![](First_project_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
mean(steps_per_day_filled$total_steps)
```

    ## [1] 10766.19

``` r
median(steps_per_day_filled$total_steps)
```

    ## [1] 10766.19

## **Weekday VS Weekend Pattern**

``` r
activity_filled$day_type <- ifelse(weekdays(as.Date(activity_filled$date)) %in%
                                     c("Saturday","Sunday"),
                                   "weekend","weekday")
avg_daytype <- activity_filled %>%
  group_by(day_type, interval) %>%
  summarise(mean_steps = mean(steps))
```

    ## `summarise()` has grouped output by 'day_type'. You can override using the `.groups` argument.

``` r
xyplot(mean_steps ~ interval | day_type, data = avg_daytype, type = "l",
       layout = c(1,2),
       xlab = "Interval", ylab = "Average Steps")
```

![](First_project_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

End of the Project…
