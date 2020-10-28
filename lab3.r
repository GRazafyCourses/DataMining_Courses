# 2.1
# Use the aggregate function to calculate how many flower bouquets were sold each day and name the data as florist_by_day (see Examples).
# Create a plot for the subdata of florist_by_day consisting of number of bouquets. Can you find some outliers using the plot?
# Find the outliers and look at dates. Do you think that these records are correct?

find_outlier <- function(v) {
    sd_by_2 = sd(v)*2
    for(value in v){
      if(value > mean(v)+sd_by_2 || value< mean(v)-sd_by_2){
        print(value)
      }
    }
}

florist_by_day <- aggregate(florist$bouquet, by = list(florist$date), FUN = length)


# Outliers:
find_outlier(florist_by_day$x)
# Result :
look.for <- c(297,265,288,278,266,301)
florist_by_day[florist_by_day$x %in% look.for, ]
#       Group.1   x
# 67  2013-03-08 297
# 144 2013-05-24 265
# 432 2014-03-08 288
# 509 2014-05-24 278
# 797 2015-03-08 266
# 874 2015-05-24 301

# Yes this records are correct because the value seems to follow a pattern
# Select roses sales records and save them as roses data frame (hint: You can use the subset R function).
roses <- subset(florist,bouquet == "roses")
#Use the aggregate function to calculate how many rose bouquets were sold each day (see Examples).
roses_by_day <- aggregate(roses$price, by = list(roses$date), FUN = length)
names(roses_by_day) <- c("date", "number_of_bouquets")
#Generate a scatter plot for the roses_by_day data.
plot(roses_by_day)

# 2.2
# Use the rose data frame and the aggregate function to calculate total sales value of roses in particular weeks.
# Save the result as roses_by_week data frame. Set the columns names to year, week, and total. Order the rows by year and week

roses_by_week <- aggregate(roses$price,by = list(roses$year,roses$week), FUN = length)
names(roses_by_week) <- c( "year","week","total")
# Plot the roses_by_week$total column using the line (l) style.
plot(roses_by_week$total,type="l")
# Implement the binning function that computes the result of smoothing by bin. The function takes as its arguments

#    the input data,
#    the bin size (the last bin may be incomplete),
#    the function used to compute the new value for the ith element.

binning <- function(x, bin_size, FUN = mean) {
  len <- length(x)
  y <- vector()
  for(i in 1:len) {
    k <- i / bin_size
    if (k == 0) {
        k0 <- i - bin_size + 1
    } else {
        k0 <- i - k + 1
    }
    k1 <- k0 + bin_size + 1
    if (k1 > len) {
        k1 <- len
    }
    y[i] <- FUN(x[k0:k1])
  }
  return(y)
}
# Use the binning function to compute the result of smoothing roses_by_week$total by bin mean and median (roses_bin_1, roses_bin_2).
# Use the bin_size parameter equal to 5.

roses_bin_1 <- binning(roses_by_week$total,5)
roses_bin_2 <- binning(roses_by_week$total,5,median)

#Plot roses_by_week$total, roses_bin_1, and roses_bin_2 on one graph and analyse the results.

plot(roses_bin_1)
points(roses_bin_2)
points(roses_by_week)

#2.3
#Use the florist data frame and the aggregate function to calculate total sales value of all types of bouquets in 2014
# in particular sender province. Save the result as bouquets_by_province data frame. Set the columns names to province and total.
bouquets_by_province <- aggregate(roses$price,by = list(roses$sender_province), FUN = length)
names(bouquets_by_province) <- c( "province","total")

# Use the binning function to compute the result of smoothing bouquets_by_province$total by bin mean using bin size: 3, 4, and 5.
 binning(bouquets_by_province$total,3)
# Results :  [1] 248.6 259.2 255.4 255.4 418.8 472.0 472.0 453.6 472.6 472.6 485.0 446.0 446.0 390.0 462.6 462.6
 binning(bouquets_by_province$total,4)
# Results :  [1] 261.8333 244.0000 371.1667 470.8333 470.8333 408.5000 448.5000 432.1667 432.1667 530.0000 446.8333 400.6667 400.6667 462.6000 472.5000 456.2500
 binning(bouquets_by_province$total,5)
# Results :  [1] 248.4286 344.8571 422.5714 416.5714 410.5714 410.5714 417.2857 478.2857 518.7143 447.8571 447.8571 400.6667 462.6000 472.5000 456.2500 553.3333

#2.4
min_max_norm <- function(x, min, max) {
  len <- length(x)
  y <- vector()
  for(i in 1:len) {
      y[i] <- min +  ((x[i] - min(x)) * (max - min)) / (max(x) - min(x))
  }
  return(y)
}

z_score_norm <- function(x) {
  len <- length(x)
  y <- vector()
  for(i in 1:len) {
      y[i] <- (x[i] - mean(x)) / sd(x)
  }
  return(y)
}

decimal_norm <- function(x, power) {
  len <- length(x)
  y <- vector()
  for(i in 1:len) {
      y[i] <- x[i] / 10**power
  }
  return(y)
}

province_norm_1 <- min_max_norm(bouquets_by_province$total, 0, 1)
province_norm_2 <- z_score_norm(bouquets_by_province$total)
province_norm_3 <- decimal_norm(bouquets_by_province$total, 4)
 plot(province_norm_2)
 lines(province_norm_1)
 lines(province_norm_3)

#2.5
#Use the florist data frame and the aggregate function to calculate total sales value of all types of bouquets
# in particular months. Save the result as
# florist_by_month data frame. Set the columns names to year, month, and total. Order the rows by year and month.

florist_by_month <- aggregate(florist$price, by = list(florist$bouquet, florist$month, florist$year), FUN = sum)
names(florist_by_month) <- c('bouquet', 'month', 'year', 'total')
florist_by_month <- florist_by_month[order(florist_by_month$year, florist_by_month$month),]

# Normalize roses_by_month$total using min-max normalization (min = 0, max = 1). Save the result as roses_norm_1.
# Normalize roses_by_month$total using z-score normalization. Save the result as roses_norm_2.
# Normalize roses_by_month$total using decimal scaling normalization. Save the result as roses_norm_3.
# Plot roses_norm_1, roses_norm_2, and roses_norm_3 on one graph and analyse the results
roses_norm_1 <- min_max_norm(florist_by_month$total, 0, 1)
roses_norm_2 <- z_score_norm(florist_by_month$total)
roses_norm_3 <- decimal_norm(florist_by_month$total, 4)
 plot(roses_norm_2)
 lines(roses_norm_1)
 lines(roses_norm_3)



#2.6
# Use the florist data frame and the aggregate function to calculate total sales value of all types of bouquets in particular weeks.
# Save the result as florist_by_week data frame. Set the columns names to year, week, and total. Order the rows by year and week.
florist_by_week <- aggregate(florist$price, by = list(florist$bouquet, florist$week, florist$year), FUN = sum)
names(florist_by_week) <- c('bouquet', 'week', 'year', 'total')
florist_by_week <- florist_by_week[order(florist_by_week$year, florist_by_week$week),]
# Select arbitrary cut points and use the cut function to assign the following level to the weeks:
# low_sale, normal_sale, high_sale, extreme_sale. Use table function to analyse the data.
cut_florist_by_week <- cut(florist_by_week$total, labels=c("low_sale", "normal_sale", "high_sale", "extreme_sale"), breaks=c(0, 1000, 2000, 3000, 10000))
#Plot a histogram for the obtained data.
table_cut <- table(cut_florist_by_week)
summary(cut_florist_by_week)
  low_sale  normal_sale    high_sale extreme_sale
          52          337           51           31
plot(table_cut)


#2.7
#Use the florist data frame and the aggregate function to calculate the total number of all types of bouquets sold in particular weeks.
#Save the result as bouquets_by_week data frame.
#Set the columns names to year, week, and total. Order the rows by year and week.
bouquet_by_week <- aggregate(florist$price, by = list(florist$bouquet, florist$week, florist$year), FUN = length)
names(bouquet_by_week) <- c('bouquet', 'week', 'year', 'total')
bouquet_by_week <- bouquet_by_week[order(bouquet_by_week$year, bouquet_by_week$week),]

# Select arbitrary cut points and use the cut function to assign the following level to the weeks: low_sale, normal_sale, high_sale, extreme_sale.
# Use table function to analyse the data.

cut_bouquet_by_week <- cut(bouquet_by_week$total, labels=c("low_sale", "normal_sale", "high_sale", "extreme_sale"), breaks=c(0, 30, 100, 150, 300))
table_cut <- table(cut_bouquet_by_week)
summary(cut_bouquet_by_week)

#Compute the Pearson's linear correlation coefficient for florist_by_week$total and bouquets_by_week$total. How do we interpret the result?
cor(data.frame(bouquet_by_week$total, florist_by_week$total), method = "pearson")

# The Results are correlated
