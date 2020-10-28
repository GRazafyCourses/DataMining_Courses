# Create a factor colors that contains names of 4 arbitrary colors.
 colors = factor(c("red","black","green","white"))
 # Create a list pattern that contains 500 colors randomly selected from the factor colors.
 pattern = sample(colors,500,replace =TRUE)
 #Create a list pattern1 as a result of representation of pattern in a form of a data frame and pattern2 - in the form of a matrix. Compare the result to be achieved.
pattern2 = matrix(unlist(pattern))
pattern1 = data.frame(unlist(pattern))
#Expand the list pattern by the factor colors (at first) and by the list colors1 (secondly). Compare the results (are there any differences between them?) with the earlier ones.
expand.grid(pattern,colors1)
expand.grid(pattern,colors)
#Create a copy of the data, say, cars2
cars2=cars
#Modify cars2: transform speed units into m/s (meters per second), and distance units into m (meters).
attr(cars2,"unit")<- c("m/s","m","km/h")
#change the name of the column
names(cars2)[1] <- "m/s"
names(cars2)[2] <- "m"
#Create a scatterplot for the variables speed and dist.
plot(cars2,xlab="speed",ylab="dist")

#Use linear regression to find the best line to fit speed and dist variables.
 scatter.smooth(cars2)
 cars_LR <- lm(cars2$speed~cars2$dist)
 abline(cars_LR)
 #Extend the data-frame with the third column (dist2), which contains the speed in km/h (kilometers per hour)
dist2 <- cars2[,1]*3.6
names(dist2)[1] <-"km/h"
cars2$speed2 <- dist2

#1.3
# What is the mean of the data? What is the median?
 median(dfAge[,1])      ===>25
 mean(dfAge[,1])        ===> 29.6
 mode(age)              ===> 25
 (min(age)+max(age))/2  ===> 41.5
 summary(age)           ===>  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
                              13.00   20.50   25.00   29.96   35.00   70.00

boxplot(age)



#1.4
mean(hospital$ageHospital)
[1] 46.44444
mean(hospital$fatHospital)
[1] 28.78333

> median(hospital$ageHospital)
[1] 51
> median(hospital$fatHospital)
[1] 30.7

> sd(hospital$ageHospital)
[1] 13.21862
> sd(hospital$fatHospital)
[1] 9.254395

boxplot(hospital)
plot(hospital)

cor(hospital,method = "pearson")

#1.5
find_outlier <- function(v) {
    sd_by_2 = sd(v)*2
    for(value in v){
      if(value > mean(v)+sd_by_2 || value< mean(v)-sd_by_2){
        print(value)
      }
    }
}

# Find outliers for the age attribute (Exercise 1.3).
find_outlier(age)
[1] 70
# Find outliers for the speed and dist attributes (cars2 dataset).
find_outlier(cars2$speed)
[1] 4
[1] 4
find_outlier(cars2$dist)
[1] 120

# Find outliers for the Sepal.Length, Sepal.Width, Petal.Length, Petal.Width attributes (iris dataset).
find_outlier(iris$Sepal.Length)
[1] 7.6
[1] 7.7
[1] 7.7
[1] 7.7
[1] 7.9
[1] 7.7
find_outlier(iris$Sepal.Width)
[1] 4
[1] 4.4
[1] 4.1
[1] 4.2
[1] 2
find_outlier(iris$Petal.Width)
find_outlier(iris$Petal.Length)

#1.6
pres.age <-c(57, 61, 57, 57, 58, 57, 61, 54, 68, 51, 49, 64, 50, 48, 65, 52, 56, 46, 54, 49, 51, 47, 55, 55, 54, 42, 51, 56, 55, 51, 54, 51, 60, 61, 43, 55, 56, 61, 52, 69, 64, 46, 54, 47)
#Create the histogram for the pres.age attribute.
hist(pres.age)
#Find outliers (exploit the same definition as previously). Can you point out the outliers using the histogram?
find_outlier(pres.age)
[1] 68
[1] 42
[1] 69
# Create the boxplot for the pres.age attribute. Compare the boxplot and histogram.
boxplot(pres.age)


#1.7
# Define R-data frame with the above data,
 year_WOG <- c(1992,1994,1998,2002,2006,2010)
 location_WOG <- c("Albertville","Lillehammer","Nagano","Salt Lake City","Torino","Vancouver")
 number_disciplines_WOG <- c(57,61,68,78,84,86)
 heads_of_state <- c("F. Mitterand","King Harald V","Emperor Akihito","President G.Bush","President C. Ciampi","Governor General M.Jean")
 WOG <- data.frame(year_WOG,location_WOG,number_disciplines_WOG,heads_of_state)
# Redefine the subset of the data taking into account the Olympic Games with more than 78 disciplines only,
 WOG_with78_or_more <- subset(WOG,WOG$number_disciplines_WOG>78)
 # Create the subset of the data taking into account the Olympic Games with the number of disciplines greather than mean value of the numbers of disciplines.
 WOG_with_more_than_mean <- subset(WOG,WOG$number_disciplines_WOG>mean(WOG$number_disciplines_WOG))

#1.8
# N1(x) = (x - mean(x)) / 100, where x is an initial value (a number of disciplines),
N1 <- function(v) {
  (v-mean(v))/100
}
# N2(x) = (x - mean(x)) * (max(x) - min(x))/1000.
N2 <- function(v) {
  (v - mean(v)) * (max(v) - min(v))/1000
}
# N3(x) is a difference between the initial values and their standard deviation, divided by their midrange. (Is it a real normalization?)
N3 <- function(v) {
  (v - sd(v)) /((min(v)+max(v))/2)
}

#1.9
# Create the (frequency) histogram of the number of passengers with 5 green breaks and blue border with all values oriented horizontally.
hist(QE$nbOfPassangers,col="green",freq=FALSE,border = "blue",breaks=5)
# Create the (probability) histogram of the number of passengers with red breaks and green border and density function with all values oriented vertically.
hist(QE$nbOfPassangers,col="red",freq=FALSE,border = "green",breaks=5)
# Formulate the condition of drawing of the density line.
lines(density(QE$nbOfPassangers))

#1.10

 lines(density(newData))
 pie(newData)
 newData <- iris$Sepal.Length[c(140:150)]
