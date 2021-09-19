# This problem set introduces you to some of the basic mechanics of R. It is based on Peng (2016) R Programming for Data Science; Chapt headings below correspond to chapter in this book. You can download the book at  http://leanpub.com/rprogramming.

# Although PS1 isn't due until 19 September, please start on it immediately so that you are prepared for lectures.

# For the homework, ONLY TURN IN THE R CODE THAT YOU USED. Start with the code from PS1_R_Programming_2021.R and add to it anything you need. If you are adding to my code, identify you new code (so I can find it) by placing it between marker rows #~~~~~~~~~~~~~~~~~~~~~~~~~~~. There is no need for you to write down detailed answers, since we will discuss everything in class. Do write down questions that you have, though.

# As a departure from the norm for later problem sets, EVERYBODY SHOULD TURN IN A COPY OF THEIR R CODE. I know that I will likely get replicate version from those of you i the same group. Still, I want to make sure everybody goes through all of the steps of submitting R code.

# Due: 19Sep21 (midnight)
#~~~~~~~~~~~~~~~~
library(dplyr)
#~~~~~~~~~~~~~~~~

#################################################################
# Chapt 5. Nuts and Bolts
#################################################################

#1. Create a 2 x 5 matrix containing 1:10, with 1:5 on the first row and 6:10 on the second row. (Bonus: Come up with two or more ways to do this.)
#~~~~~~~~~~~~~~~~
#First way
m<- matrix(1:10,nrow = 2, ncol = 5)
# Second Way
m2 <- 1:10
dim(m2) <- c(2,5)
#Third Way
m3<- as.matrix(rbind(1:5,6:10))
#~~~~~~~~~~~~~~~~

#2. Label the rows of your matrix "A" and "B", and the columns "a", ..."e".
#~~~~~~~~~~~~~~~~
rownames(m) <- c("A", "B")
colnames(m) <- c("a", "b", "c", "d","e")
#~~~~~~~~~~~~~~~~

#3. Convert the values of your matrix to double.
#~~~~~~~~~~~~~~~~
m <- as.double(m)
dim(m)<- c(2,5)
rownames(m) <- c("A", "B")
colnames(m) <- c("a", "b", "c", "d","e")
#~~~~~~~~~~~~~~~~

#4. Create a data.frame named "df" with a column named "site" with values 1:5 as factors and a column named "value" with values 100, 200, ..., 500. Then give it row.names "a", ..., "e".
#~~~~~~~~~~~~~~~~
site<- as.factor(1:5)
value<- c(100, 200, 300, 400, 500)
df<-cbind(site, value)
rownames(df)<- c("a", "b", "c", "d","e")
df<-data.frame(df)
#~~~~~~~~~~~~~~~~

#5. Rename the variables (columns) in df "site.cat" and "x".
#~~~~~~~~~~~~~~~~
colnames(df) <- c("site.cat", "X")
#~~~~~~~~~~~~~~~~

#6. Convert df to a list and extract the first element of the list by number and then by name ("site").

#~~~~~~~~~~~~~~~~
dflist<- unlist(as.list(df))
dflist[6]
dflist[1]
#~~~~~~~~~~~~~~~~
#################################################################
# Chapt 6. Uploading data
#################################################################

# Metadata for "grouse_data.csv"
# These data are simulated to have similar characteristics as the original, real data.

# ROUTE
# IDs for 50 roadside routes. 

# STATION
# IDs for each survey station, with up to 8 STATIONS per ROUTE.

# LAT
# X coordinate of survey station. UTM zone 15N NAD83 datum.

# LONG
# Y coordinate of survey station. UTM zone 15N NAD83 datum.

# WIND
# Wind speed (km/hour) recorded at 1.4m above ground at the end of each observation.

# TEMP
# Temperature (°C) recorded at 1.4m above ground at the end of each observation.

# GROUSE
# Detection/non-detection of Ruffed Grouse (1 = detected, 0 = not detected).

#7.  Create a data.frame called d by reading data from file = "grouse_data.csv". Make the variables "ROUTE" and "STATION" factors, and "GROUSE" integers
#~~~~~~~~~~~~~~~~
d<- read.csv('PS1/grouse_data.csv')
d$ROUTE<- as.factor(d$ROUTE)
d$STATION<- as.factor(d$STATION)
d$GROUSE<- as.integer(d$GROUSE)
summary(d)
#~~~~~~~~~~~~~~~~

#################################################################
# Chapt 10. Subsetting data
#################################################################

#8. Select the subset of d with (a) ROUTE = 1, (b) LAT greater than mean(LAT), (c) LAT greater than median(LAT)
#~~~~~~~~~~~~~~~~
# (a)
#Base R
d[d$ROUTE == 1,]
#dplyr
d%>%
  filter(ROUTE == 1)
# (b)
#Base R
d[d$LAT > mean(d$LAT),]
#dplyr
d%>%
  filter(LAT > mean(LAT))
# (c)
#Base R
d[d$LAT > median(d$LAT),]
#dplyr
d%>%
  filter(LAT > median(LAT))
#~~~~~~~~~~~~~~~~

#9. Create a new variable "NORTHERN" that is TRUE for the more northerly half of the stations and FALSE otherwise.
#~~~~~~~~~~~~~~~~
d1<- d %>%
  mutate(NORTHERN = ifelse(LAT > mean(LAT), "TRUE", "FALSE"))
#~~~~~~~~~~~~~~~~

#10. How many different routes are there in d?
#~~~~~~~~~~~~~~~~
#50 different routes
#~~~~~~~~~~~~~~~~

#11. Construct a matrix d.matrix that has the same information as d. Compute the number of routes using d.matrix and the unique() function.
#~~~~~~~~~~~~~~~~
d.matrix <- data.matrix(d1)
length(unique(d.matrix[,1]))
#~~~~~~~~~~~~~~~~

#################################################################
# Chapt 11. Vectorized Operations
#################################################################

#12. Create a new variable "QUADRANT" that divides the stations into four groups, with values 0 for the northwest group, 1 for the northeast group, 2 for the southwest group, and 3 for the southeast group.
#~~~~~~~~~~~~~~~~
d2<- d1%>%
  mutate(QUADRANT = ifelse(STATION == 0, "northwest", 
                           ifelse(STATION == 1, "northeast", 
                                  ifelse(STATION == 2, "southwest", "southeast"))))
#~~~~~~~~~~~~~~~~

#################################################################
# Chapt 12. Date and times
#################################################################

# You should read this chapter to be aware these features exiset.

#################################################################
# Chapt 13. Managing dataframes
#################################################################

library(dplyr)

#13. Select columns of d that start with "L" (i.e., LAT and LONG). Do this both using and not using dplyr.
#~~~~~~~~~~~~~~~~
d.L <- select(d2, starts_with("L"))
d.L.base<- d2[, c("LAT", "LONG")]
data.frame(LAT = d$LAT, LONG=d$LONG)
#~~~~~~~~~~~~~~~~

#14. Select rows of d that contain the highest 50% of the values of LAT. Do this both using and not using dplyr. Do you see any differences in the data.frame that is produced?
#~~~~~~~~~~~~~~~~
d.LAT50 <- d %>%
  filter(LAT > mean(LAT))
d.LAT50.base<- d[d$LAT>median(d$LAT),]
#~~~~~~~~~~~~~~~~

#15. Select rows of d that contain ROUTE > 45. This is much trickier than you might think!
#~~~~~~~~~~~~~~~~
d.RT45 <- d %>%
  filter(as.numeric(as.character(ROUTE)) > 45)
#~~~~~~~~~~~~~~~~

#16. Reorder rows by LAT to create a new data.frame d.LAT. Then reorder d.LAT by ROUTE
#~~~~~~~~~~~~~~~~
d.LAT <- d%>%
  arrange(LAT)%>%
  arrange(ROUTE)

d.LAT.base<- d[order(d$ROUTE),]
#~~~~~~~~~~~~~~~~

#17. Rename the column in d from ROUTE.num to num.ROUTE. Do this both using and not using dplyr.
#~~~~~~~~~~~~~~~~
#dplyr
d.rename<- d%>%
  rename(ROUTE.num = ROUTE)%>%
  rename(num.ROUTE = ROUTE.num)

#base R
names(d)[1]<- "ROUTE.num"
names(d)[1]<- "num.ROUTE"

names(d)[1]<- "ROUTE"
#~~~~~~~~~~~~~~~~

#18. Add a variable to d called exp.GROUSE than contains exp(GROUSE). Do this both using and not using dplyr.

#~~~~~~~~~~~~~~~~
#dplyr
d<- d%>%
  mutate(exp.GROUSE = exp(GROUSE))

# base R
d$exp.GROUSE <- exp(d$GROUSE)
#~~~~~~~~~~~~~~~~


#19. Create a data.frame called d.ROUTE that contains the mean value of GROUSE for each ROUTE. Do this both using and not using dplyr. Bonus: create a new data.frame d.NA which is the same as d but with d$ROUTE[1] <- NA. Then perform the same operation as you just did for d.
#~~~~~~~~~~~~~~~~
d.ROUTE<- d%>%
  group_by(ROUTE)%>%
  summarize(GROUSE = mean(GROUSE))

d.ROUTE.base<- aggregate(GROUSE~ROUTE, data = d, FUN = mean)

#BONUS
d.NA<- d
d.NA[1,] <- NA
d.NA$num.ROUTE <- as.numeric(d.NA$ROUTE)
d.NA<- d.NA %>%
  group_by(ROUTE, num.ROUTE)%>%
  summarize(GROUSE = mean(GROUSE))
#~~~~~~~~~~~~~~~~

#20. Perform the same operation as in #19 using piping in dplyr.
#~~~~~~~~~~~~~~~~
d.ROUTE<- d%>%
  group_by(ROUTE)%>%
  summarize(GROUSE = mean(GROUSE))
#~~~~~~~~~~~~~~~~

#################################################################
# Chapt 14. Control Structures
#################################################################

# The tasks here should really be done as in the previous questions. Here, I'm using the same tasks to illustrate the basics of control structures. These basic structures I'm illustrating are very common, and you will see them repeatedly throughout the course.

#21. Create a data.frame called d.ROUTE that contains the mean value of GROUSE for each ROUTE using a for loop (i.e., group_by() and aggregate() aren't allowed). This involves first creating a data.frame and then looping through the values of ROUTE to fill in values. I've given you the first line creating the data.frame
#~~~~~~~~~~~~~~~~
d$num.ROUTE<- as.numeric(d$ROUTE)

d.ROUTE.loop <- data.frame(ROUTE = unique(d$num.ROUTE))
for(i in 1:length(unique(d$ROUTE))){
  d.ROUTE.loop$GROUSE[i] <- mean(d$GROUSE[d$ROUTE == i])
}

print(d.ROUTE.loop)

# d.ROUTE<- data.grame(ROUTE = unique(d$num.ROUTE))
# for(i.ROUTE in unique(d$num.ROUTE)){
# d.ROUTE$GROUSE[d.ROUTE$ROUTE == i.ROUTE] <- mean(d$GROUSE[d$num.ROUTE == i.ROUTE])
#}

#Checking if correct
d.average<- d%>%
  group_by(as.factor(ROUTE))%>%
  summarise(mean = mean(GROUSE))
#~~~~~~~~~~~~~~~~

#22. Create a data.frame called d.ROUTE that contains the mean value of GROUSE for each ROUTE using a while loop (i.e., group_by() and aggregate() aren't allowed). 

#~~~~~~~~~~~~~~~~


d.ROUTE.while <- data.frame(ROUTE = unique(d$num.ROUTE))
i <- 0
while(i <nrow(d.ROUTE.while)){
  i <- i + 1
  d.ROUTE.while$GROUSE[i] <- mean(d$GROUSE[d$num.ROUTE == d.ROUTE.while$ROUTE[i]])
}
#View the results
cbind(aggregate(GROUSE ~ROUTE, data = d, FUN = mean), d.ROUTE.while)
abs(sum(aggregate(GROUSE ~ num.ROUTE, data = d, FUN = mean)$GROUSE != d.ROUTE.while$GROUSE))
#~~~~~~~~~~~~~~~~
#23. Create a data.frame called d.ROUTE.NA from d.NA that contains the mean value of GROUSE for each ROUTE using a for loop, in which the ROUTE with an NA is given the value NaN. You will need the is.na() function for this.
#~~~~~~~~~~~~~~~~
d.ROUTE.NA<- data.frame(ROUTE = unique(d.NA$ROUTE))
for(i in 1:length(unique(d.NA$num.ROUTE))){
  x <- d.NA$GROUSE[d.NA$num.ROUTE[i]]
  if((is.na(x)) == TRUE){
    d.ROUTE.NA$GROUSE[d.ROUTE.NA$ROUTE == i] <- NaN
  }else{
    d.ROUTE.NA$GROUSE[d.ROUTE.NA$ROUTE == i] <- x
  }
}

#~~~~~~~~~~~~~~~~
#################################################################
# Chapt 15. Functions
#################################################################

#24. Write a function locate_station() that returns the LAT and LONG for a ROUTE and STATION in d. Note you are best to use num.ROUTE, not ROUTE.
locate_station <- function(route, station, data){ #define what you put into it
  return(data[data$num.ROUTE == route & data$STATION == station, c("LAT", "LONG")])
}
locate_station(route = 30, station = 3, data = d)

# It is also possible to write the following, but there are risks.
locate_station_bad<- function(route, station){
  return(d[d$num.ROUTE == route & d$STATION == station, c("LAT", "LONG")])
}
locate_station_bad(route = 30, station = 3)

#SCOPING RULES: excludes the data file. Not good for 300 lines of code

#25. Write a function distance_station() that returns the Euclidean distance between a specified ROUTE and STATION in d and all other routes and stations. Don't bother about converting to meters -- just calculate distance in terms of latitude and longitude values (ignoring that in Wisconsin, a degree latitude is not equal to a degree longitude).
distance_station<- function(route, station, data){
  specific.location <- locate_station(route = route, station = station, data = data)
  data$euclidean<-sqrt((specific.location$LAT - data$LAT)^2 + (specific.location$LONG - data$LONG)^2)
  return(data$euclidean)
}
distance_station(route = 30, station = 3, data = d)

# 26. Write a function plot_station() that plots the location of stations, with the size of the point for each station decreasing with increasing distance from a focal station specified as input to the function.
library(ggplot2)
plot_station <- function(route, station, data){
  point.size<- (distance_station(route = route, station = station, data = data))
  rank<- rank(-point.size)
  return(ggplot(data = d) + geom_point(aes(x = LONG, y = LAT, size = rank), shape = 1))
}
plot_station(route = 40, station = 3, data = d)

#################################################################
# Chapt 16. Scoping rules in R
#################################################################

# You should read this chapter to be warned. However, I'm going to try to make sure all the code you use in class is properly scoped. The bottom line is that it is often best to define variables in the definition of a function.

# 27. Do you have any questions about scoping? 

#################################################################
# Chapt 17. Standards
#################################################################

# These are good principles (but don't expect me to always follow them).

#################################################################
# Chapt 18. Loop functions
#################################################################

# 28. Use tapply() to create the same date.frame as aggregate(GROUSE ~ num.ROUTE, data=d, FUN=mean). You can also try this using ROUTE rather than num.ROUTE, and you will see some problems.
aggregate<- aggregate(GROUSE ~ num.ROUTE, data=d, FUN=mean)

t.apply<- tapply(d$GROUSE,d$num.ROUTE, mean)
print(t.apply)

# 29. First, add columns to data.frame d which give the distance of each station from two locations (e.g., ROUTE = 1, STATION = 1; and ROUTE = 40, STATION = 1). Use apply() to create an additional column that is the sum of these distances.

#not sure I quite understand the question

# 30. Use apply() and split() to create the same date.frame as aggregate(GROUSE ~ num.ROUTE, data=d, FUN=mean). You can also try this using ROUTE rather than num.ROUTE, and you will see some problems.
aggregate(GROUSE ~ num.ROUTE, data=d, FUN=mean)
grouse<- as.matrix(split(d, d$num.ROUTE))
str(grouse)
apply.dataframe<- apply(grouse$GROUSE, 1, FUN = mean)

#################################################################
# Chapts 19/20/21. Regular expressions, Debugging and Profiling
#################################################################

# You should read these chapters to be aware these features exiset.

#################################################################
# Chapt 22. Simulation
#################################################################

# 31. Plot a histogram of 1000 values simulated from a Gaussian distribution. Then plot the probability density function to compare them. You will need the hist() function (with the freq=F option).
x <- rnorm(1000)
hist(x)
hist(x, freq = FALSE)


# 32. Plot a histogram of 1000 values simulated from a Poisson distribution. Then plot the probability density (or mass) function to compare them. You will need the hist() function (with the freq=F option).

y = rpois(1000, 2)
hist(y, freq = FALSE)

#don't worry about process more about inference of the process
