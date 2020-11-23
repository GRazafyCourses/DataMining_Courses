
#3.2
library(philentropy)

v1 = c(1,2,3,4,5,6)
v2 = c(-4,-5,-6,-6,6,7)
compareMatrix <- rbind(v1,v2)

distance(compareMatrix,method = "euclidean")
#euclidean
# 16.03122
distance(compareMatrix,method = "canberra")
# canberra
#-11.83217
distance(compareMatrix,method = "minkowski",p=2)
#minkowski
# 16.03122
distance(compareMatrix,method = "minkowski",p=3)
#minkowski
# 13.00394
distance(compareMatrix,method = "manhattan").
#manhattan
#       33
distance(compareMatrix,method = "chebyshev")
#chebyshev
#       10

#3.4
cloud.cover <- factor(c("overcast", "overcast", "scattered", "broken","broken","overcast","scattered","overcast"))
precipitation <- factor(c("light", "heavy", "light", "none", "light","moderate","none","light"))
wind <- factor(c("strong", "moderate", "light", "light", "light", "light","calm","calm"))
temperature <- factor(c("chilly", "chilly", "cold", "cold", "cold","chilly","chilly","normal"))
weather <- data.frame(cloud.cover,precipitation,wind,temperature)
