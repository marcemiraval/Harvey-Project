
# http://rpubs.com/RobinLovelace/14465

install.packages(c("rgdal", "sp"))
library(rgdal)
library(sp)# add spatial package to load the S4 spatial objects
library(spatstat)  # to calculate field of point density
library(maptools)  # to convert to point pattern

# Defining working directory
setwd ("/home/marcela/Coding/EWE-reporting/Sandy/EastCoast")
Reports <- read.csv(file="Sandy_Subset_Clean.csv", header=TRUE, sep=",")
coordinates(Reports)<-~x+y
class(Reports)
plot(Reports)
sSp <- as(SpatialPoints(Reports), "ppp")  # convert points to pp class
Dens <- density(sSp, adjust = 0.2)  # create density object
class(Dens)  # just for interest: it's got it's of pixel image class
plot(Dens)
contour(density(sSp, adjust = 0.2), nlevels = 4)  # plot as contours - this is where we're heading
