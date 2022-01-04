require(rgdal)
library(dplyr)
library(spatstat)
library(MASS)
source("functions.R")

setwd("/Users/rafaelramos/crimeanalysis/paper_code")

#
# ======================================================================================================
#

# ===========================================
# Load data
# ===========================================

shape = readOGR(dsn = ".", layer = "burglary")
burglary = data.frame(lon=shape$lon_m,lat=shape$lat_m,year=as.numeric(shape$ANOFATO)+2007,target=shape$V19,month=as.numeric(shape$V5),weekday=as.numeric(shape$DIADASEMAN),time=shape$V10,furtoOuRoubo=shape$V12)
burglary = burglary %>% mutate(hour=as.numeric(substr(as.character(time),1,2)))

shape = readOGR(dsn = ".", layer = "homicidio")
homic = data.frame(lon=shape$X,lat=shape$Y)

shape = readOGR(dsn = ".", layer = "roubo_a_transeunte")
robbery = data.frame(lon=shape$LON_M,lat=shape$LAT_M,year=as.numeric(as.character(shape$ANO_FATO)),hour=as.numeric(as.character(shape$HORA)),weekday=as.numeric(as.character(shape$DIA_SEMANA)))

#
# ======================================================================================================
#

# ===========================================
# Estimating Optimal Granularity for Burglary
# ===========================================

# setting the granularities to be tested
burg_scales = seq(100,1650,25) # in meters
burg_scales_lon = burg_scales
burg_scales_lat = burg_scales

# estimating uniformity and robustness using the random samples method
# SHOULD TAKE A FEW MINUTES, BUT NOT TOO LONG
burg_spatstats_random = get_spatialstats_all(burglary,burg_scales_lon,burg_scales_lat,random_samples=T,signif=0.99)

# estimating uniformity and robustness using the contiguous samples method
# SHOULD TAKE A LONG TIME - RESULT IS SIMILAR (THOUGH NOT IDENTICAL) TO THE FORMER METHOD, 
# SO IN MOST CASES THE PRIOR WILL SUFFICE
burg_spatstats_contig = get_spatialstats_all(burglary,burg_scales_lon,burg_scales_lat,random_samples=F,signif=0.99)


#
# Burglary - Uniformity plots
#

tiff("burg_unif_robust.tiff",width=1800,height=800,res=200)
par(mfrow=c(1,2))

plot(NULL,NULL,xlab="Quadrat size (meters)",ylab="Internal Uniformity",xlim=c(150,1000),ylim=c(0,1))
points(burg_scales,1-burg_spatstats_random$nn_pass,col="red",pch=16)
points(burg_scales,1-burg_spatstats_random$quadrat_pass,col="black",pch=16)
points(burg_scales,1-burg_spatstats_contig$nn_pass,col="blue",pch=17)
points(burg_scales,1-burg_spatstats_contig$quadrat_pass,col="green",pch=17)
title("Internal uniformity - Burglary")
legend("bottomleft", 
       col = c("red"), 
       pch = c(16), 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 1, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.05, 0.05),
       y.intersp=2)
legend("bottomleft", 
       legend = c("Nearest-neighbor - random", "Quadrat Count - random",
                  "Nearest-neighbor - contiguous","Quadrat Count - contiguous"), 
       col = c("red", 
               "black",
               "blue",
               "green"), 
       pch = c(16,16,17,17), 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 0.5, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.05, 0.05),
       y.intersp=2)

#
# Burglary - Robustness to error plots
#

plot(NULL,NULL,xlab="Quadrat size (meters)",ylab="Robustness to error",xlim=c(150,1000),ylim=c(0,1))
# note that 1-burg_spatstats_random$error_binom is due to robusness = 1 - coef_var
points(burg_scales,1-burg_spatstats_random$error_binom,col="black",pch=16)
points(burg_scales,1-burg_spatstats_random$error_pois,col="black",pch=4)
points(burg_scales,1-burg_spatstats_random$error_boot,col="red",pch=16)
points(burg_scales,1-burg_spatstats_contig$error_binom,col="green",pch=16)
points(burg_scales,1-burg_spatstats_contig$error_pois,col="black",pch=1)
points(burg_scales,1-burg_spatstats_contig$error_boot,col="blue",pch=16)
title("Robustness to error - Burglary")

legend("bottomright", 
       col = c("black"), 
       pch = c(16), 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 1, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.15, 0.01),
       y.intersp=2)

legend("bottomright", 
       legend = c("Binomial - random", "Poisson - random", "Resampled - random",
                  "Binomial - contiguous", "Poisson - contiguous", "Resampled - contiguous"), 
       col = c("black", 
               "black",
               "red",
               "green",
               "black",
               "blue"), 
       pch = c(16,4,16,16,1,16), 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 0.5, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.15, 0.01),
       y.intersp=2)

dev.off()

#
# Burglary - Tradeoff analysis
#

tiff("burg_tradeoff.tiff",width=1800,height=800,res=200)
par(mfrow=c(1,3),oma=c(0,0,3,0))
robust = 1-burg_spatstats_random$error_boot
unif = 1-burg_spatstats_random$nn_pass
my_scales = burg_scales

# Criteria 1: du/dr = -1
plot(robust,unif,xlab="Robustness to error",ylab="Internal Uniformity",pch=16)
splinefit = smooth.spline(x=robust,y=unif,df=10)
my_spline = predict(splinefit,x=seq(0,1,0.01))
my_derivs = predict(splinefit,x=seq(0,1,0.01),deriv=1)
opt_i = which.min((my_derivs$y+1)^2)
opt_robust = my_spline$x[opt_i]
opt_uniformity = my_spline$y[opt_i]
lines(my_spline$x,my_spline$y,col="red")
points(opt_robust,opt_uniformity,col="blue",pch=16,cex=2)
title("Balance of gains criterion")
legend("bottomleft", 
       legend = c("Estimates","Optimal"), 
       col = c("black", 
               "blue"), 
       pch = c(20,16), 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 1.0, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.05, 0.08),
       y.intersp=1.5)
legend("bottomleft", 
       legend = c("Fitted spline"), 
       col = c("red"), 
       pch = "-", 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 1.0, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.05, 0.01),
       y.intersp=1.5)

# Criteria 2: unif*robust
plot(my_scales,unif*robust,ylab="Robustness * Uniformity",xlab="Granularity (meters)",xlim=c(100,1000),pch=16)
splinefit = smooth.spline(x=my_scales,y=unif*robust,df=10)
my_spline = predict(splinefit,x=seq(100,1000,5))
lines(my_spline$x,my_spline$y,col="red")
opt_i = which.max(my_spline$y)
opt_granularity = my_spline$x[opt_i]
opt_ur = my_spline$y[opt_i]
points(opt_granularity,opt_ur,col="blue",pch=16,cex=2)
lines(c(-100,5100),c(opt_uniformity*opt_robust,opt_uniformity*opt_robust),col="darkgreen")
title("Product criterion")
legend("bottomleft", 
       legend = c("Estimates","Optimal"), 
       col = c("black", 
               "blue"), 
       pch = list(20,16), 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 1.0, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.05, 0.17),
       y.intersp=1.5)
legend("bottomleft", 
       legend = c("Fitted spline","Balance of gains"), 
       col = c("red","darkgreen"), 
       pch = "-", 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 1.0, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.05, 0.01),
       y.intersp=1.5)

# Criteria 3: unif + robust
plot(my_scales,unif+robust,ylab="Robustness + Uniformity",xlab="Granularity (meters)",xlim=c(100,1000),pch=16)
splinefit = smooth.spline(x=my_scales,y=unif+robust,df=10)
my_spline = predict(splinefit,x=seq(100,1000,5))
lines(my_spline$x,my_spline$y,col="red")
opt_i = which.max(my_spline$y)
opt_granularity2 = my_spline$x[opt_i]
opt_ur = my_spline$y[opt_i]
points(opt_granularity,opt_ur,col="blue",pch=16,cex=2)
lines(c(-100,5100),c(opt_uniformity+opt_robust,opt_uniformity+opt_robust),col="darkgreen")
title("Sum criterion")
title("Tradeoff Analysis: Burglary", outer=TRUE, cex.main=1.75)
legend("bottomleft", 
       legend = c("Estimates","Optimal"), 
       col = c("black", 
               "blue"), 
       pch = list(20,16), 
       bty = "n", 
       pt.cex = 1.5, 
       #cex = 1.0, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.05, 0.17),
       y.intersp=1.5)
legend("bottomleft", 
       legend = c("Fitted spline","Balance of gains"), 
       col = c("red","darkgreen"), 
       pch = "-", 
       bty = "n", 
       pt.cex = 1.5, 
       #cex = 1.0, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.05, 0.01),
       y.intersp=1.5)
dev.off()

# Calculating the mean and sd of the optimal granularity estimated using the different criterias

opt_granularity # Product
opt_granularity2 # Sum
splinefit = smooth.spline(x=my_scales,y=robust,df=10)
my_spline = predict(splinefit,x=seq(100,1000,5))
opt_i = which.min((my_spline$y-opt_robust)^2)
opt_granularity3a = my_spline$x[opt_i]
splinefit = smooth.spline(x=my_scales,y=unif,df=10)
my_spline = predict(splinefit,x=seq(100,1000,5))
opt_i = which.min((my_spline$y-opt_uniformity)^2)
opt_granularity3b = my_spline$x[opt_i]
opt_granularity3a
opt_granularity3b
opt_granularity
opt_granularity2
opt_gran_burg = mean(c(opt_granularity3a,
       opt_granularity3b,
       opt_granularity,
       opt_granularity2))
sd(c(opt_granularity3a,
     opt_granularity3b,
     opt_granularity,
     opt_granularity2))

#
# ======================================================================================================
#



# ===========================================
# Estimating Optimal Granularity for Homicides (essentially the same done with burglaries)
# ===========================================


homic_scales = seq(100,5000,100) # meters
homic_scales_lon = homic_scales
homic_scales_lat = homic_scales

# SHOULD TAKE A FEW MINUTES, BUT NOT TOO LONG
homic_spatstats_random = get_spatialstats_all(homic,homic_scales_lon,homic_scales_lat,random_samples=T,signif=0.99)

# SHOULD TAKE A FEW MINUTES, BUT NOT TOO LONG
homic_spatstats_contig = get_spatialstats_all(homic,homic_scales_lon,homic_scales_lat,random_samples=F,signif=0.99)

#
# Homicides - Uniformity plots
#

tiff("homic_unif_robust.tiff",width=1800,height=800,res=200)
par(mfrow=c(1,2))
plot(NULL,NULL,xlab="Quadrat size (meters)",ylab="Internal Uniformity",xlim=c(100,5000),ylim=c(0,1))
points(homic_scales,1-homic_spatstats_random$nn_pass,col="red",pch=16)
points(homic_scales,1-homic_spatstats_random$quadrat_pass,col="black",pch=16)
points(homic_scales,1-homic_spatstats_contig$nn_pass,col="blue",pch=17)
points(homic_scales,1-homic_spatstats_contig$quadrat_pass,col="green",pch=17)
title("Internal uniformity - Homicides")
legend("bottomleft", 
       legend = c("Nearest-neighbor - random", "Quadrat Count - random",
                  "Nearest-neighbor - contiguous","Quadrat Count - contiguous"), 
       col = c("red", 
               "black",
               "blue",
               "green"), 
       pch = c(16,16,17,17), 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 0.5, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.05, 0.05),
       y.intersp=2)

# 
# Homicides - Robustness to error plots
#

plot(NULL,NULL,xlab="Quadrat size (meters)",ylab="Robustness to error",xlim=c(100,5000),ylim=c(0,1))
points(homic_scales,1-homic_spatstats_random$error_binom,col="black",pch=16)
points(homic_scales,1-homic_spatstats_random$error_pois,col="black",pch=4)
points(homic_scales,1-homic_spatstats_random$error_boot,col="red",pch=16)
points(homic_scales,1-homic_spatstats_contig$error_binom,col="green",pch=16)
points(homic_scales,1-homic_spatstats_contig$error_pois,col="black",pch=1)
points(homic_scales,1-homic_spatstats_contig$error_boot,col="blue",pch=16)
title("Robustness to error - Homicides")
legend("bottomright", 
       legend = c("Binomial - random", "Poisson - random", "Resampled - random",
                  "Binomial - contiguous", "Poisson - contiguous", "Resampled - contiguous"), 
       col = c("black", 
               "black",
               "red",
               "green",
               "black",
               "blue"), 
       pch = c(16,4,16,16,1,16), 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 0.5, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.15, 0.01),
       y.intersp=2)

dev.off()

# 
# Homicides - Tradeoff Analysis
#

tiff("homic_tradeoff.tiff",width=1800,height=800,res=200)
par(mfrow=c(1,3),oma=c(0,0,3,0))
robust = 1-homic_spatstats_random$error_boot
unif = 1-homic_spatstats_random$nn_pass
my_scales = homic_scales

# Criteria 1: du/dr = -1
plot(robust,unif,xlab="Robustness to error",ylab="Internal Uniformity",pch=16)
splinefit = smooth.spline(x=robust,y=unif,df=10)
my_spline = predict(splinefit,x=seq(0,1,0.01))
my_derivs = predict(splinefit,x=seq(0,1,0.01),deriv=1)
opt_i = which.min((my_derivs$y+1)^2)
opt_robust = my_spline$x[opt_i]
opt_uniformity = my_spline$y[opt_i]
lines(my_spline$x,my_spline$y,col="red")
points(opt_robust,opt_uniformity,col="blue",pch=16,cex=2)
title("Balance of gains criterion")
legend("bottomleft", 
       legend = c("Estimates","Optimal"), 
       col = c("black", 
               "blue"), 
       pch = c(20,16), 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 1.0, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.05, 0.08),
       y.intersp=1.5)
legend("bottomleft", 
       legend = c("Fitted spline"), 
       col = c("red"), 
       pch = "-", 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 1.0, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.05, 0.01),
       y.intersp=1.5)

# Criteria 2: unif*robust
plot(my_scales,unif*robust,ylab="Robustness * Uniformity",xlab="Granularity (meters)",pch=16)
splinefit = smooth.spline(x=my_scales,y=unif*robust,df=10)
my_spline = predict(splinefit,x=seq(100,5000,100))
lines(my_spline$x,my_spline$y,col="red")
opt_i = which.max(my_spline$y)
opt_granularity = my_spline$x[opt_i]
opt_ur = my_spline$y[opt_i]
points(opt_granularity,opt_ur,col="blue",pch=16,cex=2)
lines(c(-100,5100),c(opt_uniformity*opt_robust,opt_uniformity*opt_robust),col="darkgreen")
title("Product criterion")
legend("bottomleft", 
       legend = c("Estimates","Optimal"), 
       col = c("black", 
               "blue"), 
       pch = list(20,16), 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 1.0, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.15, 0.17),
       y.intersp=1.5)
legend("bottomleft", 
       legend = c("Fitted spline","Balance of gains"), 
       col = c("red","darkgreen"), 
       pch = "-", 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 1.0, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.15, 0.01),
       y.intersp=1.5)

# Criteria 3: unif + robust
plot(my_scales,unif+robust,ylab="Robustness + Uniformity",xlab="Granularity (meters)",pch=16)
splinefit = smooth.spline(x=my_scales,y=unif+robust,df=10)
my_spline = predict(splinefit,x=seq(100,5000,100))
lines(my_spline$x,my_spline$y,col="red")
opt_i = which.max(my_spline$y)
opt_granularity2 = my_spline$x[opt_i]
opt_ur = my_spline$y[opt_i]
points(opt_granularity,opt_ur,col="blue",pch=16,cex=2)
lines(c(-100,5100),c(opt_uniformity+opt_robust,opt_uniformity+opt_robust),col="darkgreen")
title("Sum criterion")
title("Tradeoff Analysis: Homicide", outer=TRUE, cex.main=1.75)
legend("bottomleft", 
       legend = c("Estimates","Optimal"), 
       col = c("black", 
               "blue"), 
       pch = list(20,16), 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 1.0, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.09, 0.17),
       y.intersp=1.5)
legend("bottomleft", 
       legend = c("Fitted spline","Balance of gains"), 
       col = c("red","darkgreen"), 
       pch = "-", 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 1.0, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.09, 0.01),
       y.intersp=1.5)
dev.off()

opt_granularity # Product
opt_granularity2 # Sum
splinefit = smooth.spline(x=my_scales,y=robust,df=10)
my_spline = predict(splinefit,x=seq(100,5000,100))
opt_i = which.min((my_spline$y-opt_robust)^2)
opt_granularity3a = my_spline$x[opt_i]
splinefit = smooth.spline(x=my_scales,y=unif,df=10)
my_spline = predict(splinefit,x=seq(100,5000,100))
opt_i = which.min((my_spline$y-opt_uniformity)^2)
opt_granularity3b = my_spline$x[opt_i]
opt_granularity3a
opt_granularity3b
opt_granularity
opt_granularity2
opt_gran_homic = mean(c(opt_granularity3a,
       opt_granularity3b,
       opt_granularity,
       opt_granularity2))
sd(c(opt_granularity3a,
     opt_granularity3b,
     opt_granularity,
     opt_granularity2))


#
# ======================================================================================================
#


# ===========================================
# Estimating Optimal Granularity for Robbery (essentially the same done with burglaries)
# ===========================================

robbery_scales = seq(100,5000,100) # meters
robbery_scales_lon = robbery_scales
robbery_scales_lat = robbery_scales

# SHOULD TAKE A FEW MINUTES, BUT NOT TOO LONG
robbery_spatstats_random = get_spatialstats_all(robbery,robbery_scales_lon,robbery_scales_lat,random_samples=T,signif=0.99)

# SHOULD TAKE A FEW MINUTES, BUT NOT TOO LONG
robbery_spatstats_contig = get_spatialstats_all(robbery,robbery_scales_lon,robbery_scales_lat,random_samples=F,signif=0.99)

# 
# Robbery - Uniformity plots
#

tiff("robb_unif_robust.tiff",width=1800,height=800,res=200)
par(mfrow=c(1,2))

plot(NULL,NULL,xlab="Quadrat size (meters)",ylab="Internal Uniformity",xlim=c(100,5000),ylim=c(0,1))
points(robbery_scales,1-robbery_spatstats_random$nn_pass,col="red",pch=16)
points(robbery_scales,1-robbery_spatstats_random$quadrat_pass,col="black",pch=16)
points(robbery_scales,1-robbery_spatstats_contig$nn_pass,col="blue",pch=17)
points(robbery_scales,1-robbery_spatstats_contig$quadrat_pass,col="green",pch=17)
title("Internal uniformity - Robbery")
legend("topright", 
       legend = c("Nearest-neighbor - random", "Quadrat Count - random",
                  "Nearest-neighbor - contiguous","Quadrat Count - contiguous"), 
       col = c("red", 
               "black",
               "blue",
               "green"), 
       pch = c(16,16,17,17), 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 0.5, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.15, 0.05),
       y.intersp=2)

#
# Robbery - Robustness to error plots
#

plot(NULL,NULL,xlab="Quadrat size (meters)",ylab="Robustness to error",xlim=c(100,5000),ylim=c(0,1))
points(robbery_scales,1-robbery_spatstats_random$error_binom,col="black",pch=16)
points(robbery_scales,1-robbery_spatstats_random$error_pois,col="black",pch=4)
points(robbery_scales,1-robbery_spatstats_random$error_boot,col="red",pch=16)
points(robbery_scales,1-robbery_spatstats_contig$error_binom,col="green",pch=16)
points(robbery_scales,1-robbery_spatstats_contig$error_pois,col="black",pch=1)
points(robbery_scales,1-robbery_spatstats_contig$error_boot,col="blue",pch=16)
title("Robustness to error - Robbery")
legend("bottomright", 
       legend = c("Binomial - random", "Poisson - random", "Resampled - random",
                  "Binomial - contiguous", "Poisson - contiguous", "Resampled - contiguous"), 
       col = c("black", 
               "black",
               "red",
               "green",
               "black",
               "blue"), 
       pch = c(16,4,16,16,1,16), 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 0.5, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.15, 0.01),
       y.intersp=2)
dev.off()

#
# Robbery - Tradeoff analysis
#

tiff("robb_tradeoff.tiff",width=1800,height=800,res=200)
par(mfrow=c(1,3),oma=c(0,0,3,0))
robust = 1-robbery_spatstats_random$error_boot
unif = 1-robbery_spatstats_random$nn_pass
my_scales = robbery_scales

# Criteria 1: du/dr = -1
plot(robust,unif,xlab="Robustness to error",ylab="Internal Uniformity",pch=16)
splinefit = smooth.spline(x=robust,y=unif,df=10)
my_spline = predict(splinefit,x=seq(0,1,0.01))
my_derivs = predict(splinefit,x=seq(0,1,0.01),deriv=1)
opt_i = which.min((my_derivs$y+1)^2)
opt_robust = my_spline$x[opt_i]
opt_uniformity = my_spline$y[opt_i]
lines(my_spline$x,my_spline$y,col="red")
points(opt_robust,opt_uniformity,col="blue",pch=16,cex=2)
title("Balance of gains criterion")
legend("bottomleft", 
       legend = c("Estimates","Optimal"), 
       col = c("black", 
               "blue"), 
       pch = c(20,16), 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 1.0, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.05, 0.08),
       y.intersp=1.5)
legend("bottomleft", 
       legend = c("Fitted spline"), 
       col = c("red"), 
       pch = "-", 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 1.0, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.05, 0.01),
       y.intersp=1.5)

# Criteria 2: unif*robust
plot(my_scales,unif*robust,ylab="Robustness * Uniformity",xlab="Granularity (meters)",pch=16)
splinefit = smooth.spline(x=my_scales,y=unif*robust,df=10)
my_spline = predict(splinefit,x=seq(100,5000,100))
lines(my_spline$x,my_spline$y,col="red")
opt_i = which.max(my_spline$y)
opt_granularity = my_spline$x[opt_i]
opt_ur = my_spline$y[opt_i]
points(opt_granularity,opt_ur,col="blue",pch=16,cex=2)
lines(c(-100,5100),c(opt_uniformity*opt_robust,opt_uniformity*opt_robust),col="darkgreen")
title("Product criterion")
legend("bottomleft", 
       legend = c("Estimates","Optimal"), 
       col = c("black", 
               "blue"), 
       pch = list(20,16), 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 1.0, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.05, 0.17),
       y.intersp=1.5)
legend("bottomleft", 
       legend = c("Fitted spline","Balance of gains"), 
       col = c("red","darkgreen"), 
       pch = "-", 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 1.0, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.05, 0.01),
       y.intersp=1.5)

# Criteria 3: unif + robust
plot(my_scales,unif+robust,ylab="Robustness + Uniformity",xlab="Granularity (meters)",pch=16)
splinefit = smooth.spline(x=my_scales,y=unif+robust,df=10)
my_spline = predict(splinefit,x=seq(100,5000,100))
lines(my_spline$x,my_spline$y,col="red")
opt_i = which.max(my_spline$y)
opt_granularity2 = my_spline$x[opt_i]
opt_ur = my_spline$y[opt_i]
points(opt_granularity2,opt_ur,col="blue",pch=16,cex=2)
lines(c(-100,5100),c(opt_uniformity+opt_robust,opt_uniformity+opt_robust),col="darkgreen")
title("Sum criterion")
title("Tradeoff Analysis: Robbery", outer=TRUE, cex.main=1.75)
legend("bottomleft", 
       legend = c("Estimates","Optimal"), 
       col = c("black", 
               "blue"), 
       pch = list(20,16), 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 1.0, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.05, 0.17),
       y.intersp=1.5)
legend("bottomleft", 
       legend = c("Fitted spline","Balance of gains"), 
       col = c("red","darkgreen"), 
       pch = "-", 
       bty = "n", 
       pt.cex = 1.5, 
       cex = 1.0, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.05, 0.01),
       y.intersp=1.5)
dev.off()

opt_granularity # Product
opt_granularity2 # Sum
splinefit = smooth.spline(x=my_scales,y=robust,df=10)
my_spline = predict(splinefit,x=seq(100,5000,100))
opt_i = which.min((my_spline$y-opt_robust)^2)
opt_granularity3a = my_spline$x[opt_i]
splinefit = smooth.spline(x=my_scales,y=unif)
my_spline = predict(splinefit,x=seq(100,5000,100),df=10)
opt_i = which.min((my_spline$y-opt_uniformity)^2)
opt_granularity3b = my_spline$x[opt_i]
opt_granularity3a
opt_granularity3b
opt_granularity
opt_granularity2
opt_gran_robbery = mean(c(opt_granularity3a,
                          opt_granularity3b,
                          opt_granularity,
                          opt_granularity2))
sd(c(opt_granularity3a,
     opt_granularity3b,
     opt_granularity,
     opt_granularity2))