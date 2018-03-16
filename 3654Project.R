election2012=read.csv("pres_polls2012.csv")
#Subsetting to only Virginia and Polling prior to Oct 19th.
Virginia2012=subset(election2012,election2012$State=="Virginia" & election2012$Day < 292)
Demmodel2012=lm(Virginia2012$Dem~Virginia2012$Day, data=Virginia2012)
Repmodel2012=lm(Virginia2012$GOP~Virginia2012$Day, data=Virginia2012)
plot(Virginia2012$Day,Virginia2012$Dem,col="blue",xlab="Number of days",
     ylab="Percentage of Votes",ylim=c(25,55),pch=16, 
     main="Democrat vs. Republican Voting Polls 2012", type="l", lwd=2)
points(Virginia2012$Day,Virginia2012$GOP,col="red",pch=16, type="l", lwd=2)
abline(Demmodel2012, col="blue")
abline(Repmodel2012, col="red")
legend(245,30,legend=c("Democrat","Republican"),col=c("blue","red"),pch=16)

Demmodel2012 #regression equation: y = 0.005426x + 46.943578
Repmodel2012 #regression equation: y = 0.01681x + 41.52351
Virginia2012later=subset(election2012,election2012$State=="Virginia")
Virginia2012later
#Day 310 represents general election day (November 6th)
(0.005426*310) + 46.943578 #Dem prediction (before error)
(0.01681*310) + 41.52351 #Rep prediction (before error)
#Dem > Rep (checks off)

dempredict2012=(0.005426*310) + 46.943578 #Dem prediction (before error)
reppredict2012=(0.01681*310) + 41.52351 #Rep prediction (before error)
finaldem2012=dempredict2012/(dempredict2012+reppredict2012)
finalrep2012=reppredict2012/(dempredict2012+reppredict2012)
finaldem2012
finalrep2012


#------------------------------------------------------------------------------------------------------------------------
election2008=read.csv("pres_polls2008.csv",header=FALSE)
#Subsetting to only Virginia and Polling prior to Oct 19th.
Virginia2008=subset(election2008,election2008$V3=="Virginia"& election2008$V1 < 282)
Demmodel2008=lm(Virginia2008$V5~Virginia2008$V1, data=Virginia2008)
Repmodel2008=lm(Virginia2008$V6~Virginia2008$V1, data=Virginia2008)
plot(Virginia2008$V1,Virginia2008$V5,col="blue",xlab="Number of days",
     ylab="Percentage of Votes",ylim=c(25,55),pch=16, 
     main="Democrat vs. Republican Voting Polls 2008", type="l",lwd="2")
points(Virginia2008$V1,Virginia2008$V6,col="red",pch=16, type="l", lwd="2")
abline(Demmodel2008, col="blue")
abline(Repmodel2008, col="red")
legend(220,34,legend=c("Democrat","Republican"),col=c("blue","red"),pch=16)


Demmodel2008 #regression equation: y = 0.01681x + 43.05018
Repmodel2008 #regression equation: y = -0.008652x + 48.513747
Virginia2008later=subset(election2008,election2008$V3=="Virginia")
Virginia2008later
#Day 308 represents general election day (November 4th)
(0.01681*308) + 43.05018 #Dem prediction (before error)
(-0.008652*308) + 48.513747 #Rep prediction (before error)
#Dem > Rep (checks off)
dempredict2008=(0.01681*308) + 43.05018 #Dem prediction (before error)
reppredict2008=(-0.008652*308) + 48.513747 #Rep prediction (before error)
finaldem2008=dempredict2008/(dempredict2008+reppredict2008)
finalrep2008=reppredict2008/(dempredict2008+reppredict2008)
finaldem2008
finalrep2008


#------------------------------------------------------------------------------------------------------------------------
election2016=read.csv("pres_polls2016.csv")
Virginia2016=subset(election2016,election2016$State=="Virginia")
Demmodel2016=lm(Virginia2016$Dem~Virginia2016$Day, data=Virginia2016)
Repmodel2016=lm(Virginia2016$GOP~Virginia2016$Day, data=Virginia2016)

plot(Virginia2016$Day,Virginia2016$Dem,col="blue",xlab="Number of days",
     ylab="Percentage of Votes",ylim=c(15,55),pch=16, 
     main="Democrat vs. Republican Voting Polls 2016", type="p", lwd="2")
points(Virginia2016$Day,Virginia2016$GOP,col="red",pch=16,type="p", lwd="2")
abline(Demmodel2016, col="blue")
abline(Repmodel2016, col="red")
legend(220,27,legend=c("Democrat","Republican"),col=c("blue","red"),pch=16)

# plot(c(0,310),c(20,60), type="n") # sets the x and y axes scales
# lines(Virginia2016$Day,Virginia2016$Dem,col="blue",xlab="Number of days",
#       ylab="Percentage of Votes",ylim=c(15,55),pch=16, 
#       main="Democrat vs. Republican Voting Polls 2016", type="l", lwd="2")
# lines(Virginia2016$Day,Virginia2016$GOP,col="red",pch=16,type="l", lwd="2")


# data <- read.table(header=TRUE, text='
#  id weight
#                    1     20
#                    2     27
#                    3     24
#                    ')
# 
# # Ways to add a column
# data$size      <- c("small", "large", "medium")
# data[["size"]] <- c("small", "large", "medium")
# data[,"size"]  <- c("small", "large", "medium")
# data$size      <- 0   # Use the same value (0) for all rows

Virginia2016$Res = with(Virginia2016, ifelse(Virginia2016$Dem>=Virginia2016$GOP, "Democratic", "Republic"))


library(ggplot2)
library(ggthemes)
library(extrafont)

charts.data <- read.csv("copper-data-for-tutorial.csv")
p10 <- ggplot() + geom_bar(aes(y = Dem, x = Day, fill = Res), data=Virginia2016,
                           stat="identity")
p10 <- ggplot() + geom_bar(aes(y = percentage, x = year, fill = product), data = charts.data, 
                           stat="identity")
p10

#Day 312 represents general election day (November 8th)
# Dempred2016=(-0.01191*312) + 46.47850 #Dem prediction (before error)
# Reppred2016=(-0.01446*312) + 40.37645 #Rep prediction (before error)
# Dempred2016
# Reppred2016
# Dempred2016/0.8
# Reppred2016/0.8

Dempred2016=(-0.01191*312) + 46.47850 #Dem prediction (before error)
Reppred2016=(-0.01446*312) + 40.37645 #Rep prediction (before error)
Dempred2016/(Dempred2016+Reppred2016)
Reppred2016/(Dempred2016+Reppred2016)
#Hillary Clinton (Democratic Party) is the predicted winner in Virginia.