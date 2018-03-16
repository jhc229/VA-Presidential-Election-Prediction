election2012=read.csv("pres_polls2012.csv")
#Subsetting to only Virginia and polling prior to Oct 19th.
Virginia2012=subset(election2012,election2012$State=="Virginia" & election2012$Day < 292)
# View(Virginia2012)
length(Virginia2012$Dem)
length(Virginia2012$GOP)
for (i in 1:50) {
  DemInstance = Virginia2012$Dem[i]
  Virginia2012$Dem[i]=(DemInstance/(DemInstance+Virginia2012$GOP[i]))*100
  Virginia2012$GOP[i]=(Virginia2012$GOP[i]/(DemInstance+Virginia2012$GOP[i]))*100
}
# View(Virginia2012)
Demmodel2012=lm(Virginia2012$Dem~Virginia2012$Day, data=Virginia2012)
Repmodel2012=lm(Virginia2012$GOP~Virginia2012$Day, data=Virginia2012)
plot(Virginia2012$Day,Virginia2012$Dem,col="blue",xlab="Number of days",
     ylab="Percentage of Votes",ylim=c(25,55),pch=16, 
     main="Democrat vs. Republican Voting Polls 2012", type="l", lwd=2)
points(Virginia2012$Day,Virginia2012$GOP,col="red",pch=16, type="l", lwd=2)
abline(Demmodel2012, col="blue")
abline(Repmodel2012, col="red")
legend(225,34,legend=c("Democrat","Republican"),col=c("blue","red"),pch=16)
Demmodel2012 #regression equation: y = -0.006342x + 52.969925 
Repmodel2012 #regression equation: y = 0.006342x + 47.030075
Virginia2012later=subset(election2012,election2012$State=="Virginia")
# View(Virginia2012later)
#Day 310 represents general election day (November 6th)
Dempred2012=(-0.006342*310) + 52.969925 #Dem prediction (before error)
Reppred2012=(0.006342*310) + 47.030075 #Rep prediction (before error)
Dempred2012
Reppred2012
#Dem > Rep (checks off)





election2008=read.csv("pres_polls2008.csv",header=FALSE)
#Subsetting to only Virginia and polling prior to Oct 19th.
Virginia2008=subset(election2008,election2008$V3=="Virginia"& election2008$V1 < 282)
# View(Virginia2008)
length(Virginia2008$V5)
length(Virginia2008$V6)
for (i in 1:40) {
  DemInstance = Virginia2008$V5[i]
  Virginia2008$V5[i]=(DemInstance/(DemInstance+Virginia2008$V6[i]))*100
  Virginia2008$V6[i]=(Virginia2008$V6[i]/(DemInstance+Virginia2008$V6[i]))*100
}
# View(Virginia2008)
Demmodel2008=lm(Virginia2008$V5~Virginia2008$V1, data=Virginia2008)
Repmodel2008=lm(Virginia2008$V6~Virginia2008$V1, data=Virginia2008)
plot(Virginia2008$V1,Virginia2008$V5,col="blue",xlab="Number of days",
     ylab="Percentage of Votes",ylim=c(25,60),pch=16, 
     main="Democrat vs. Republican Voting Polls 2008", type="l", lwd=2)
points(Virginia2008$V1,Virginia2008$V6,col="red",pch=16, type="l", lwd=2)
abline(Demmodel2008, col="blue")
abline(Repmodel2008, col="red")
legend(220,37,legend=c("Democrat","Republican"),col=c("blue","red"),pch=16)
Demmodel2008 #regression equation: y = 0.01376x + 47.00869
Repmodel2008 #regression equation: y = -0.01376x + 52.99131
Virginia2008later=subset(election2008,election2008$V3=="Virginia")
View(Virginia2008later)
#Day 308 represents general election day (November 4th)
Dempred2008=(0.01376*308) + 47.00869 #Dem prediction (before error)
Reppred2008=(-0.01376*308) + 52.99131 #Rep prediction (before error)
Dempred2008
Reppred2008
#Dem > Rep (checks off)



election2016=read.csv("pres_polls.csv")
Virginia2016=subset(election2016,election2016$State=="Virginia")
# View(Virginia2016)
length(Virginia2016$Dem)
length(Virginia2016$GOP)
for (i in 1:32) {
  DemInstance = Virginia2016$Dem[i]
  Virginia2016$Dem[i]=(DemInstance/(DemInstance+Virginia2016$GOP[i]))*100
  Virginia2016$GOP[i]=(Virginia2016$GOP[i]/(DemInstance+Virginia2016$GOP[i]))*100
}
# View(Virginia2016)
Demmodel2016=lm(Virginia2016$Dem~Virginia2016$Day, data=Virginia2016)
Repmodel2016=lm(Virginia2016$GOP~Virginia2016$Day, data=Virginia2016)
plot(Virginia2016$Day,Virginia2016$Dem,col="blue",xlab="Number of days",
     ylab="Percentage of Votes",ylim=c(15,65),pch=16, 
     main="Democrat vs. Republican Voting Polls 2016",type="l",lwd=2)
points(Virginia2016$Day,Virginia2016$GOP,col="red",pch=16, type="l", lwd=2)
abline(Demmodel2016, col="blue")
abline(Repmodel2016, col="red")
legend(220,30,legend=c("Democrat","Republican"),col=c("blue","red"),pch=16)
Demmodel2016 #regression equation: y = 0.003221x + 53.467449
Repmodel2016 #regression equation: y = -0.003221x + 46.532551
#Day 312 represents general election day (November 8th)
Dempred2016=(0.003221*312) + 53.467449 #Dem prediction (before error)
Reppred2016=(-0.003221*312) + 46.532551 #Rep prediction (before error)
Dempred2016
Reppred2016
finaldem2016-finalrep2016 #margin of victory
#Hillary Clinton (Democratic Party) is the predicted winner in Virginia.