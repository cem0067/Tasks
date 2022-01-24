setwd('/Users/Courtney/Desktop/Evolution/Tasks/Task_02')
Data <- read.csv('http://jonsmitchell.com/data/beren.csv',
                 stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]
Feeds <- which(Data[,9] == 'bottle')
berenMilk <- Data[Feeds,]
head(berenMilk)
Feeds <- which(Data[,'event'] == 'bottle')
Feeds <- which(Data$event == 'bottle')
dayID <- apply(Data, 1, function(x) paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data$age <- dateID - dateID[which(Data$event == 'birth')]
head(Data)
beren2 <- Data
beren3 <- beren2[order(beren2$age),]
head(beren)
head(beren2)
head(beren3)
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
Question 1: Hypothesis 1 is inapproriate because there is no 'amount' specified in the data provided. The amount described in this hypothesis could be times fed or the ounces of what he was fed. 
            Hypothesis 2 is inappropriate because stating that there is a relationship is not specific enough, meaning that every possibly is accounted for in the data. This gives no explanation for how testing matches with the researcher's' question.
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot( beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab = "who gave the bottle", ylab = "amount of milk consumed (oz)")
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab = "age in days", ylab = "ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height = 4, width = 4)
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab = "age in days", ylab = "ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
Question 2: the data is impossible to interpret because of multiple data points collected per day, not an equal distribution of the data, and the scales on the axes are not labeled well. 
source("http://jonsmitchell.com/code/plotFxn02b.R")
pdf("r02b-cumulativeMilkByTime.pdf", height = 4, width = 4)
dev.off()
Naps <- which(beren3$event == "nap")
beren4 <- beren3[Naps,]
startID <- apply(beren4, 1, function(x) paste(x[5:6], collapse = '-'))
starttimeID <- sapply(startID, as.difftime, format = "%H-%M")
stopID <- apply(beren4, 1, function(x) paste(x[7:8], collapse = '-'))
stoptimeID <- sapply(stopID, as.difftime, format = "%H-%M")
NapLength <- stoptimeID - starttimeID
NapPerDay <- tapply(NapLength, beren4$day, sum)
NapPerDay
plot(as.numeric(names(NapPerDay)), NapPerDay, type="b", pch=16, xlab = "day", ylab = "total time slept (hours)")
cro(starttimeID, NapLength)
cor.test(starttimeID, NapLength)
correlation: there is a negative correlation between the nap start time and duration of the nap. 
