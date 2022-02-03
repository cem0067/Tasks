results <- read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors = F)
counts <- results[,c("yellow", "red", "green", "blue", "black", "tan")]
backgrounds <- c("White", "Red", "Yellow", "Green", "Blue", "Black")
backgroundCol <- c("white", "#d53e4f", "#fee08b", "#abdda4", "#3288bd", "black")
calcChi(counts[1,])
Chisqs <- apply(counts, 1, calcChi)
par("mar")
par(mar=c(2,2,2,2))
plotChis(counts)
par(mar=c(1,1,1,1))
plotChis(counts)
par(mar=c(0.5,0.5,0.5,0.5))
plotChis(counts)
#When ChiSquare is high, the bars are very uneven. When ChiSquare is low, the bars are much more even or the same. The plotChis() shows that there is less of a difference as the ChiSquare value is closer to zero. 
Avg <- mean(Chisqs)
#given the plots, one should interpret this as a noticeable difference in the data. 
#the average ChiSquare value is much higher than the critical value (11.70).
backgroundAvgs <- tapply(Chisqs, results[,3], mean)
backgroundAvgs
#the background does cause some differennces in the ChiSquare values.
propSig <- length(which(Chisqs > 11.70)) / length(Chisqs)
percSig <- round(100 * propSig)
#the high number doesnt suprise me
#I dont think natural selection was the only cause of this number as freshman biology students could have used other "methods" to reach their results and alter the data they collected. 
par(las = 1, mar = c(4,4,1,1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis=1)
hist(Chisqs, main="", xlab = "chi-squared values", ylab = "frequency")
par(las = 1, mar = c(4,4,1,1), mgp = c(2, 0.5, 0), tck = -0.01, cex.axis=1)
plot(1, 1, xlim=c(0,400), ylim=c(1,8.5), xlab = "", ylab = "", type="n", yaxt="n")
axis(2, at = 1:length(backgrounds), labels = backgrounds)
mtext(side=1, expression(chi^2), cex=1.75, line=2.5)
counter <- 1
for (i in backgrounds) {
  Data <- Chisqs[which(results[,3] ==i)]
  addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
  counter <- counter + 1
}
abline(v = 11.70, lty = 2, lwd = 2, col='black')
#all of the backgrounds have a majority of data to the right of the critical value line, meaning there are meaningful differences.
Simulation <- simDraws(10000)
addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side= 2, at=7, line=0, "simulated")
abline(v = 11.70, lty=2, lwd=2)
Fit <- c(1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation2 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0, 0, 0, 0.25))
Fit <- c(0.1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation3 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation3, Color=rgb(0, 0, 0, 0.25))
Fit <- c(0.5, 0.6, 0.7, 1, 1, 1)
names(Fit) <- 1:6
Simulation4 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation4, Color=rgb(0, 0, 0, 0.25))
Fit <- c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
names(Fit) <- 1:6
Simulation5 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0, 0, 0, 0.25))
Fit <- c(0.1, 0.1, 0.1, 0.1, 0.1, 1)
names(Fit) <- 1:6
Simulation6 <- simDraws(1e4, w = Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0, 0, 0, 0.25))
mtext(side=2, at=8, line=0, "sel.sim")
Simulation7 <- c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0, 0, 1, 0.25))
#the mixture does look like the student generated data.
#artificial selection
#natural selection
#The students are not simulating the natural selection processes very well, meaning it is not matching the simulations.
#comparing the numbers to simulated numbers tells more about the selection process becuase the critical value only tells whether there is a significant difference or not. 
#a mutation could shift the ChiSquare value in either direction depending on how noticeable the mutation is in the environment. a more vibrant color in the envirnment will take away from the colors that arent as vibrant. but a more dull color that can camofluage better will make other colors stand out more than before. 
