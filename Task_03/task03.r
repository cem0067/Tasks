trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)
size <- 50
sample1 <- sample(population1, size)
sample2 <- sample(population2, size)
boxplot(sample1, sample2)
#the samples are the same size, but contain different data points. the populations have different distributions
source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")
Alan <- makeBaby(PatGrandma, PatGrandpa)
Brenda <- makeBaby(MatGrandma, MatGrandpa)
Focus <- makeBaby(Brenda, Alan)
# the number should be 0.5
ToMom <- length(grep("mom", Focus)) / length(Focus)
# the number should be around 0.25. The numbers are a little off from my expectation, but I knew it wouldnt be exact.
ToMomMom <- length(grep("grandma_mom", Focus)) / length(Focus)
ToMomDad <- length(grep("grandpa_mom", Focus)) / length(Focus)
ToDadDad <- length(grep("grandpa_da", Focus)) / length(Focus)
ToDadMom <- length(grep("grandma_da", Focus)) / length(Focus)
# Focus is not equally related to the paternal or maternal grandparents. The average relatedness of each grandparent is 0.25.
Sibling_01 <- makeBaby(Brenda, Alan)
#about 50% is expected to be shared. the actual amount shared is ~55%.
ToSib <- length(intersect(Focus, Sibling_01)) / length(Focus)
#Focus should share about 50% of genes with the siblings
ManySiblings <- replicate(1e3, length(intersect(Focus, makeBaby(Brenda, Alan))) / length(Focus))
quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main="", xlab = "proportion shared genes")
#the range of values can be explained because not all siblings share the exact amount of genes with each other due to crossover in cell division.
HWE <- function(p) {
  aa <- p^2
  ab <- 2 * p * (1 - p)
  bb <- (1 - p)^2
  return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)
plot(1, 1, type="n", xlim=c(0,1), ylim=c(0,1), xlab="freq. allele a", ylab="geno. freq")
p <- seq(from = 0, to = 1, by = 0.01)
GenoFreq <- t(sapply(p, HWE))
lines(p, GenoFreq[, "aa"], lwd=2, col="red")
#the frequency of aa individuals increases as a frequency increases. As a decreases, aa individuals decrease in frequency. Time is not shown. Geographic space is not shown.
lines(p, GenoFreq[, "ab"], lwd=2, col="purple")
lines(p, GenoFreq[, "bb"], lwd=2, col="blue")
legend("top", legend=c("aa", "ab", "bb"), col=c("red", "purple", "blue"), lty=1, lwd=2, bty="n")
Pop <- simPop(500)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
#the frequency of the aa genotype matches the expectation.
Pop <- simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50, pch=22, bg="red")
#the distribution of the frequency has changed. smaller populations tend to have more similar characteristics than larger populations. 
library(learnPopGen)
x <- genetic.drift(Ne=200, nrep=5, pause=0.01)
genetic.drift(Ne=100, nrep=5, pause=0.01)
genetic.drift(Ne=1000, nrep=5, pause=0.01)
PopSizes <- 5:50
Samples <- rep(PopSizes, 5)
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
Line <- lm(tExt ~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
Line2 <- lm(tExt~Samples +0)
abline(Line2)
#the line becomes further from the points that are above the line. This means that the line fits the data for the lower sample numbers but it doesnt fit well for the larger samples.
# extra credit
abline(mod <- lm(Line2))
coef(mod)
fit.lm <- lm(mod)
slope <- coef(fit.lm)
# the slope is the same fpr the new line