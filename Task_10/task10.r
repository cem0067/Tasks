library(phytools)
trees <- vector(mode = "list", length = 0)
births <- c()
Fractions <- c()
ABL <- c()
for (i in 1:100) {
  births[[i]] <- runif(1, 1, 100)
  Fractions[[i]] <- runif(1, 1, 100)
  trees[[i]] <- pbtree(b = births[i], d = births[i]*Fractions[i], n=100)
  ABL[[i]] <-mean(trees[[i]]$edge.length)
}
plot(trees[[i]])

TipLog <- log(sapply(trees, Ntip))
Birthrate <- births
Deathrate <- births * Fractions
Net <- Birthrate - Deathrate
plot(Net, TipLog, xlab = "net diversification rate", ylab = "number of tips")
cor(Net, TipLog)
# weak positive correlation

ABL <- unlist(ABL)
plot(Birthrate, ABL, xlab="speciation rate", ylab="average branch length")
cor(Birthrate, ABL)
#negative correlation

tips <- sapply(trees, Ntip)
which.max(tips)
Tree <- which.max(tips)
plot(Tree)
rates <- c()
traits <- vector(mode = "list", length = 0)
for (i in 1:100) {
  rates[[i]] <- runif(1, 1, 100)
  fastBM(tree = MaxTree, sig2 = rates[i])
  traits[[i]] <- fastBM
}
Mean <- mean(traits)
cor(Mean, rates)
plot(Mean, rates)
Variance <- var(traits)
cor(Variance, rates)
cor(traits[[1]], traits[[2]])
traitsMat <- cbind(traits[[1]], traits[[4]])