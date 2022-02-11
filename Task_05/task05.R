library(learnPopGen)
coalescent.plot(n = 2, ngen = 20, color.order= "alternating")
pdf(file = "coalescent1.pdf", width = 7, height = 7)
dev.off()
pdf(file = "coalescent2.pdf", width = 7, height = 7)
coalescent.plot(n = 20, ngen = 50, color.order = "alternating")
dev.off()
pdf(file = "coalescent3.pdf", width = 7, height = 7)
coalescent.plot(n = 50, ngen = 100, color.order = "alternating")
dev.off()
# plot 1 had 2 individuals to start, plot 2 had 20 individuals, and plot 3 had 50 individuals. This can be changed by changing the 'n =' part of the code.
#it took 1 generation to reach fixation in plot 1, 49 generations to reach the final allele fixation in plot 2, and 78 generations to reach the final fixation in plot 3. It is difficult to take the average of fixation given the different sample sizes. 
#on average, each haploid individual has 2 offspring. the variance is +/-1
#fitness shows that the allele or genotype that is most likely to have successful offspring will pass the gene onto the next generations. meaning one gene is better suitable for the environment. 
#the most recent common ancestor for the focal locus typically is not alive when the allele reaches fixation. the exception to this is plot 1 with just 2 individuals.
install.packages("coala")
install.packages("rehh", dep=T)
install.packages("assertthat", dep=T)
install.packages("RcppArmadillo", dep=T)
install.packages("https://cran.r-project.org/src/contrib/Archive/scrm/scrm_1.7.3-1.tar.gz", repos=NULL, type="source")
install.packages("https://cran.r-project.org/src/contrib/Archive/coala/coala_0.6.0.tar.gz", repos=NULL, type="source")
library(coala)
library(phytools)
model <- coal_model(sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) +
  feat_mutation(10) + 
  feat_recombination(10) + 
  sumstat_trees() + 
  sumstat_nucleotide_div()
stats <- simulate(model, nsim = 1)
Diversity <- stats$pi
Diversity
#The numbers are not all the same. The difference is caused by 
Nloci <- length(stats$trees)
t1 <- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
#the number( of tips doesnt match the number of trees because of the mutations/recombinations within the individuals.
Agel <- max(nodeHeights(t1))
t2 <- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()
Agel2 <- max(nodeHeights(t2))
#The common ancestor is further than the first SNP. 
#They do not match.
par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
compare.chronograms(t1, t2)
t1_1 <- read.tree(text=stats$trees[[1]][1])
t1_2 <- read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1, t1_2)
for (locus in 1:Nloci) {
  ntrees <- length(stats$trees[[locus]])
  for (n in 1:ntrees) {
    if (locus == 1 && n == 1) {
      outPhy <- read.tree(text=stats$trees[[locus]][n])
    }
    else {
      outPhy <- ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
    }
  }
}
par(mfrow=c(1, 1))
densityTree(outPhy)
model2 <- coal_model(sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) +
  feat_mutation(10) + 
  feat_recombination(20) +
  sumstat_trees() +
  sumstat_nucleotide_div()
#I predict that it will take longer for the recombination to occur in the lineage tree due to more possibilities of recombination over time. 
stats2 <- simulate(model2, nsim =1)
Diversity2 <- stats2$pi
Nloci2 <- length(stats2$trees)
t3 <- read.tree(text = stats2$trees[[1]][1])
plot(t3)
axisPhylo()
Agel3 <-max(nodeHeights(t3))
model3 <- coal_model(10, 50) + 
  feat_mutation(par_prior("theta", sample.int(100, 1))) +
  sumstat_nucleotide_div()
stats3 <- simulate(model3, nsim = 40)
mean_pi <- sapply(stats3, function(x) mean(x$pi))
theta <- sapply(stats3, function(x) x$pars[["theta"]])
plot(mean_pi, theta)
abline(lm(mean_pi ~ theta))
