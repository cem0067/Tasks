setwd("~/Desktop/Evolution/Tasks")
library(phytools)
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
text.string <- AnolisTree
read.tree(AnolisTree, text= "text.string")
tree <- read.tree(AnolisTree, text= "text.string")
tree2 <- read.tree(text="(((A,B),(C,D)),E);")
plot(tree, type="fan")
data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors = F, row.names = 1)
svl <- setNames(data$svl, rownames(data))
Ancestors <- fastAnc(tree, svl, vars=TRUE, CI= TRUE)
par(mar=c(0.1, 0.1, 0.1, 0.1))
plot(tree, type= "fan", lwd=2, show.tip.label=F)
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])
nodelabels(pch=16, cex=0.25*Ancestors$ace)
obj <- contMap(tree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))
fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c("Anolis_aliniger", "Anolis_aliniger", "Anolis_occultus", "Anolis_ricordii", "Anolis_cristatellus", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_angusticeps", "Anolis_angusticeps"))
fossilNodes <- c(25.4, 23.2, 17.7, 19.7, 24, 31)
nodeN <- c("Anolis_cybotes", "Anolis_angusticeps")
Node <- fastMRCA(tree, fossilData[i, "tip1"], fossilData[i, "tip2"])
fossilNodes[i] <- fossilData[i, "svl"]
nodeN[i] <- Node
names(fossilNodes) <- nodeN
Ancestors_withFossils <- fastAnc(tree, svl, anc.states=fossilNodes, CI= TRUE, var = TRUE)
install.packages("geiger")
library(geiger)
?fitContinuous
fitContinuous(data = fossilData, model = c("BM", "OU", "EB", "white"))
