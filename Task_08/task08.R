library(phytools)
library(ape)
text.string <- "(((((((cow, pig), whale), (bat,(lemur, human))), (robin, iguana)), coelacanth), (gold_fish , trout)), shark);"
vert.tree <- read.tree(text=text.string)
plot(vert.tree, edge.width = 2)
nodelabels(frame="circle", bg='white', cex=1)
vert.tree
str(vert.tree)
tree <- read.tree(text="(((A,B), (C,D)), E);")
plotTree(tree, offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)
tree$tip.label
tree$edge
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length, col='black', border='white', main="", xlab="edge length for Anolis tree", ylim=c(0,50), xlim=c(0,6))
tipEdges <- which(AnolisTree$edge[,2] <= Ntip(AnolisTree))
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths)[which(Lengths == min(Lengths))]
plot(AnolisTree, cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)
?plot.phylo
plot(AnolisTree, "phylogram", show.tip.label = FALSE)
plot(AnolisTree, "fan", cex=0.50)
plot(AnolisTree, "phylogram", tip.color = 'red', cex=0.25)
?which
AnolisTree$edge.length <- round(AnolisTree$edge.length, 0.5)
edgelabels(AnolisTree$edge.length, cex=0.5)
plot(AnolisTree, "phylogram", cex=0.25, show.node.label = TRUE)
tiplabels(frame='circle', col = 'blue', cex =0.25)
drop.tip(AnolisTree, tip = 82, trim.internal = TRUE, subtree = FALSE, root.edge = 0)
NewAnolisTree <- drop.tip(AnolisTree, tip = 82, trim.internal = TRUE, subtree = FALSE, root.edge = 0)
plot(NewAnolisTree, cex = 0.25)
ltt(AnolisTree)
abline(0, 1, lwd=2, col='red', lty=2)
fit.bd(AnolisTree, b=AnolisTree, d=AnolisTree, rho = 0.2)
AnolisFit <- fit.bd(AnolisTree, b=AnolisTree, d=AnolisTree, rho = 0.2)
str(AnolisFit)