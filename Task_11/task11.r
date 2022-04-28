x <- rnorm(100, mean = 5, sd = 4)
?rnorm
y <- (x * 5) + 2 + rnorm(100, min(0), max(0.1))
lm <- lm(formula = y ~ x)
summary(lm)
# the slope is 5.000818 and the intercept is 2.009610
z <- rnorm(1)
for (i in 1:100) {
  y2 <- (x * z) + 2 + rnorm(100, min(0), max(0.1))
  lm2 <- lm(formula = y2 ~ x)
  slope <- lm2[["coefficients"]][["x"]]
  intercept <- lm2[["coefficients"]][["(Intercept)"]]
}
plot(z, slope)
# this reveals that the slope and z values are very similar.

library(dplyr)
library(ggplot2)
doors <- 1:3
sample_doors <- function() { return(sample(doors, size = 1000, replace = TRUE))}
games <- data.frame(prize = sample_doors(), pick = sample_doors())
games$strategy <- factor(ifelse(games$prize == games$pick, 'stay', 'switch'))
monte_show <- function(prize, pick) {
  remaining <- setdiff(doors, c(prize, pick))
  return(ifelse(length(remaining)==1,
                remaining,
                sample(remaining, 1)))
}
games <- games %>%
  rowwise %>%
  mutate(shown = monte_show(prize, pick),
         stay = pick,
         switch = setdiff(doors, c(pick, shown)),
         strategy = factor(ifelse(prize == stay, 'stay', 'switch')))
print(summary(games$strategy) / nrow(games))
qplot(strategy, data = games, fill = strategy, geom = 'bar') + 
  xlab('Winning Strategy') +
  ggtitle('Monty Hall Simulation')


install.packages('meme')
install.packages("imager")
install.packages("magick")
library(meme)
library(imager)
library(magick)
setwd("~/Downloads")
u <- load.image("dclqfek-9db6d694-837a-469b-8a90-b4316af42267.png")
meme(u, "did someone say", "breeding season?")
