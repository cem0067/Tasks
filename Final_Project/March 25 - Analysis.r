setwd('~/Desktop/Evolution/Tasks/Final_Project')
read.csv("Data Set Final.csv", stringsAsFactors = F)
data <- read.csv("Data Set Final.csv", stringsAsFactors = F)
plot(data, xlab="Condition Difference", ylab= "Prey to Predator Size Ratio", pch = 16)
test <- cor.test(x = data$Condition.Difference, y = data$PPSR, method = "pearson")
test$estimate
#the pearson test shows the correlation between the two variables, which is 0.203474, meaning there is not a strong correlation between the condition difference and the prey-to predator size ratio.