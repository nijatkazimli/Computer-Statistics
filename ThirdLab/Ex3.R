rm(list = ls())

# Exercise set 3
# Exercise 3.1
taskA <- dnorm(0, mean = 1, sd = 4)
taskB <- pnorm(5, mean = 1, sd = 4)

# (c) compute P(X > 2.5), P(1 <= X <= 4).
taskCi <- 1 - pnorm(2.5, mean = 1, sd = 4)
taskCii <- pnorm(4, mean = 1, sd = 4) - pnorm(1, mean = 1, sd = 4)

taskD <- qnorm(0.9, mean = 1, sd = 4)
taskDtest <- pnorm(6.1262062621784, mean = 1, sd = 4)

install.packages("mosaic") # enough to run once to install
library(mosaic)
taskEB <- xpnorm(5, mean = 1, sd = 4)
taskECi <- 1 - xpnorm(2.5, mean = 1, sd = 4)
taskECii <- xpnorm(4, mean = 1, sd = 4) - xpnorm(1, mean = 1, sd = 4)
taskED <- xqnorm(0.9, mean = 1, sd = 4)


# Exercise 3.2
# Parameters for the normal distributions
means <- c(0, 0, 0, 1, 2, 3)
variances <- c(1, 4, 9, 1, 1, 1)
labels <- c("N(0,1)", "N(0,4)", "N(0,9)", "N(1,1)", "N(2,1)", "N(3,1)")

# Create the plotting area with two rows
par(mfrow = c(2, 1))

# PDF plot
plot(1, type = "n", xlim = c(-10, 10), ylim = c(0, 0.5), xlab = "X", ylab = "PDF", main = "PDF")
for (i in 1:length(means)){
  curve(dnorm(x, mean = means[i], sd = sqrt(variances[i])), col = i, lwd = 2, add = TRUE)
}
legend("topright", legend = labels, col = 1:length(means), lty = 1, title = "Distribution")

# CDF plot
plot(1, type = "n", xlim = c(-10, 10), ylim = c(0, 1), xlab = "X", ylab = "CDF", main = "CDF")
for (i in 1:length(means)) {
  curve(pnorm(x, mean = means[i], sd = sqrt(variances[i])), col = i, lwd = 2, add = TRUE)
}
legend("topright", legend = labels, col = 1:length(means), lty = 1, title = "Distribution")

# Reset the plotting area to default
par(mfrow = c(1, 1))



