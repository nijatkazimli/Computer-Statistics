rm(list = ls())

# Exercise set 3
# Exercise 3.1
taskA <- dnorm(0, mean = 1, sd = 2)
taskB <- pnorm(5, mean = 1, sd = 2)

# (c) compute P(X > 2.5), P(1 <= X <= 4).
taskCi <- 1 - pnorm(2.5, mean = 1, sd = 2)
taskCii <- pnorm(4, mean = 1, sd = 2) - pnorm(1, mean = 1, sd = 2)

taskD <- qnorm(0.9, mean = 1, sd = 2)
taskDtest <- pnorm(6.1262062621784, mean = 1, sd = 2)

install.packages("mosaic") # enough to run once to install
library(mosaic)
taskEB <- xpnorm(5, mean = 1, sd = 4)
taskECi <- 1 - xpnorm(2.5, mean = 1, sd = 2)
taskECii <- xpnorm(4, mean = 1, sd = 2) - xpnorm(1, mean = 1, sd = 2)
taskED <- xqnorm(0.9, mean = 1, sd = 2)


# Exercise 3.2
means <- c(0, 0, 0, 1, 2, 3)
variances <- c(1, 4, 9, 1, 1, 1)
labels <- c("N(0,1)", "N(0,4)", "N(0,9)", "N(1,1)", "N(2,1)", "N(3,1)")

par(mfrow = c(2, 1)) # 2 row, 1 column plot area

plot(1, type = "n", xlim = c(-10, 10), ylim = c(0, 0.5),
     xlab = "X", ylab = "PDF", main = "PDF")
# type = "n"    plot without any points or lines (empty)
for (i in 1:length(means)){
  curve(dnorm(x, mean = means[i], sd = sqrt(variances[i])),
        col = i, lwd = 2, add = TRUE)
  # lwd - line width
  # col - color
  # add - add to an existing curve
}
legend("topright", legend = labels,
       col = 1:length(means), title = "Distribution", lty = 1)

plot(1, type = "n", xlim = c(-10, 10), ylim = c(0, 1),
     xlab = "X", ylab = "CDF", main = "CDF")
for (i in 1:length(means)) {
  curve(pnorm(x, mean = means[i], sd = sqrt(variances[i])),
        col = i, lwd = 2, add = TRUE)
}
legend("topright", legend = labels, 
       col = 1:length(means), title = "Distribution", lty = 1)

par(mfrow = c(1, 1))


# Exercise 3.3
x <- seq(-5, 5, by = 0.1)
pdf_normal <- dnorm(x, mean = 0, sd = 1)
pdf_t1 <- dt(x, df = 1) # dt - pdf of Student t distribution
pdf_t2 <- dt(x, df = 2)
pdf_t10 <- dt(x, df = 10)
pdf_t100 <- dt(x, df = 100)

plot(x, pdf_normal, col = "red", xlab = "X", ylab = "PDF", main = "PDFs")
points(x, pdf_t1, col = "blue")
points(x, pdf_t2, col = "green")
points(x, pdf_t10, col = "yellow")
points(x, pdf_t100, col = "black")


# Exercise 3.4
data <- data.frame(airquality)
hist(data$Temp, freq = FALSE, main = "Histogram", xlab = "x")
curve(dnorm(x, mean(data$Temp), sd(data$Temp)), add = TRUE, lwd = 2)
# bell shape - normal distribution

qqnorm(data$Temp)
qqline(data$Temp)
# closer to the straight line - normal distribution


# Exercise 3.5
x <- 0:10
plot(dbinom(x, size = 10, prob = 0.3), main = "Bin(10, 0.3)")
plot(dpois(x, lambda = 2), main = "Pois(2)")


# Exercise 3.6
taskA <- dbinom(2, size = 4, prob = 0.5)
taskB <- pbinom(2, size = 4, prob = 0.5)
taskC <- 1 - pbinom(1, size = 4, prob = 0.5)

binomial_pmf <- function(k, n, p) {
  binomial_coeff <- factorial(n) / (factorial(k) * factorial(n - k))
  probability <- binomial_coeff * p^k * (1 - p)^(n - k)
  return(probability)
}
taskAtest <- binomial_pmf(2, 4, 0.5)
taskBtest <- sum(binomial_pmf(0:2, 4, 0.5))
taskCtest <- 1 - sum(binomial_pmf(0:1, 4, 0.5))


# Exercise 3.7
n_samples <- 10000
count <- 0
cumulative_fractions <- numeric(n_samples)
for (i in 1:n_samples) {
  U1 <- runif(1, 0, 1)
  U2 <- runif(1, 0, 1)
  U3 <- runif(1, 0, 1)
  
  S <- U1 + U2 + U3
  
  if (S<1.5) {
    count <- count + 1
  }
  cumulative_fractions[i] <- count / i
}

approximate_prob <- count / n_samples

IrwinHall <- function(x, n) {
  if (x > n | x < 0) {
    return("invalid argument")
  }
  result <- 0
  for (k in 0:floor(x)) {
    term <- (-1)^k * choose(n, k) * (x - k)^n
    result <- result + term
  }
  result <- result / factorial(n)
  return(result)
}

exact_prob <- IrwinHall(1.5, 3)
relative_error <- abs(exact_prob - approximate_prob) / abs(exact_prob)

plot(1:n_samples, cumulative_fractions)
abline(h = exact_prob, lwd = 3)


# Exercise 3.8
set.seed(123)
taskA <- rnorm(10^3, mean = 2, sd = 3)

# i
quantiles_025 <- quantile(taskA, 0.25)
quantiles_075 <- quantile(taskA, 0.75)

theoretical_quantiles_025 <- qnorm(0.25, mean = 2, sd = 3)
theoretical_quantiles_075 <- qnorm(0.75, mean = 2, sd = 3)

# ii
hist(taskA, freq = FALSE, ylim = c(0, 0.15))
curve(dnorm(x, mean = 2, sd = 3), add = TRUE, lwd = 2)

# iii
ecdf_sample <- ecdf(taskA)
plot(ecdf_sample)
curve(pnorm(x, mean = 2, sd = 3), add = TRUE)


# Exercise 3.9
