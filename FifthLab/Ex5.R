# Exercise 5.1
data <- c(4.68, 4.13, 4.8, 4.63, 5.08, 5.79, 6.29, 6.79,
          4.93, 4.25, 5.7, 4.74, 5.88, 6.77, 6.04, 4.95)

# a
par(mfrow = c(2, 1))

hist(data, freq = F)
curve(dnorm(x, mean = mean(data), sd = sd(data)), add = T, lwd = 2)

qqnorm(data)
qqline(data)
dev.off()

# b
sample_mean <- mean(data)
sample_size <- length(data)
known_standard_deviation <- 0.85
alpha <- 0.05
z_score <- qnorm(1 - alpha / 2)
(ci <- c(sample_mean - known_standard_deviation / sqrt(sample_size) * z_score,
         sample_mean + known_standard_deviation / sqrt(sample_size) * z_score))
rm(list = ls())

# c
sample_mean <- mean(data)
sample_size <- length(data)
sample_standard_deviation <- sd(data)
alpha <- 0.05
t_score <- qt(1 - alpha / 2, sample_size - 1)
(ci <- c(sample_mean - sample_standard_deviation / sqrt(sample_size) * t_score,
        sample_mean + sample_standard_deviation / sqrt(sample_size) * t_score))
rm(list = ls())

# d
sample_size <- length(data)
sample_standard_deviation <- sd(data)
alpha <- 0.05
a <- qchisq(1 - alpha / 2, sample_size - 1)
b <- qchisq(alpha / 2, sample_size - 1)
(ci_var <- c((sample_size - 1)^2 / a, (sample_size - 1)^2 / b))
(ci_sd <- sqrt(ci_var))
rm(list = ls())


# Exercise 5.2
sample_size <- 20
sample_standard_deviation <- 2.3
alpha <- 0.1
a <- qchisq(1 - alpha / 2, sample_size - 1)
b <- qchisq(alpha / 2, sample_size - 1)
(ci <- c((sample_size - 1)^2 / a, (sample_size - 1)^2 / b))


# Exercise 5.3
sample_size <- 25
sample_sum <- 1508
sample_square_sum <- 95628
sample_mean <- sample_sum / sample_size

# a
known_variance <- 225
known_sd <- sqrt(225)
alpha <- 0.05
z_score <- qnorm(1 - alpha / 2)
(ci <- c(sample_mean - known_sd / sqrt(sample_size) * z_score,
         sample_mean + known_sd / sqrt(sample_size) * z_score))
rm(list = ls())

# b
# refer to my note for the below formula
sample_variance <- (sample_square_sum - 2*sample_mean*sample_sum + sample_size*sample_mean^2) / (sample_size - 1)
sample_standard_deviation <- sqrt(sample_variance)
alpha <- 0.05
t_score <- qt(1 - alpha / 2, sample_size - 1)
(ci <- c(sample_mean - sample_standard_deviation / sqrt(sample_size) * t_score,
         sample_mean + sample_standard_deviation / sqrt(sample_size) * t_score))
rm(list = ls())

# c
sample_variance <- (sample_square_sum - 2*sample_mean*sample_sum + sample_size*sample_mean^2) / (sample_size - 1)
sample_standard_deviation <- sqrt(sample_variance)
alpha <- 0.10
t_score <- qt(1 - alpha / 2, sample_size - 1)
(ci <- c(sample_mean - sample_standard_deviation / sqrt(sample_size) * t_score,
         sample_mean + sample_standard_deviation / sqrt(sample_size) * t_score))
# it is narrower
rm(list = ls())


# Exercise 5.4
# Set random seed for reproducibility
set.seed(123)

# Parameters
M <- 100  # Number of samples
n <- 50   # Sample size
mu <- 0   # True mean
sigma <- 1 # True standard deviation

# Create empty vectors to store results
ci_mu <- matrix(NA, nrow = M, ncol = 2)
ci_sigma2 <- matrix(NA, nrow = M, ncol = 2)
coverage_mu <- rep(NA, M)
coverage_sigma2 <- rep(NA, M)

# Generate samples and compute confidence intervals
for (i in 1:M) {
  # Generate a sample from N(0, 1)
  sample_data <- rnorm(n, mean = mu, sd = sigma)
  
  # (a) Compute 95% CI for µ, given that σ^2 is unknown
  sample_mean <- mean(sample_data)
  sample_sd <- sd(sample_data)
  t_score <- qt(0.975, df = n - 1)
  ci_mu[i, ] <- c(sample_mean - t_score * (sample_sd / sqrt(n)), sample_mean + t_score * (sample_sd / sqrt(n)))
  coverage_mu[i] <- mu >= ci_mu[i, 1] & mu <= ci_mu[i, 2]
  
  # (b) Compute 95% CI for σ^2, given that µ is unknown
  chi2_lower <- qchisq(0.025, df = n - 1)
  chi2_upper <- qchisq(0.975, df = n - 1)
  ci_sigma2[i, ] <- c(((n - 1) * sample_sd^2) / chi2_upper, ((n - 1) * sample_sd^2) / chi2_lower)
  coverage_sigma2[i] <- sigma^2 >= ci_sigma2[i, 1] & sigma^2 <= ci_sigma2[i, 2]
}

# Compute coverage rates
coverage_rate_mu <- mean(coverage_mu)
coverage_rate_sigma2 <- mean(coverage_sigma2)

# Print coverage rates
cat("Coverage Rate for µ:", coverage_rate_mu, "\n")
cat("Coverage Rate for σ^2:", coverage_rate_sigma2, "\n")

# Create coverage plots
par(mfrow = c(2, 1))
plot(ci_mu[, 1], type = "l", col = "blue", ylim = c(min(ci_mu), max(ci_mu)), ylab = "CI for µ", xlab = "Sample")
lines(ci_mu[, 2], col = "blue")
abline(h = mu, col = "red", lty = 2)

plot(ci_sigma2[, 1], type = "l", col = "green", ylim = c(min(ci_sigma2), max(ci_sigma2)), ylab = "CI for σ^2", xlab = "Sample")
lines(ci_sigma2[, 2], col = "green")
abline(h = sigma^2, col = "red", lty = 2)


# Exercise 5.5
data <- airquality$Wind
sample_mean <- mean(data)
sample_sd <- sd(data)
sample_size <- length(data)
alpha <- 0.05

z_score <- qnorm(1 - alpha / 2)
(ci <- c(sample_mean - sample_sd / sqrt(sample_size) * z_score,
         sample_mean + sample_sd / sqrt(sample_size) * z_score))

bootstrap_samples <- function(data, num_samples) {
  indices <- sample(1:length(data), replace = TRUE, size = length(data) * num_samples)
  matrix(data[indices], nrow = num_samples, byrow = TRUE)
}
bootstrap_data <- bootstrap_samples(data, 1000)
bootstrap_means <- apply(bootstrap_data, 1, mean)
(ci_bootstrap <- quantile(bootstrap_means, c(alpha/2, 1 - alpha/2)))


# Exercise 5.6
alpha <- 0.05
sample_prop <- 115/400
sample_size <- 400
z_score <- qnorm(1 - alpha / 2)
(ci <- c(sample_prop - sqrt(sample_prop*(1-sample_prop)/sample_size)*z_score,
         sample_prop + sqrt(sample_prop*(1-sample_prop)/sample_size)*z_score))