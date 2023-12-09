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