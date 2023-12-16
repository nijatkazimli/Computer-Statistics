# Exercise 6.2
data <- c(27, 26, 25, 24, 28, 25, 30, 30, 24)
result <- t.test(data, mu = 25, alternative = "greater")
print(result)

rm(list = ls())

# Exercise 6.3
library(boot)
data <- channing$entry
# a----
par(mfrow = c(2, 1))
hist(data, freq = F)
curve(dnorm(x, mean = mean(data), sd = sd(data)), add = T, lwd = 2)

qqnorm(data)
qqline(data, lwd = 2, col = 2)
dev.off()

# b----
alpha <- 0.05
sample_sd <- sd(data)
sample_size <- length(data)
sample_mean <- mean(data)
t_score <- qt(1 - alpha/2, df = sample_size - 1)
(ci <- sample_mean + c(-1, 1) * sample_sd / sqrt(sample_size) * t_score)

# c----
entry_by_years <- data / 12
(result <- t.test(entry_by_years, mu = 75))

rm(list = ls())


# Exercise 6.4
number_of_heads <- 48
total_flips <- 100
observed_proportion <- number_of_heads / total_flips
(result <- prop.test(number_of_heads, total_flips, p = 0.5, conf.level = 0.90))

rm(list = ls())