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


# Exercise 6.8
test1 <- c(3, 5, 9, 8, 6, 7)
test2 <- c(2, 6, 7, 6, 5, 4)
size <- length(test1)

mean1 <- mean(test1)
mean2 <- mean(test2)

sd1 <- sd(test1)
sd2 <- sd(test2)

sp_squared <- ((size - 1)*(sd1^2 + sd2^2)) / (2*size - 2)

t_stat <- (mean1 - mean2) / sqrt(sp_squared*2/size)
df <- 2*size - 2

critical_point <- qt(0.95, df)

if (t_stat > critical_point) {
  cat("Reject the null hypothesis. There is enough evidence to suggest that the true mean for Test 1 is higher than Test 2.\n")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to suggest a difference in means.\n")
}

rm(list = ls())

# Exercise 6.9
n1 <- 5
x_sum <- 38.5
x_bar <- x_sum / n1
x_sq_sum <- 297.24

n2 <- 7
y_sum <- 52.4
y_bar <- y_sum / n2
y_sq_sum <- 392.93

var1 <- (x_sq_sum - 2*x_bar*x_sum + n1*x_bar^2) / (n1 - 1)
var2 <- (y_sq_sum - 2*y_bar*y_sum + n2*y_bar^2) / (n2 - 1)

# a----
test_statistic <- var1 / var2

df1 <- n1 - 1
df2 <- n2 - 1

critical_value <- qf(0.95, df1, df2)

if (test_statistic > critical_value) {
  cat("Reject the null hypothesis. There is enough evidence to suggest that the variances are not equal.\n")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to suggest a difference in variances.\n")
}

# b----
s_pooled_sq <- ((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2)

se_diff <- sqrt(s_pooled_sq * (1/n1 + 1/n2))

t_stat <- (x_bar - y_bar) / se_diff

df <- n1 + n2 - 2

critical_value <- qt(0.95, df)

if (t_stat > critical_value) {
  cat("Reject the null hypothesis. There is enough evidence to suggest that the means are not equal.\n")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to suggest a difference in means.\n")
}
rm(list = ls())

# Exercise 6.10
library(boot)
data(channing)

# a----
gender <- channing$sex

p_w <- sum(gender == "Female") / length(gender)
p_m <- sum(gender == "Male") / length(gender)

# Set the expected ratio based on the null hypothesis
expected_ratio <- 4
p_w_expected <- expected_ratio / (1 + expected_ratio)

# Calculate the expected proportions
p_m_expected <- 1 - p_w_expected

# Calculate the standard error of the difference in proportions
se_diff <- sqrt(p_w_expected * p_m_expected * (1/sum(gender == "Female") + 1/sum(gender == "Male")))

# Calculate the Z-test statistic
z_stat <- (p_w - p_w_expected) / se_diff

# Calculate the p-value
p_value <- 2 * pnorm(abs(z_stat), lower.tail = FALSE)

# Set the significance level
alpha <- 0.05

# Check if the p-value is less than the significance level
if (p_value < alpha) {
  cat("Reject the null hypothesis. There is evidence that the ratio of women to men is not 4:1.")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to suggest a difference in the ratio.")
}

# b----
age_men <- channing$entry[channing$sex == "Male"]
age_women <- channing$entry[channing$sex == "Female"]

# Calculate the means
mean_men <- mean(age_men)
mean_women <- mean(age_women)

# Calculate the standard deviations
sd_men <- sd(age_men)
sd_women <- sd(age_women)

# Calculate the sample sizes
n_men <- length(age_men)
n_women <- length(age_women)

# Calculate the standard errors of the means
se_men <- sd_men / sqrt(n_men)
se_women <- sd_women / sqrt(n_women)

# Calculate the pooled standard error
pooled_se <- sqrt((sd_men^2 / n_men) + (sd_women^2 / n_women))

# Calculate the t-statistic
t_stat <- (mean_men - mean_women) / pooled_se

# Calculate the degrees of freedom
df <- n_men + n_women - 2

# Calculate the two-tailed p-value
p_value <- 2 * pt(abs(t_stat), df, lower.tail = FALSE)

# Set the significance level
alpha <- 0.05

# Check if the p-value is less than the significance level
if (p_value < alpha) {
  cat("Reject the null hypothesis. There is evidence that the mean age of entry is different for men and women.")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to suggest a difference in the mean age of entry.")
}
# c----
# Extract age of entry information and gender
age <- channing$entry / 12
gender <- channing$sex

# Create a binary variable indicating whether the entry age is before 80
before_80 <- ifelse(age < 80, 1, 0)

# Create contingency table for men and women
table_gender_before_80 <- table(gender, before_80)

# Extract counts for men and women entering before 80
count_men_before_80 <- table_gender_before_80["Male", 2]
count_women_before_80 <- table_gender_before_80["Female", 2]

# Calculate proportions for men and women entering before 80
prop_men_before_80 <- count_men_before_80 / sum(table_gender_before_80["Male", ])
prop_women_before_80 <- count_women_before_80 / sum(table_gender_before_80["Female", ])

# Calculate the observed difference in proportions
observed_diff <- prop_women_before_80 - prop_men_before_80

# Assuming large enough sample sizes, perform a z-test for proportions
# Calculate standard error of the difference
se_diff <- sqrt((prop_men_before_80 * (1 - prop_men_before_80)) / sum(table_gender_before_80["Male", ]) +
                  (prop_women_before_80 * (1 - prop_women_before_80)) / sum(table_gender_before_80["Female", ]))

# Calculate the z-statistic
z_stat <- observed_diff / se_diff

# Calculate the one-tailed p-value
p_value <- pnorm(z_stat, lower.tail = FALSE)

# Set the significance level
alpha <- 0.05

# Check if the p-value is less than the significance level
if (p_value < alpha) {
  cat("Reject the null hypothesis. There is evidence that the proportion of women entering before 80 is bigger than for men.")
} else {
  cat("Fail to reject the null hypothesis. There is not enough evidence to suggest a difference in proportions.")
}

