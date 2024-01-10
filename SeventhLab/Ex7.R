# Exercise 7.1
income <- c(1.5, 10, 8, 3, 5, 15, 2)    * 10^3
clients <- c(3, 6 , 5, 3.5, 4, 8, 3.2)  * 10^2

# a----
plot(clients, income)

# built-in
cor_coef_built_in <- cor(income, clients)

# manual
n <- length(income)
sum_xy <- sum(clients * income)
sum_x <- sum(clients)
sum_y <- sum(income)
sum_x_squared <- sum(clients^2)
sum_y_squared <- sum(income^2)

cor_coef_manual <- (n * sum_xy - sum_x * sum_y) / 
  sqrt((n * sum_x_squared - sum_x^2) * (n * sum_y_squared - sum_y^2))

dev.off()

# b----
#   Y = β_0 + β_1 * X + ε
#
#   Y is the dependent variable (income).
#   X is the independent variable (number of clients).
#   β_0 is the y-intercept (constant term).
#   β_1 is the slope coefficient for the number of clients.
#
#   ε is the error term, representing the unobserved factors that 
#   influence the dependent variable but are not included in the model.

# The main assumptions of the linear regression model regarding the error term
# (ε) are:
  
# Linearity: The relationship between the dependent variable and the independent
# variable is assumed to be linear. This means that a change in the independent
# variable leads to a constant change in the dependent variable.

# Independence: The error terms should be independent of each other.
# The occurrence of one error should not predict the occurrence of another.

# Homoscedasticity: The variance of the error terms should be constant across
# all levels of the independent variable. In other words, the spread of
# residuals should be consistent.

# Normality: The error terms are assumed to be normally distributed. This
# assumption is important for hypothesis testing and constructing confidence
# intervals.

# No Perfect Multicollinearity: The independent variables should not be
# perfectly correlated with each other.
# c----
mean_x <- mean(clients)
mean_y <- mean(income)

# Calculate slope (beta_1)
beta_1_manual <- sum((clients - mean_x) * (income - mean_y)) / sum((clients - mean_x)^2)

# Calculate intercept (beta_0)
beta_0_manual <- mean_y - beta_1_manual * mean_x

# Calculate residuals
residuals_manual <- income - (beta_0_manual + beta_1_manual * clients)

# Calculate residual sum of squares (RSS)
rss_manual <- sum(residuals_manual^2)

# Calculate variance of residuals (sigma^2)
sigma_squared_manual <- rss_manual / (length(income) - 2)


# Using lm() function

# Create a data frame
data <- data.frame(income = income, clients = clients)

# Fit linear regression model using lm()
model <- lm(income ~ clients, data = data)

# Get OLS estimates using lm()
beta_0_lm <- coef(model)[1]
beta_1_lm <- coef(model)[2]

# Get R-squared value
rsquared <- summary(model)$r.squared

# d----
fitted_values_manual <- beta_0_manual + beta_1_manual * clients
fitted_values_lm <- predict(model)

plot(clients, income)
points(clients, fitted_values_lm, col = 2)
abline(model, col = 3)
legend("topleft", 
       legend = c("Actual Values", "Fitted Values", "OLS Fitted Line"),
       col = c(1, 2, 3), lty = 1)
dev.off()

# e----
alpha <- 0.05
p_value_beta_0 <- summary(model)$coefficients[1, 4]
is_beta_0_significant <- p_value_beta_0 < alpha

p_value_beta_1 <- summary(model)$coefficients[2, 4]
is_beta_1_significant <- p_value_beta_1 < alpha

# f----
residuals <- residuals(model)
par(mfrow = c(1, 2))
plot(residuals)
qqnorm(residuals)
qqline(residuals)
dev.off()

# g----
new_data <- data.frame(clients = 1000)
predicted_income <- predict(model, newdata = new_data)


rm(list = ls())


# Exercise 7.2
n <- 1000
beta <- c(3, 5, 15)
set.seed(10)
x1 <- rchisq(n, 3)
x2 <- rchisq(n, 9) - 50
err <- rnorm(n, 0, 5)
y <- cbind(1, x1, x2) %*% beta + err
data <- data.frame(y, x1, x2)


model <- lm(y ~ x1 + x2, data = data)
summary_results <- summary(model)
alpha <- 0.05

# Test for Beta_0 (intercept) and other coefficients
p_values <- summary_results$coefficients[, 4]
significant_coeffs <- p_values < alpha


par(mfrow = c(1, 2))
plot(model, which = 1, main = "Residuals Plot")

qqnorm(model$residuals, main = "QQ Plot for Residuals")
qqline(model$residuals)

dev.off()
rm(list = ls())

# Exercise 7.3
install.packages('carData')
library(carData)

model_original <- lm(salary ~ yrs.since.phd + discipline, data = Salaries)
(summary_results_original <- summary(model_original))

alpha <- 0.05
p_values_original <- summary_results_original$coefficients[, 4]
significant_coeffs_original <- p_values_original < alpha

par(mfrow = c(1, 2))
plot(model_original, which = 1)

qqnorm(model_original$residuals)
qqline(model_original$residuals)

# Transform variables
Salaries$log_salary <- log(Salaries$salary)
Salaries$yrs.since.phd_squared <- Salaries$yrs.since.phd^2

model_transformed <- lm(log_salary ~ yrs.since.phd + yrs.since.phd_squared + discipline, data = Salaries)
(summary_results_transformed <- summary(model_transformed))

p_values_transformed <- summary_results_transformed$coefficients[, 4]
significant_coeffs_transformed <- p_values_transformed < alpha

par(mfrow = c(1, 2))
plot(model_transformed, which = 1)
qqnorm(model_transformed$residuals)
qqline(model_transformed$residuals)

dev.off()
rm(list = ls())