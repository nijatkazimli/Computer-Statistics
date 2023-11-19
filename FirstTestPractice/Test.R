# EXERCISE 1
i <- 2:5
sum(i^3 + 2*i^2)
rm(list = ls())

# EXERCISE 2
i <- c(rep(1, 30), rep(5, 20), rep(9, 10))
idx <- seq(58)
sum(i[idx] - 3*i[idx + 1] - 2*i[idx + 2])
rm(list = ls())

# EXERCISE 3
sum(UCBAdmissions["Admitted", ,])
sum(UCBAdmissions["Rejected", ,])
sum(UCBAdmissions["Admitted", ,]) / sum(UCBAdmissions)
sum(UCBAdmissions["Rejected", ,]) / sum(UCBAdmissions)

prop.table(UCBAdmissions[, -1, ], margin = 2)
UCBAdmissions["Rejected", -1, ]

prop.table(UCBAdmissions[, , "A"], margin = 2)

# EXERCISE 4
# a
esoph_data <- data.frame(esoph)
esoph_data$smoker <- ifelse(esoph_data$tobgp == "0-9g/day", "No", "Yes")
esoph_data$smoker <- factor(esoph_data$smoker, levels = c("Yes", "No"))

# b
(classTable <- table(esoph_data$smoker, esoph_data$ncases > 0))

# c
(classTable[3] / classTable[1]) / (classTable[4] / classTable[2])
rm(list = ls())

# EXERCISE 5
# a
dnorm(3, 3, 5)
pnorm(2, 3, 5)

# b
pnorm(2, 3, 5) - dnorm(2, 3, 5)
pnorm(6, 3, 5) - pnorm(2.5, 3, 5)

# c
qnorm(0.9, 3, 5)

# EXERCISE 6
data <- Theoph$conc
par(mfrow = c(2, 1))
hist(data, freq = FALSE, ylim = c(0, 0.2), xlab = NULL)
curve(dnorm(x, mean(data), sd(data)), add = TRUE, col = 2, lwd = 2)
qqnorm(data)
qqline(data, col = 2, lwd = 2)
rm(list = ls())
dev.off()

# EXERCISE 7
sample <- rexp(100, 2)
# a
median(sample)
IQR(sample)

# b
boxplot(sample)

# c
noutliers <- function(vector) {
  Q1 <- quantile(vector, 0.25)
  Q3 <- quantile(vector, 0.75)
  
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  outliers <- vector[vector < lower_bound | vector > upper_bound]
  
  low_extreme <- sum(outliers < lower_bound)
  low_not_extreme <- sum(outliers >= lower_bound & outliers < Q1)
  
  high_extreme <- sum(outliers > upper_bound)
  high_not_extreme <- sum(outliers <= upper_bound & outliers > Q3)
  
  result <- list(
    Low_Extreme = low_extreme,
    Low_Not_Extreme = low_not_extreme,
    High_Extreme = high_extreme,
    High_Not_Extreme = high_not_extreme
  )
  
  return(result)
}
noutliers(sample)