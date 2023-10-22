rm(list = ls())

# Exercise set 2
# Exercise 2.1

data <- c("m", "l", "l", "h", "h", "l", "h", "m", "h", "l")
taskA <- "Categorical (Qualitative) Ordinal"

taskB <- factor(data, levels = c("l", "m", "h"), 
                labels = c("low", "middle", "high"), ordered = TRUE)
print(taskB)

taskC <- table(taskB)
print(taskC)


abs_freq <- table(taskB)
rel_freq <- prop.table(abs_freq)
cum_abs_freq <- cumsum(abs_freq)
cum_rel_freq <- cumsum(rel_freq)

taskD <- data.frame(
  Class = levels(taskB),
  Abs_Freq = as.numeric(abs_freq),
  Rel_Freq = as.numeric(rel_freq),
  Cum_Abs_Freq = cum_abs_freq,
  Cum_Rel_Freq = cum_rel_freq
)
View(taskD)

taskEi <- barplot(taskC, main = "Barplot of data", xlab = "Factor levels",
                  ylab = "Frequency")
taskEii <- pie(taskC, main = "Pie chart of data")


# Exercise 2.2
x <- data.frame(mtcars)
task <- as.data.frame(as.numeric(table(x$cyl)))
colnames(task) <- "Freuency"
rownames(task) <- paste(as.data.frame(table(x$cyl))$Var1, "cylinders")
View(task)


# Exercise 2.3
library(coin)
x <- data.frame(malformations)

taskA <- cbind(xtabs(~malformation + consumption, data = x))
View(taskA)
taskA <- addmargins(taskA) # to add marginal sums

taskBi <- table(x$consumption)
taskBii <- table(x$malformation)

taskC <- taskA["Present", "1-2"] / sum(taskA[ , "1-2"])

# taskD
x$consumption_high <- ifelse(x$consumption %in% c("0", "1-2"), "No", "Yes")
x$consumption_high <- as.factor(x$consumption_high)