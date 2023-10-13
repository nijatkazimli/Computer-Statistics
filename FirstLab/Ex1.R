rm(list = ls()) # removes all environment variables

# Exercise set 1
# Exercise 1.1
a <- c(1L, 3L ,4L)
b <- seq(1, 100)
c <- seq(1, 99, by = 2)
d <- seq(100, 1)
e <- seq(100, -5)


# Exercise 1.2a
i1 <- c(5, 5, 6);   typeof(i1)  # double
i2 <- seq(1, 50);   typeof(i2)  # integer
i3 <- seq(50, -5);  typeof(i3)  # integer

ii <- c(1L, 5L, 6L); typeof(ii) # integer

iii1 <- c(TRUE, TRUE, FALSE); typeof(i3)  # logical
iii2Helper <- seq(1, 50); iii2 <- c(iii2Helper < 50); typeof(iii2) # logical

iv <- c("quick", "introduction", "to", "R"); typeof(iv) # character

# Exercise 1.2b
myList <- list(
  a <- c(TRUE, TRUE, FALSE),
  b <- list(list(b1 <- c(1, 4, 5), b2 <- c("ab", "cdef"))),
  c <- 4.6
); typeof(myList) # list

print_nested_list <- function(my_list, indent = 0) {
  for (element in my_list) {
    if (is.list(element)) {
      # If the element is a list, print it with increased indentation
      cat(rep("  ", indent), "List:\n")
      print_nested_list(element, indent + 1)
    } else {
      # If the element is not a list, print it
      cat(rep("  ", indent), element, "\n")
    }
  }
}
# the function is for printing the list in a nice way (just cosmetics...)

print_nested_list(myList)


# Exercise 1.3
a <- c("this", "is", "CS", "lab")
b <- c("and", "we", "start", "today")

taskA <- paste(a, collapse = " ")
taskB <- paste(a, collapse = ".")
taskC <- paste(a, b, sep = " ")
taskD <- paste(a, b, sep = "|")
taskE <- paste(a, b, collapse = " ")
taskF <- paste(a, b, sep = "|", collapse = " ")
taskG <- paste(paste(a, collapse = " "), paste(b, collapse = " "))

b <- b[2:length(b)]
taskH <- paste(paste(a, collapse = " "), paste(b, collapse = " "), sep = ", ")

# sep param - takes elements from each vector a, b, and creates a new element 
#                                                           having a separator
# collapse param - the same but creates one string which contains all new 
#                                                                     elements


# Exercise 1.4
x <- c(1, 2, -3.4, 0, 8.1, 1.9, 12, 0, -1)
taskA <- length(x)
taskB1 <- sort(x)       # ascending
taskB2 <- rev(sort(x))  # descending
# The difference between sort() and order() is that sort() returns a new
# collection, keeping original, while order() changes the original collection.
taskC <- sum(x)
taskD1 <- min(x); taskD2 <- max(x)
taskE1 <- cumsum(x); taskE2 <- cumprod(x); taskE3 <- cummin(x)
taskE4 <- cummax(x)
taskF <- taskE1 / seq_along(x) # The seq_along(x) is equivalent to 1:length(x)
taskG1 <- which(x < 0); taskG2 <- x[taskG1]
names(x) <- letters[1:length(x)] # taskH
for (i in taskG1) {
  cat("index: ", i, " number: ", x[i], "\n")
} # taskI
taskJ1 <- replace(x, x == 0, NA); taskJ2 <- sum(x)


# Exercise 1.5
matrix_elements <- c(1, 2, 3, 0, 1, 4, 5, 6, 0)
# M <- matrix(matrix_elements, nrow = 3, ncol = 3)
M <- array(matrix_elements, dim = c(3, 3)) # a more generic way
taskA <- dim(M)
taskB <- t(M)
taskC <- M %*% M # matrix square operator. M^2 squares element-wise
taskD <- sum(M)

# rownames(M) <- c("row1", "row2", "row3")
# colnames(M) <- c("col1", "col2", "col3")
rownames(M) <- paste("row", 1:nrow(M), sep = "") # a more generic way, number
colnames(M) <- paste("col", 1:ncol(M), sep = "") # of cols, rows doesn't matter
print(M) # taskE

taskFi <- M[2, 3]

# vector way
taskFii <- M[2, ]
taskFiii <- M[ , 1]
logical_subset <- M == 2 | M == 3
taskiv <- M[logical_subset]

# matrix way
taskFiiM <- M[2, , drop = FALSE]    # drop param - whether to drop empty cols
taskFiiiM <- M[ , 1, drop = FALSE]  # or rows
taskFiv <- M * logical_subset

taskG <- M; taskG[taskG == 2] <- 12; taskG[taskG == 3] <- 13

taskHSumColumn <- colSums(M)
taskHMeanColumn <- colMeans(M)
taskHMinColumn <- apply(M, 2, min) # apply min to the second dimension of M
taskHMaxColumn <- apply(M, 2, max) # the same with max

taskHSumRow <- rowSums(M)
taskHMeanRow <- rowMeans(M)
taskHMinRow <- apply(M, 1, min) # apply min to the first dimension (row) of M
taskHMaxRow <- apply(M, 1, max) # the same with max

taskI <- matrix(c(apply(M, 1, min), apply(M, 1, max)), ncol = 2)
colnames(taskI) <- c("min", "max")
rownames(taskI) <- paste("row", 1:nrow(taskI))

# Note: cbind(...) is similar to c(...) but it also binds the row, col names


# Exercise 1.6
x <- data.frame(Titanic) # IMPORTANT
taskA <- typeof(x)
taskBi <- class(x); taskBii <- dim(x)
View(x)   # to view the table

taskC <- subset(x, Age == "Adult")
View(taskC)

taskD <- subset(x, Sex == "Male")
View(taskD)

taskE <- subset(x, Age == "Adult" & Sex == "Male")
View(taskE)


# Exercise 1.7
taskAhelper <- matrix(c(1, 5, 15, 2, 6, 36), ncol = 2) 
taskA <- as.data.frame(taskAhelper)
View(taskA)

taskBi <- data.frame(
  name = c("Kowalski", "Nowak"),
  age = c(21, 23),
  student = c(TRUE, FALSE)
)
rownames(taskBi) <- paste("std", 1:nrow(taskBi), sep = "")
View(taskBi)
taskBii <- taskBi["std1", ] # the second argument is the column we want to
View(taskBii)               # extract. Empty means all columns.


# Exercise 1.8
x_values <- seq(2, 4, by = 0.1)
taskAvalues <- exp(x_values) * cos(x_values)
taskA <- data.frame(x = x_values, y = taskAvalues)
View(taskA)

taskB <- sum(seq(2, 10))
taskC <- sum(seq(2, 100, by = 2))
taskD <- log(prod(seq(1, 50)))

taskE <- 0
for (i in 1:10) {
  for (j in 1:20) {
    taskE <- taskE + i*j
  }
}

taskF <- 0
for (i in 1:3) {
  for (j in 1:4) {
    taskF <- taskF + i^j
  }
}

taskGhelper <- 0
for (i in 1:10) {
  for (j in 1:20) {
    taskGhelper <- taskGhelper + i^j
  }
}
taskG <- sqrt(taskGhelper)


# Exercise 1.9
minmax <- function(x, n) {
  if (n > length(x)) {
    return("argument too long")
  }
  
  x_sorted = sort(x)
  min_values <- head(x_sorted, n)
  
  max_values <- rev(tail(x_sorted, n))
  
  return(list(smallest = min_values, largest = max_values))
}

x <- c(5, 3, 8, 1, 9, 2, 4, 7, 6)
n <- 5
result <- minmax(x, n)
print(result)


# Exercise 1.10
taskList <- list(
  x = c(1, 3, 9, 15),
  y = c(5, 3, 19)
)
taskA <- sapply(taskList, sum)

taskBi <- sapply(taskList, mean)
taskBii <- sapply(taskList, sd) # sd(...) stands for standard deviation

# lapply()
# The output is always a list, even if the result of applying the function is 
# a vector or a different data structure.

# sapply()
# similar to lapply(), but it attempts to simplify the result into a vector or
# matrix when possible. If the results are of consistent length and type,
# it will simplify the output.


# Exercise 1.11
curve(x^3 - x^2 + x - 1, from = -2, to = 3)


# Exercise 1.12
x <- data.frame(cars)
View(x)
x$speed = x$speed * 1.61
x$dist = x$dist * 0.3
plot(x$speed, x$dist, xlab = "speed", ylab = "distance", 
     main = "Speed vs Distance", pch = 10, col = "blue"
)
abline(lm(dist ~ speed, data = x), col = "red") # a regression line