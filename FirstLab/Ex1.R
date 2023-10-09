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
