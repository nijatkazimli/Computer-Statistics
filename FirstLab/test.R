rm(list = ls()) # removes all environment variables

myVar <- 3

vector <- c(3 ,5 ,7)
print(mean(vector))

typeof(c(1,5,6)); typeof(mean); typeof("r-programming"); typeof(NULL)

# Ctrl + L to clear the console
# Alt + -  to put <-
# Ctrl + Enter to run a line of the code
# Ctrl + Shift + Enter to run all lines of the code

typeof(4) # double
typeof(4L) # integer

(m <- matrix(c(1, 5, 6, 9), nrow = 2))
apply(m, 2, mean) # apply mean function to the 2nd dimension (col) of matrix m.

str(m)  # str() is an alternative function to display the summary of
        # the output produced, especially when the data set is huge.

s = c("aa", "bb", "cc", "dd", "ee")
s[1] # prints "aa" because indexing starts from 1

c(s > "aa")

pow2 <- function(x) {
  return(x^2) # return is not mandatory as the last line is returned
}

pow2(6)

dir() # current directory content
ls() # environment content (variables, functions, ...)
getwd() # print current working directory path

x <- 1:16 # same as y
y <- seq(1, 16) # same as x
