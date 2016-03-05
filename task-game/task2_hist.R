# Generate a vector sample from 1 to 10 of 100 whole numbers and generate a histogram of the frequency each number between 1 and 10 occurs.

x <- sample(1:10, 100, replace = TRUE)
hist(x, breaks = 0:10)
