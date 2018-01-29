# instead of creating empty vector with c(), use vector("type", length)

# numeric vector with 5 elts 
v <- vector("numeric", 5)
# inspect
v

# character vector with 3 elts
v <- vector("character", 3)
# inspect
v

## Main use: speed up for loop
# make the vector
n <- 5 # number of times to run the loop
x <- vector("integer", n)

for (i in seq(n)) { # faster than for (i in seq(5))
  x[i] <- i
} 