######################################################
## WORKFLOW
# 1. COMPARE FUNCTION RUNTIMES
# 2. GENERATE DETAILED TIME, STORAGE PROFILE OF SCRIPT
######################################################

########################################
# 1. COMPUTE FUNCTION RUNTIME COMPARISON
########################################

library(microbenchmark)

# Compare the two functions
compare <- microbenchmark(read.csv("movies.csv"), 
                          readRDS("movies.rds"), 
                          times = 10)
compare

# to time how long it takes to calculate the square root of the numbers 
#  from one to ten million, you would write the following:
system.time(sqrt(1:1e7))

######################################################
# 2. GENERATE DETAILED TIME, STORAGE PROFILE OF SCRIPT
######################################################

# Load the profvis package
library(profvis)
# Load the data set
library(ggplot2movies)
data(movies, package = "ggplot2movies") 

# Profile the following code with the profvis function
profvis({ 
  # Load and select data
  movies <- movies[movies$Comedy == 1, ]
  
  # Plot data of interest
  plot(movies$year, movies$rating)
  
  # Loess regression line
  model <- loess(rating ~ year, data = movies)
  j <- order(movies$year)
  
  # Add a fitted line to the plot
  lines(movies$year[j], model$fitted[j], col = "red")
})   ## Remember the closing brackets!     
