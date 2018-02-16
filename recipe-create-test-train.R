# elegant workflow for splitting dataset into training and testing data for machine learning

# get data
data("mtcars")

# split into train & test sets, with 75/25 split
splitIndex <- sample(nrow(mtcars), floor(.75 * nrow(mtcars)))
train <- mtcars[splitIndex,]
test <- mtcars[-splitIndex,]

# split into train & test sets, with 90/10 split
splitIndex <- sample(nrow(mtcars), floor(.90 * nrow(mtcars)))
train <- mtcars[splitIndex,]
test <- mtcars[-splitIndex,]
