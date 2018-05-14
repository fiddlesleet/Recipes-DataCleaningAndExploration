# install.packages('SciencesPo')
library(SciencesPo)
with(mtcars, Scatterplot(x = wt, y = mpg,
                         main = "Vehicle Weight-Gas Mileage Relationship",
                         xlab = "Vehicle Weight",
                         ylab = "Miles per Gallon",
                         font.family = "serif") )

summary(mtcars$mpg)
data(vote)
