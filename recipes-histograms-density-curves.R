# plotting density distributions with ggplot2

######################################################################################
# WORKFLOW
# 1. Get Data
# 2. Get Conditional Means
# 3. Histogram: single, overlaid, stacked/faceted
# 4. Density Curves
# 5. Compare 2 Density Curves: overlaid, semi-transparent, with conditional mean lines
######################################################################################

library(dplyr)
library(ggplot2)

##########
# GET DATA
##########
set.seed(1234)
d <- data_frame(cond = factor(rep(c("A", "B"), each=200)), 
                rating = c(rnorm(200), rnorm(200, mean=.8)))
# inspect
d

#######################
# GET CONDITIONAL MEANS
#######################

means <- d %>%
  group_by(cond) %>%  
  summarize(mean = mean(rating))

############
# HISTOGRAMS
############

# basic histogram of vector 'rating'. each bin has width 1
ggplot(d, aes(x = rating)) + 
  geom_histogram(binwidth = 1, color = "blue", fill = "red") + 
  geom_vline(aes(xintercept = mean(rating, na.rm = TRUE)),
             color = "yellow", linetype = "dashed", size = 1)

# 2 overlaid histograms
ggplot(d, aes(x = rating, fill = cond)) +
  geom_histogram(binwidth = .5, alpha = .5, position = "identity")

# stacked histograms using facets
ggplot(d, aes(x = rating, fill = cond)) +
  geom_histogram(binwidth = .5, alpha = .5, position = "identity") +
  facet_grid(cond ~.)

# stacked histograms using facets with mean lines
ggplot(d, aes(x = rating, fill = cond)) +
  geom_histogram(binwidth = .5, alpha = .5, position = "identity") + 
  facet_grid(cond ~.) +
  geom_vline(data = means, aes(xintercept = mean, color = cond),
             linetype = "dashed", size = 1)


##########################
# DENSITY CURVES
##########################

# basic density curve
ggplot(d, aes(x = rating)) + 
  geom_density()

# histogram with kernel density curve and mean line
ggplot(d, aes(x = rating)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = .5,
                 color = "black",
                 fill = "white") + # histogram with denisty instead of count
  geom_density(alpha = .2, fill = "#FF6666") +
  geom_vline(aes(xintercept = mean(rating, na.rm = TRUE)),
             color = "red", linetype = "dashed", size = 1)


##########################
# COMPARE 2 DENSITY CURVES
##########################

# 2 overlaid density plots
  summarize(mean = mean(rating))
ggplot(d, aes(x = rating, color = cond)) +
  geom_density()

# 2 overlaid density plots with semi-transparent fill
ggplot(d, aes(x = rating, fill = cond)) +
  geom_density(alpha = .3)
  
# with conditional mean lines:
ggplot(d, aes(x = rating, fill = cond)) +
  geom_density(alpha = .3) +
  geom_vline(data = means, aes(xintercept = mean, color = cond),
             linetype = "dashed", size = 1)
