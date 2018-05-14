library(stringr)
library(dplyr)

###############
# SETUP STRINGS
###############

nyc.lower_case <- "new york city"
nyc.upper_case <- "NEW YORK CITY"
nyc.sentence_case <- "New York City"
nyc.myspace_case <- "NeW yOrK CiTY!! <3"

#####################
# CONVERT STRING CASE
#####################

# convert string to upper case
nyc <- str_to_upper(nyc.lower_case)
# inspect
print(nyc)

# convert string to lower case
nyc <- str_to_lower(nyc.upper_case)
# inspect
nyc

# convert string to sentence case
nyc <- str_to_title(nyc.myspace_case)

# These functions handle foreign language characters:
fr <- str_to_upper("é", "Fr") # French
# inspect
fr

