library(choroplethr)
library(mapproj)
library(dplyr)

###########
# LOAD DATA
###########

data("df_county_demographics")
# inspect
str(df_county_demographics)
head(df_county_demographics)

######################################
# PLOT MAPS OF MULTIPLE STATES AT ONCE
######################################

map_multiple_states <- function(state, abrev) {
  county_choropleth(df_county_demographics, 
                    state_zoom = state,
                    title = paste(abrev, "County Per Capita Income\n2012 Estimates", sep = " "),
                    num_colors = 9) +
    coord_map()
}

## LOAD DESIRED DEMOGRAPHIC DATA INTO VALUE FIELD
#  - here, plot median income in selected states
df_county_demographics$value = df_county_demographics$per_capita_income

# choose states to analyze 
states <- c('california', 'washington', 'pennsylvania', 'montana')
abrevs <- c('CA', "WA", 'PA', 'MT')

# plot 
mapply(map_multiple_states, states, abrevs, SIMPLIFY = FALSE)
