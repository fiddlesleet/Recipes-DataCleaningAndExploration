library(GGally) # contains ggcorr function
library(ggcorrplot) # contains ggcorrplot function for a corrgram with p-values
library(ggplot2)
library(readr)
library(RColorBrewer)

################################################################
# WORKFLOW
# 1. GET DATA
# 2: SAMPLE FINAL OUTPUT: CORRGRAMS
# 3: SAMPLE FINAL OUTPUT: CORRGRAMS WITH P-VALUES

# ----------------- HOW THE CORRGRAMS WORK --------------------
# 1. GET CORRGRAM
# 2. CHANGE COLOR SCALE
# 3. CHANGE COLORS
# 4. LABEL THE SQUARES
# 5. USE CIRCLES INSTEAD OF SQUARES
# 6. USE COLORED LABELS INSTEAD OF SHAPES
#    - a text-only corrgram
################################################################

##############
# 1. GET DATA
##############

nba <- read_csv("http://datasets.flowingdata.com/ppg2008.csv")
nba

###################################
# 2. SAMPLE FINAL OUTPUT: CORRGRAMS
###################################

# Make Corrgram using squares
ggcorr(nba[, 2:15], # avoid warning message by plotting only numerical data 
       hjust = 0.75, # move corrgram row labels left
       size = 5, # make corrgram row labels smaller
       color = "grey50", # change corrgram row label color
       nbreaks = 4, # 4 color gradients
       palette = "RdGy", # use RColoRBrewer
       name = expression(rho), # name the scale/key
       legend.size = 12, # size of legend its font 
       label = TRUE, # label each square with its correlation coeff
       label_size = 3, # shrink each square's label
       label_color = "white", # square label color
       label_round = 2, # round corr coeff label to 2 digits 
       label_alpha = TRUE) # vary transparency of label with strength of corr 
# if you have a long row label at bottom left, may get cut off. use:
# layout.exp = n, where n = # of invisible titles to add behind the label to shift
# the plot over to the right 

# Make Corrgram using Text Only
ggcorr(nba[, 2:15], 
       geom = "text",
       hjust = 1, # move corrgram row labels left
       size = 5, # make corrgram row labels smaller
       color = "grey50", # change corrgram row label color
       nbreaks = 5, # 5 color gradients
       palette = "RdYlBu", # use RColoRBrewer
       name = expression(rho), # name the scale/key
       legend.size = 12,
       label = TRUE, 
       label_alpha = 0.5)

# Make Corrgram using Circles
ggcorr(nba[, 2:15], 
       geom = "circle", 
       nbreaks = 5, 
       min_size = 0, # (optional) manually control min. circle size
       max_size = 6) # (optional) manually control max circle size

# Highlight only the correlations >0.5 or < (-0.5)
ggcorr(nba[, 2:15], geom = "blank", label = TRUE, hjust = 0.75) +
  geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

#################################################
# 3: SAMPLE FINAL OUTPUT: CORRGRAMS WITH P-VALUES
#################################################

# Compute a correlation matrix
corr <- round(cor(nba[,2:15]), 2)

# Compute matrix of p-values of all correlation coeffs
p_values <- cor_pmat(nba[, 2:15]) # will throw error if non-numeric cols included

# PLOT 1: Using default p <= .05 as significance level
ggcorrplot(corr, 
           hc.order = TRUE, 
           type = "lower", 
           title = "Correlations and p-values, p <= .05",
           show.legend = TRUE, 
           legend.title = expression(rho),
           # lab = TRUE, # adds corrcoeff label to each square, but makes it messy
           lab_col = "grey50", # row label color
           p.mat = p_values) + # add the p values matrix 
  theme_minimal()

# PLOT 2: Set p <= .10 as significance level
ggcorrplot(corr, 
           hc.order = TRUE, 
           type = "lower", 
           show.legend = TRUE, 
           legend.title = expression(rho),
           # lab = TRUE, # adds corrcoeff label to each square, but makes it messy
           sig.level = 0.10,               
           title = "Correlations and p-values, p <= .10",
           lab_col = "grey50", # row label color
           p.mat = p_values) +
  theme_minimal()

# PLOT 3: here setting p <= .01 as significance level
ggcorrplot(corr, 
           # hc.order = TRUE, 
           type = "lower", 
           show.legend = TRUE, 
           legend.title = expression(rho),
           # lab = TRUE, # adds corrcoeff label to each square, but makes it messy
           sig.level = 0.01,   
           title = "Correlations and p-values, p <= .01",
           pch.col = 'red', # control X color
           lab_col = "grey50", # row label color
           p.mat = p_values) +
  theme_minimal()

# PLOT 4, CIRCLES: here setting p <= .10 as significance level
# - note the circles' diamaeter automatically follows corrcoeff-significance 
ggcorrplot(corr, 
           #hc.order = TRUE, 
           method = "circle",
           type = "lower", 
           show.legend = TRUE, 
           legend.title = expression(rho),
           # lab = TRUE, # adds corrcoeff label to each square, but makes it messy
           sig.level = 0.10,               
           title = "Correlations and p-values, p <= .10",
           lab_col = "grey50", # row label color
           p.mat = p_values) +
  theme_minimal()

# PLOT 5, COLORS: here setting p <= .10 as significance level
ggcorrplot(corr, 
           #hc.order = TRUE, 
           type = "upper", # upper square 
           show.legend = TRUE, 
           legend.title = expression(rho),
           colors = c("red", "orange", "green"), # colors: c(lo, mid, hi)
           # lab = TRUE, # adds corrcoeff label to each square, but makes it messy
           sig.level = 0.10,               
           title = "Correlations and p-values, p <= .10",
           lab_col = "grey50", # row label color
           p.mat = p_values) +
  theme_minimal()

# ------------------------------------------------------------------------------------------------
# ------------------------------------ HOW THE CORRGRAMS WORK ------------------------------------
# ------------------------------------------------------------------------------------------------

#####################
# 1. GET CORRELATIONS
#####################

# get correlations across entire dataset
ggcorr(nba)
# - the warning comes from the first column, names, which are not numeric. remove:
ggcorr(nba[, 1])

# change to spearman or kendall correlation if ordinal data:
# ggcorr(nba[, -1, method = ("pairwise.complete.obs", "kendall"))
# ggcorr(nba[, -1, method = ("pairwise.complete.obs", "spearman"))

########################
# 2. CHANGE COLORS SCALE
########################

# make corrgram with 5 color gradients
ggcorr(nba[, 2:15], nbreaks = 5)

# make corrgram with 10 color gradients, my preference 
ggcorr(nba[, 2:15], nbreaks = 10)

######################################
# 3. CHANGE SCALE NAME, SIZE, POSITION
######################################

# position legend below, narrow width
ggcorr(nba[, 2:15], 
       name = expression(rho), 
       legend.position = "bottom",
       legend.size = 12) 

# position legend at bottom, wide
ggcorr(nba[, 2:15],
       name = expression(rho),
       legend.position = "bottom",
       legend.size = 12) + 
  guides(fill = guide_colorbar(barwidth = 18, title.vjust = 0.75)) # colorbar is used for continuous scales

# position legend at bottom, wide, with slightly bigger title than the size 12 legend labels
ggcorr(nba[, 2:15],
       name = expression(rho),
       legend.position = "bottom",
       legend.size = 12) + 
  guides(fill = guide_colorbar(barwidth = 18, title.vjust = 0.75)) +
  theme(legend.title = element_text(size = 14))

# position legend at left
ggcorr(nba[, 2:15], 
       name = expression(rho), 
       legend.position = "left",
       legend.size = 12) 

##################
# 4. CHANGE COLORS
##################

# Default gradient colors are bright red -> light grey -> bright blue. Change these.
ggcorr(nba[, 2:15],
       low = "steelblue",
       mid = "white",
       high = "darkred",
       name = expression(rho),
       legend.position = "bottom",
       legend.size = 12) + 
  guides(fill = guide_colorbar(barwidth = 18, title.vjust = 0.75)) +
  theme(legend.title = element_text(size = 14))

# Change colors to a ColorBrewer palette 
# - use this only with nbreaks, which should only be used for categorical data
# - you'll want the scale to the right, which looks way better than below
ggcorr(nba[, 2:15],
       nbreaks = 4,
       palette = "RdGy",
       name = expression(rho),
       legend.size = 12) 

######################
# 5. LABEL THE SQUARES
######################

# default corrgram with labels
ggcorr(nba[, 2:15], label = TRUE)

# make corrgram with 5 color gradients & labels
ggcorr(nba[, 2:15], 
       nbreaks = 5,
       label = TRUE)

# With the RColorBrewer colors
ggcorr(nba[, 2:15],
       nbreaks = 4,
       palette = "RdGy",
       name = expression(rho),
       legend.size = 12,
       label = TRUE) 

# CONTROL SIZE AND COLOR OF THE LABELS
ggcorr(nba[, 2:15],
       nbreaks = 4,
       palette = "RdGy",
       name = expression(rho),
       legend.size = 12,
       label = TRUE, 
       label_size = 3,
       label_color = "white")

# CONTROL HOW MANY DIGITS THE LABELS ROUND TO, AND HAVE TRANSPARENCY FOLLOW STRENGTH OF CORR
ggcorr(nba[, 2:15],
       nbreaks = 4,
       palette = "RdGy",
       name = expression(rho),
       legend.size = 12,
       label = TRUE,
       label_size = 3,
       label_color = "white",
       label_round = 2, # round corr to 2 digits
       label_alpha = TRUE)

# MAKE ROW LABELS SMALLER, MOVE THEM LEFT AND CHANGE THEIR COLOR
ggcorr(nba[, 2:15], # avoid warning message by plotting only numerical data 
       hjust = 0.75, # move corrgram row labels left
       size = 5, # make corrgram row labels smaller
       color = "grey50", # change corrgram row label color
       nbreaks = 4, # 4 color gradients
       palette = "RdGy", # use RColoRBrewer
       name = expression(rho), # name the scale/key
       legend.size = 12, # size of legend its font 
       label = TRUE, # label each square with its correlation coeff
       label_size = 3, # shrink each square's label
       label_color = "white", # square label color
       label_round = 2, # round corr coeff label to 2 digits 
       label_alpha = TRUE) # vary transparency of label with strength of corr 
       # if you have a long row label at bottom left, may get cut off. use:
       # layout.exp = n, where n = # of invisible titles to add behind the label to shift
       # the plot over to the right 

###################################
# 6. USE CIRCLES INSTEAD OF SQUARES
###################################

# make corrgram with 5 color gradients
ggcorr(nba[, 2:15], 
       geom = "circle", 
       nbreaks = 5)

# control min & max size of circles
ggcorr(nba[, 2:15], 
       geom = "circle", 
       nbreaks = 5, 
       min_size = 0, 
       max_size = 6)

#########################################
# 7. USE COLORED LABELS INSTEAD OF SHAPES
# - a text-only corrgram
#########################################

ggcorr(nba[, 2:15], 
       geom = "text",
       hjust = 1, # move corrgram row labels left
       size = 5, # make corrgram row labels smaller
       color = "grey50", # change corrgram row label color
       nbreaks = 5, # 5 color gradients
       palette = "RdYlBu", # use RColoRBrewer
       name = expression(rho), # name the scale/key
       legend.size = 12,
       label = TRUE, 
       label_alpha = 0.5)
